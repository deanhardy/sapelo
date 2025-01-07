###############################################################################################
## PURPOSE: geocoding tax assessor records & plotting info by locality and ownership
## BY: Dean Hardy
###############################################################################################
rm(list=ls())
## considerations: differences among heirs or corporate properties? mean distance for ownership categories?

library(tidyverse)
library(sf)
library(tidygeocoder)
library(fuzzyjoin)

utm <- 2150 ## NAD83 17N

## define data directory
datadir <- '/Users/dhardy/Dropbox/r_data/sapelo/'

## import property owner data
o <- read.csv(file.path(datadir, 'property/owners_sapelo_SydRA.csv'), stringsAsFactors = F) %>%
  mutate(own3cat = ifelse(own_cat %in% c('LLC', 'LLP', 'INC', 'Non-descendant'), 'Non-descendant', own_cat),
         own4cat = ifelse(own_cat %in% c('LLC', 'LLP', 'INC'), 'Company', own_cat),
         zip = ifelse(str_length(zip) == 4, paste0("0", zip), zip)) %>%
  select(-c(X:X.13)) %>% 
  na.omit() %>%
  mutate(parcel_id = str_remove_all(parcel_id, pattern = ' ')) 

## import tax records, merge address-related columns, filter SIHA and local addresses, clean up headers
tax <- read.csv(paste0(datadir, 'property/taxes/tax_records_99-23.csv'), stringsAsFactors = F) %>%
  mutate(ZIP.Code = as.character(ZIP.Code),
         zip = if_else(str_length(ZIP.Code) == 4, paste0("0", ZIP.Code), ZIP.Code)) %>%
  # select(-c(X:X.13)) %>% 
  mutate(address = paste(Property.Address, City, State, zip)) %>%
  filter(!str_detect(Deed.Name, 'HERITAGE'), 
         address != '   NA'
         ) %>%
  rename(state = State, year = Year, bill = Bill.., name = Deed.Name, parcel.id = Map.Code, due.date = Due.Date, paid.date = Add.to.Cart) %>%
  mutate(prior.payment = as.numeric(gsub("[\\$,]", "", Prior.Payment))) %>%
  mutate(amount.due = as.numeric(gsub("[\\$,]", "", Amount.Due))) %>%
  mutate(parcel.id = str_remove_all(parcel.id, pattern = ' ')) %>%
  mutate(locality = if_else(str_detect(address, 'Sapelo'), 'local', 'nonlocal')) %>%
  select(year, bill, name, parcel.id, address, state, locality, due.date, prior.payment, amount.due, paid.date)

## filter individual parcels and plot
ggplot(filter(tax, parcel.id == '0101A0084'), aes(year, prior.payment)) + 
  geom_point() 

temp <- filter(tax, parcel.id == '0101A0099')

## import owner category classifications
ownr1 <- read.csv(file.path(datadir, "property/transactions_sapelo_primary.csv"), stringsAsFactors = F) %>%
  select(grantee, grantee_category) %>%
  rename(owner = grantee, category = grantee_category)
ownr2 <- read.csv(file.path(datadir, "property/transactions_sapelo_primary.csv"), stringsAsFactors = F) %>%
  select(grantor, grantor_category) %>%
  rename(owner = grantor, category = grantor_category)
ownr3 <- read.csv(file.path(datadir, 'property/owners_sapelo_SydRA.csv'), stringsAsFactors = F) %>%
  mutate(category = ifelse(own_cat %in% c('LLC', 'LLP', 'INC', 'Non-descendant'), 'Non-descendant', own_cat)) %>%
  select(-c(X:X.13)) %>% 
  na.omit() %>%
  select(owner, category) 

## join owner classification data
ownr <- rbind(ownr1, ownr2, ownr3) %>%
  filter(!is.na(owner) & !category %in% c('', ' ') & !owner %in%  c('unknown', 'unkown', 'u', '', ' ')) %>%
  mutate(category = if_else(category %in% c('outsider', 'Non-descendant', 'l_outsider'), 'nontraditional', 
                            if_else(category %in% c('l_descendant', 'Descendant'), 'descendant',
                                    if_else(category == 'Heritage Authority', 'authority',
                                            if_else(category == 'unkown', 'unknown',
                                    category))))) %>%
  filter(!category %in% c('authority', 'County')) %>%
  distinct()

## join owncat info from transactions data to tax bills data using name of record with distance of 5 (most optimal)
## help here maybe: https://cran.r-project.org/web/packages/fuzzyjoin/readme/README.html
joined <- tax %>%
  group_by(year) %>%
  stringdist_inner_join(ownr, by = c(name = "owner"), max_dist = 3, ignore_case = TRUE) %>%
  mutate(year.bill = paste(year, bill)) %>%
  distinct(year.bill, .keep_all = T)

# tax2 <- tax %>%
#   group_by(year) %>%
#   left_join(., trx, by = join_by(name == grantee))
# select(-c(date:owner)) %>%
# select(-c(tax_zip:price.ha))

## sum taxes paid/due by year
tax.sum.yr <- joined %>%
  rename(ownership = category) %>%
  group_by(year, ownership, locality) %>%
  summarise(count = n(),
            due = sum(amount.due)+sum(prior.payment))

## plot amount owed by locality and ownership
fig.local <- tax.sum.yr %>%
  filter(year < 2023, ownership != 'unknown') %>%
  ggplot(., aes(year, due/count, color = ownership)) +
  geom_point() + 
  geom_smooth(method = lm) +
  scale_y_continuous(name = 'Average Amount Owed ($)', limits = c(0,2500), breaks = seq(0,2500,200)) + 
  scale_x_continuous(breaks = seq(1999, 2022, 3), minor_breaks = seq(1999,2022,1)) + 
  ggtitle('Taxes Owed by Locality & Ownership') + 
  facet_wrap('locality')
fig.local

png(paste0(datadir, 'figures/relational/', 'taxes-by-locality.png'), 
    height = 4, width = 7, units = 'in', res = 150)
fig.local
dev.off()

## sum number addresses by year
## add r-squared value
## https://stackoverflow.com/questions/7549694/add-regression-line-equation-and-r2-on-graph

# lm_eqn <- function(df){
#   m <- lm(y ~ x, df);
#   eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
#                    list(a = format(unname(coef(m)[1]), digits = 2),
#                         b = format(unname(coef(m)[2]), digits = 2),
#                         r2 = format(summary(m)$r.squared, digits = 3)))
#   as.character(as.expression(eq));
# }


## plot # tax addresses by ownership/locality
fig <- tax.sum.yr %>%
  # mutate(year = ymd(year, truncated = 2L)) %>%
  group_by(year, ownership) %>%
  filter(year < 2023, ownership != 'unknown') %>%
  # summarise(count = sum(count)) %>%
  ggplot(aes(year, count, color = ownership)) +
  geom_point() + 
  geom_smooth(method = lm) +
  # geom_text(x = 25, y = 300, label = lm_eqn(.), parse = TRUE) + 
  # geom_smooth(se = F, lty = 'dotted') + 
  scale_y_continuous(name = "Addresses (#)",
                     breaks = seq(0,250, 20)) + 
  scale_x_continuous(breaks = seq(1999, 2022, 3), minor_breaks = seq(1999,2022,1)) + 
  ggtitle('# Tax Addresses by Locality & Ownership') + 
  facet_wrap('locality')
fig

png(paste0(datadir, 'figures/relational/', 'taxaddress-count-by-ownership.png'), 
    height = 4, width = 7, units = 'in', res = 150)
fig
dev.off()

## plot % of tax addresses by year next???
tax.prop<- tax.sum.yr %>%
  group_by(year, ownership) %>%
  pivot_wider(names_from = locality, values_from = c(count, due)) %>%
  mutate(nl_cnt_prop = count_nonlocal/(count_nonlocal+count_local))

fig.prop <- tax.prop %>%
  filter(year < 2023, ownership != 'unknown') %>%
  ggplot(aes(year, nl_cnt_prop*100, color = ownership)) +
  geom_point() + 
  scale_y_continuous(name = "Nonlocal Tax Addresses (%)",
                     breaks = seq(0,100, 10),
                     limits = c(0,100)) + 
  scale_x_continuous(breaks = seq(1999, 2022, 3), minor_breaks = seq(1999,2022,1)) + 
  ggtitle('Percentage Nonlocal Tax Addresses by Ownership')
fig.prop

png(paste0(datadir, 'figures/relational/', 'taxaddress-percentage-by-ownership.png'), 
    height = 4, width = 7, units = 'in', res = 150)
fig.prop
dev.off()

#####################
## geocode addresses
#####################
ll <- joined %>%
  geocode(address, method = 'arcgis', lat = latitude , long = longitude)

sap.geo = st_as_sf(ll, coords = c("longitude", "latitude"),
                  crs = 4326, agr = "constant")

## export geocoded data for re-import
st_write(sap.geo, file.path(datadir, 'spatial-data/geocoded/tax_geocode.GEOJSON'), 'GEOJSON', delete_dsn=TRUE)
