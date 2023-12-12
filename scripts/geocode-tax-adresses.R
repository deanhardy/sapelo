###############################################################################################
## PURPOSE: geocoding tax assessor records 
## BY: Dean Hardy
###############################################################################################
rm(list=ls())

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
  select(-c(X:X.13)) %>% 
  mutate(address = paste(Property.Address, City, State, zip)) %>%
  filter(!str_detect(Deed.Name, 'HERITAGE'), 
         City != 'Sapelo Island',
         City != 'Sapelo  Island',
         address != '   NA'
         ) %>%
  rename(state = State, year = Year, bill = Bill.., name = Deed.Name, parcel.id = Map.Code, due.date = Due.Date, paid.date = Add.to.Cart) %>%
  mutate(prior.payment = as.numeric(gsub("[\\$,]", "", Prior.Payment))) %>%
  mutate(amount.due = as.numeric(gsub("[\\$,]", "", Amount.Due))) %>%
  mutate(parcel.id = str_remove_all(parcel.id, pattern = ' ')) %>%
  select(year, bill, name, parcel.id, address, state, due.date, prior.payment, amount.due, paid.date)

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
  mutate(category = if_else(category %in% c('Non-descendant', 'l_outsider'), 'outsider', 
                            if_else(category %in% c('l_descendant', 'Descendant'), 'descendant',
                                    if_else(category == 'Heritage Authority', 'authority',
                                            if_else(category == 'unkown', 'unknown',
                                    category))))) %>%
  filter(!category %in% c('authority', 'County')) 

## join owncat info from transactions data to tax bills data using name of recor with distance of 5 (most optimal)
## help here maybe: https://cran.r-project.org/web/packages/fuzzyjoin/readme/README.html
joined <- tax %>%
  group_by(year) %>%
  stringdist_inner_join(ownr, by = c(name = "owner"), max_dist = 3, ignore_case = TRUE) %>%
  distinct()

# tax2 <- tax %>%
#   group_by(year) %>%
#   left_join(., trx, by = join_by(name == grantee))
# select(-c(date:owner)) %>%
# select(-c(tax_zip:price.ha))

## sum taxes paid/due by year
tax.sum.yr <- joined %>%
  group_by(year, category) %>%
  summarise(count = n(),
            due = sum(amount.due)+sum(prior.payment))

ggplot(tax.sum.yr) +
  geom_point(aes(year, due/1000, color = category)) + 
  scale_y_continuous(name = 'Amount Owed (x$1,000)', limits = c(0,600), breaks = seq(0,600,50))

#####################
## geocode addresses
#####################
ll <- joined %>%
  geocode(address, method = 'arcgis', lat = latitude , long = longitude)

sap.geo = st_as_sf(ll, coords = c("longitude", "latitude"),
                  crs = 4326, agr = "constant")

## export geocoded data for re-import
st_write(sap.geo, file.path(datadir, 'spatial-data/geocoded/tax_geocode.GEOJSON'), 'GEOJSON', delete_dsn=TRUE)
