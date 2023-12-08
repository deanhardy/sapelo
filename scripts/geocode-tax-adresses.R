###############################################################################################
## PURPOSE: geocoding tax assessor records 
## BY: Dean Hardy
###############################################################################################
rm(list=ls())

library(tidyverse)
library(sf)
library(tidygeocoder)

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


## import transactions data
trx <- read.csv(file.path(datadir, "property/transactions_sapelo_primary.csv"), stringsAsFactors = F) %>%
  mutate(date = as.Date(date, "%m/%d/%y")) %>%
  mutate(year = year(date), group = ifelse(price >0, "Money", "No Money")) %>%
  mutate(group = factor(group, levels = (c("No Money", "Money")))) %>%
  mutate(price.ha = price/(acres * 0.404686)) %>%
  filter(acres != 'na',
         !grantee_category %in% c('unknown', 'unkown', 'u', '')) %>%
  mutate(parcel.id = str_remove_all(parcel.id, pattern = ' ')) %>%
  rename(parcel_id = parcel.id)

## import tax records, merge address-related columns, filter SIHA and local addresses, clean up headers
df <- read.csv(paste0(datadir, 'property/taxes/tax_records_99-23.csv'), stringsAsFactors = F) %>%
  mutate(ZIP.Code = as.character(ZIP.Code),
         zip = if_else(str_length(ZIP.Code) == 4, paste0("0", ZIP.Code), ZIP.Code)) %>%
  select(-c(X:X.13)) %>% 
  mutate(address = paste(Property.Address, City, State, zip)) %>%
  # mutate(across(starts_with("Prior.Payment"), ~gsub("\\$", "", .) %>% as.numeric)) %>%
  # mutate(across(starts_with("Amount.Due"), ~gsub("\\$", "", .) %>% as.numeric)) %>%
  filter(!str_detect(Deed.Name, 'HERITAGE'), 
         City != 'Sapelo Island') %>%
  rename(year = Year, bill = Bill.., name = Deed.Name, parcel.id = Map.Code, due.date = Due.Date, paid.date = Add.to.Cart) %>%
  mutate(prior.payment = as.numeric(gsub("[\\$,]", "", Prior.Payment))) %>%
  mutate(amount.due = as.numeric(gsub("[\\$,]", "", Amount.Due))) %>%
  select(year, bill, name, parcel.id, address, due.date, prior.payment, amount.due, paid.date)

## sum taxes paid/due by year
tax.sum.yr <- df %>%
  group_by(year) %>%
  summarise(count = n(),
            due = sum(amount.due)+sum(prior.payment))

ggplot(tax.sum.yr) +
  geom_point(aes(year, due))

#####################
## geocode addresses
#####################
# ll <- df %>%
#   geocode(address, method = 'arcgis', lat = latitude , long = longitude)

# sap.geo = st_as_sf(ll, coords = c("longitude", "latitude"), 
#                   crs = 4326, agr = "constant")

## export geocoded data for re-import
# st_write(sap.geo, file.path(datadir, 'spatial-data/geocoded/tax_geocode.shp'), 'ESRI Shapefile', delete_dsn=TRUE)

## re-import geocoded data
sap.geo <- st_read(file.path(datadir, 'spatial-data/geocoded/tax_geocode.shp'), stringsAsFactors = F) %>%
  mutate(parcl_d = str_remove_all(parcl_d, pattern = ' ')) %>%
  rename(parcel_id = parcl_d)

## join owncat info from transactions data
## still need to import transactions data
sap.geo2 <- left_join(sap.geo, trx, by = join_by(name == grantee))
  # select(-c(date:owner)) %>%
  # select(-c(tax_zip:price.ha))



# load United States state map data
MainStates <- map_data("state")

ggplot(filter(sap.geo, year == 2000)) +
  geom_polygon(data=MainStates, aes(x=long, y=lat, group=group),
               color="black", fill="white") +
  geom_sf() +
  coord_sf(default_crs = sf::st_crs(4326)) + 
  theme_void()

