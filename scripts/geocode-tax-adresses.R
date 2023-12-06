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

## import tax records, merge address-related columns, filter SIHA and local addresses, clean up headers
df <- read.csv(paste0(datadir, 'property/taxes/tax_records_99-23.csv'), stringsAsFactors = F) %>%
  mutate(ZIP.Code = as.character(ZIP.Code),
         zip = if_else(str_length(ZIP.Code) == 4, paste0("0", ZIP.Code), ZIP.Code)) %>%
  select(-c(X:X.13)) %>% 
  na.omit() %>%
  mutate(address = paste(Property.Address, City, State, zip)) %>%
  # mutate(across(starts_with("Prior.Payment"), ~gsub("\\$", "", .) %>% as.numeric)) %>%
  # mutate(across(starts_with("Amount.Due"), ~gsub("\\$", "", .) %>% as.numeric)) %>%
  filter(!str_detect(Deed.Name, 'HERITAGE'), City != 'Sapelo Island') %>%
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
ll <- df %>%
  geocode(address, method = 'arcgis', lat = latitude , long = longitude)

sap.geo = st_as_sf(ll, coords = c("longitude", "latitude"), 
                  crs = 4326, agr = "constant")

## join owncat info from transactions data
## still need to import transactions data
sap.geo2 <- left_join(sap.geo, o, by = join_by(name == owner))

# load United States state map data
MainStates <- map_data("state")

ggplot(filter(sap.geo, year == 2000)) +
  geom_polygon(data=MainStates, aes(x=long, y=lat, group=group),
               color="black", fill="white") +
  geom_sf() +
  coord_sf(default_crs = sf::st_crs(4326)) + 
  theme_void()

## import property owner data
o <- read.csv(file.path(datadir, 'property/owners_sapelo_SydRA.csv'), stringsAsFactors = F) %>%
  mutate(own3cat = ifelse(own_cat %in% c('LLC', 'LLP', 'INC', 'Non-descendant'), 'Non-descendant', own_cat),
         own4cat = ifelse(own_cat %in% c('LLC', 'LLP', 'INC'), 'Company', own_cat),
         zip = ifelse(str_length(zip) == 4, paste0("0", zip), zip)) %>%
  select(-c(X:X.13)) %>% 
  na.omit()

## import property spatial data
p <- st_read(file.path(datadir, 'spatial-data/parcels/parcels_2023.shp'), stringsAsFactors = F) %>%
  st_transform(utm) %>%
  rename(parcel_id = PARCEL_ID)
p$parcel_id <- str_squish(p$parcel_id)

po <- full_join(p, o, by = 'parcel_id')
