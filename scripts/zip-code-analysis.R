rm(list=ls())

library(tidyverse)
library(stringr)
library(lubridate)
library(readxl)
library(sf)
library(tmap)
library(tidygeocoder)
library(ggplot2)

utm <- 2150 ## NAD83 17N

## tidygeocoder
## https://cran.r-project.org/web/packages/tidygeocoder/readme/README.html

## define data directory
datadir <- '/Users/dhardy/Dropbox/r_data/sapelo'

## import property owner data
o <- read.csv(file.path(datadir, 'property/owners_sapelo_primary.csv'), stringsAsFactors = F) %>%
  mutate(own3cat = ifelse(own_cat %in% c('LLC', 'LLP', 'INC', 'Non-descendant'), 'Non-descendant', own_cat)) %>%
  mutate(own4cat = ifelse(own_cat %in% c('LLC', 'LLP', 'INC'), 'Company', own_cat))

p <- st_read(file.path(datadir, 'spatial-data/parcels/'), stringsAsFactors = F) %>%
  st_transform(utm) %>%
  rename(parcel_id = PARCEL_ID)
p$parcel_id <- str_squish(p$parcel_id)

# as.data.frame(table(unique(p$parcel_id)))

## manual entry of acreage for those without shapefile boundary
po <- full_join(p, o, by = 'parcel_id') %>%
  mutate(gis_acres = 
           ifelse(parcel_id == '0102A 0134002', 1.21, 
                  ifelse(parcel_id == '0102A 0134', gis_acres - 1.21,
                         ifelse(parcel_id == '0102A 0090', 1.15, 
                                ifelse(parcel_id == '0101A 0068001', 1, 
                                       ifelse(parcel_id == '0101A 0106001', 0.25,
                                              ifelse(parcel_id == '0102A 0025', 1,
                                                     ifelse(parcel_id == '0102A 0045001', 2,
                                                            ifelse(parcel_id %in% c('0101A 0071', '0101A 0071001', '0101A 0071002', '0101A 0071003', '0101A 0071004'), 0.4, gis_acres))))))))) %>%
  mutate(own_cat = ifelse(is.na(own_cat), 'Unknown', own_cat)) %>%
  mutate(own3cat = ifelse(is.na(own3cat), 'Unknown', own3cat)) %>%
  mutate(own4cat = ifelse(is.na(own4cat), 'Unknown', own4cat))

po <- po %>% 
  # filter(street != '') %>%
  mutate(address = paste(house_no, street, city, state, zip))

lat_longs <- po %>%
  geocode(address, method = 'arcgis', lat = latitude , long = longitude)

# lat_longs_cascade <- po %>%
#     geocode_combine( 
#     queries = list(
#       list(method = 'census', mode = 'batch'),
#       list(method = 'census', mode = 'single'),
#       list(method = 'osm')
#     ),
#     global_params = list(street = 'street', 
#                          city = 'city', state = 'state', postalcode = 'zip'),
#     query_names = c('census batch', 'census single', 'osm')
#   )

ggplot(lat_longs, aes(longitude, latitude), color = "grey99") +
  borders("state") + geom_point() +
  # ggrepel::geom_label_repel(aes(label = own_cat)) +
  theme_void()
