rm(list=ls())

library(tidyverse)
library(stringr)
library(lubridate)
library(readxl)
library(sf)
library(tmap)
library(tidygeocoder)
library(ggplot2)
library(maps)

# load United States state map data
MainStates <- map_data("state")

utm <- 2150 ## NAD83 17N

## tidygeocoder
## https://cran.r-project.org/web/packages/tidygeocoder/readme/README.html

## define data directory
datadir <- '/Users/dhardy/Dropbox/r_data/sapelo'

## import property owner data
o <- read.csv(file.path(datadir, 'property/owners_sapelo_primary.csv'), stringsAsFactors = F) %>%
  mutate(own3cat = ifelse(own_cat %in% c('LLC', 'LLP', 'INC', 'Non-descendant'), 'Non-descendant', own_cat),
         own4cat = ifelse(own_cat %in% c('LLC', 'LLP', 'INC'), 'Company', own_cat),
         zip = ifelse(str_length(zip) == 4, paste0("0", zip), zip))

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
  
ll <- lat_longs %>%
  filter(longitude < 0)
  
# library(usmap)
# plot_usmap(regions = "states") + 
#     labs(title = "US State",
#          subtitle = "This is a blank map of the counties of the United States.") + 
#     theme(panel.background = element_rect(color = "black", fill = "lightblue"))

# sf version 0.3-4, 0.4-0
hh.geo = st_as_sf(ll, coords = c("longitude", "latitude"), 
                 crs = 4326, agr = "constant") %>%
  filter(!own3cat %in% c('Heritage Authority', 'County'))

ggplot(hh.geo) +
  geom_polygon(data=MainStates, aes(x=long, y=lat, group=group),
                color="black", fill="white") +
  geom_sf(aes(color = own4cat)) +
  coord_sf(default_crs = sf::st_crs(4326)) + 
  theme_void()

off.island <- hh.geo %>% filter(zip != 31327)
off.island.outsider <- off.island %>% filter(own3cat == 'Non-descendant')


