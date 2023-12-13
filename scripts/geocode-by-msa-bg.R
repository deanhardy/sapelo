###############################################################################################
## PURPOSE: geolocating points inside MSAs and Block Groups 
## BY: Dean Hardy
###############################################################################################
rm(list=ls())

library(tidyverse)
library(sf)
library(tidygeocoder)
library(tidycensus)

## define data directory
datadir <- '/Users/dhardy/Dropbox/r_data/sapelo/'

## download CBSAs including metro and micro statistical areas
cbsa <- get_acs(geography = 'cbsa',
                variables = c("B19013_001"), 
                year = 2022,
                geometry = T) %>%
  rowid_to_column(., var = 'cbsa.id') 

cbsa.df <- st_drop_geometry(cbsa) %>%
  rename(cbsa.name = NAME)

cbsa %>%
  ggplot(aes(fill = estimate)) + 
  geom_sf(color = NA) + 
  scale_fill_viridis_c(option = "magma") 

## import geocoded data
geo <- st_read(file.path(datadir, 'spatial-data/geocoded/tax_geocode.GEOJSON'), stringsAsFactors = F) %>%
  st_transform(., st_crs(cbsa)) %>%
  rowid_to_column(., var = 'row.id')

## identify CBSAs of geocoded addresses
## some help: https://gis.stackexchange.com/questions/282750/identify-polygon-containing-point-with-r-sf-package
int <- data.frame(st_intersects(geo, cbsa)) %>%
  rename(cbsa.id = col.id)

## join cbsa info with geocoded data
geo.cbsa <- geo %>%
  left_join(., int, by = 'row.id') %>%
  left_join(., cbsa.df, by = 'cbsa.id')

geo.fltr.cbsa <- geo.cbsa %>%
  group_by(year, category, cbsa.name) %>%
  summarise(count = n())

## map data
usa <- st_as_sf(maps::map("state", fill=TRUE, plot =FALSE))

ggplot(filter(geo.fltr.cbsa, year == 2022)) + 
  # geom_sf(aes(size = count, color = category)) + 
  geom_sf(data = usa) +   
  geom_sf(color = "#2b2b2b", fill = "white", size=0.125) +
  geom_sf(data = cbsa) + 
  geom_point(
    aes(color = category, geometry = geometry, size = count),
    stat = "sf_coordinates",
    alpha = 0.8,
  ) + 
  scale_size(range = c(1, 5), name="count") + 
  theme(legend.position = "bottom")

    