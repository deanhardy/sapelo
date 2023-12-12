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

## import geocoded data
geo <- st_read(file.path(datadir, 'spatial-data/geocoded/tax_geocode.GEOJSON'), stringsAsFactors = F) %>%
  st_transform(4269)

## download CBSAs including metro and micro statistical areas
cbsa <- get_acs(geography = 'cbsa',
                variables = c(medincome = "B19013_001"), 
                year = 2022,
                geometry = T)
cbsa %>%
  ggplot(aes(fill = estimate)) + 
  geom_sf(color = NA) + 
  scale_fill_viridis_c(option = "magma") 

temp <- st_intersects(cbsa, geo)


