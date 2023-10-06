rm(list=ls())

library(tidyverse)
library(sf)
library(lubridate)
library(readxl)

## define data directory
datadir <- '/Users/dhardy/Dropbox/r_data/sapelo'

## import parcel owner data and trans
df <- st_read(file.path(datadir, 'spatial-data/parcel_data_export/parcel_data.geojson'), stringsAsFactors = F) %>%
  st_transform(4326) %>%
  mutate(owner = ifelse(is.na(owner), 'unknown', owner)) %>%
  filter(gis_acres != 'NA') %>%
  mutate(own3cat = if_else(own3cat == 'Outsider', 'Non-traditional', 
                           if_else(own3cat == 'Other', 'County', own3cat))) %>%
  mutate(own3cat = fct_relevel(own3cat, c('Descendant', 'Heritage Authority', 'Non-traditional', 'County'))) %>%
  arrange(own3cat)

## import water level data
hobo <- st_read(file.path(datadir, 'spatial-data/hobo_sites/hobo_sites2.shp'), stringsAsFactors = F) %>%
  st_transform(4326) %>%
  mutate(site = as.numeric(site)) %>%
  dplyr::select(site)

## sete icon for water level sites
iconblue <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'white',
  library = 'ion',
  markerColor = 'blue'
)

## prep background imagery
## https://tmieno2.github.io/R-as-GIS-for-Economists/create-maps.html

ggplot() + 
  geom_sf(data = df) + 
  geom_sf_text(
    data = df,
    aes(label = parcel_id),
    check_overlap = TRUE,
    size = 3,
    color = "blue"
  )

