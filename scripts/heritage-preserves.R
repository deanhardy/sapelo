rm(list=ls())

#### define libraries and parameters ####
library(sf)
library(tidycensus)
library(tidyverse)
library(readxl)
library(stringr)
library(tmap)
library(tigris)
options(scipen=999)

## for area calculations
alb <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-84 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs" ## http://spatialreference.org/ref/sr-org/albers-conic-equal-area-for-florida-and-georgia/

## define data directory
datadir <- file.path('/Users/dhardy/Dropbox/r_data/sapelo')

#### import data ####
## cons lands from GA DNR 2019 via georgia data spatial data clearinghouse
cl <- st_read(file.path(datadir, 'cl19'), stringsAsFactors = F) %>%
  rename(GIS_name = name) %>%
  mutate(GIS_name = str_squish(GIS_name)) %>%
  mutate(GIS_name = str_trim(GIS_name)) %>%
  st_transform(4326)

## heritage preserves from TOIF
hp <- read.csv(file.path(datadir, 'heritage-preserves/preserves.csv'), stringsAsFactors = F) %>%
  mutate(GIS_name = str_squish(GIS_name)) %>%
  mutate(GIS_name = str_trim(GIS_name)) %>%
  filter(GIS_name != "")

## join cons lands spatial data to preserves list data and calc area
hp2 <- inner_join(cl, hp, by = 'GIS_name') %>% 
  st_transform(alb) %>%
  mutate(acres = as.numeric(st_area(geometry) * 0.00024710538)) %>%
  st_transform(4326)

#### ancillary data for cartographic use ####
## https://www.census.gov/geo/maps-data/maps/2010ua.html
st <- states(cb = FALSE, resolution = '500k', year = NULL) %>%
  st_as_sf() %>%
  filter(NAME %in% c('Georgia', 'South Carolina', 'North Carolina', 'Alabama', 'Florida', 'Tennessee')) %>%
  st_transform(4326)

## download desired states
ga <- states(cb = FALSE, resolution = '500k', year = NULL) %>%
  st_as_sf() %>%
  filter(NAME == 'Georgia') %>%
  st_transform(4326)

## download counties
cnty <- counties('GA', resolution = '500k', year = 2018) %>%
  st_as_sf() %>%
  st_transform(4326)

## download desired urban areas
urb <- urban_areas(cb = TRUE, year = 2018) %>%
  st_as_sf() %>%
  filter(NAME10 %in% c('Athens-Clarke County, GA', 'Savannah, GA', 'Albany, GA', 'Macon, GA',
                       'Atlanta, GA', 'Brunswick, GA', 'Augusta-Richmond County, GA--SC',
                       'Valdosta, GA')) %>%
  st_centroid() %>%
  mutate(name = ifelse(NAME10 == 'Athens-Clarke County, GA', 'Athens',
                       ifelse(NAME10 == 'Savannah, GA', 'Savannah',
                              ifelse(NAME10 == 'Atlanta, GA', 'Atlanta',
                                     ifelse(NAME10 == 'Macon, GA', 'Macon',
                                            ifelse(NAME10 == 'Brunswick, GA', 'Brunswick',
                                                   ifelse(NAME10 == 'Augusta-Richmond County, GA--SC', 'Augusta', 
                                                          ifelse(NAME10 == 'Albany, GA', 'Albany', 
                                                                 ifelse(NAME10 == 'Valdosta, GA', 'Valdosta', 
                                                                        ifelse(NAME10 == 'Rome, GA', 'Rome', NA)))))))))) %>%
  st_transform(4326)

## natural earth ocean data
# ocean <- st_read(file.path(datadir, 'spatial-data/ocean/ne_10m_ocean.shp')) %>%
#   st_transform(4326)

cst <- coastline(year = 2018) %>%
  st_as_sf() %>%
  st_transform(4326)

## download water features for states
cnty_list <- c('Chatham', 'Liberty', 'Bryan', 'McIntosh', 'Glynn', 'Camden')
OUT <- NULL
wtr <- NULL
for(i in cnty_list){
  OUT <- area_water('GA', i, year = 2018) %>%
    st_as_sf()
  wtr <- rbind(wtr, OUT)
}

## map of GA HPs region
fig <- 
  tm_shape(ga) + tm_fill(col = 'white') +
  tm_shape(st) + tm_fill(col = 'grey95', lwd = 0.5) +
  tm_shape(ga) + tm_fill(col = 'white') +
  tm_shape(cnty) + tm_borders(col = 'grey80', lwd = 0.2) +
  tm_shape(st) + tm_borders(col = 'grey50') + 
  tm_shape(wtr) + tm_fill(col = 'grey85') +
  tm_shape(hp2) + tm_fill(col = 'black') +
  tm_shape(urb) + tm_dots(col = 'black', size = 0.05, shape = 15, legend.show = FALSE) + 
  tm_text(text = 'name', size = 0.8, just = 'right', xmod = -0.1, ymod = -0.1, shadow = FALSE) + 
  tm_credits(text = 'Map of Georgia Heritage Preserves.', size = 0.8,
             position = c(0.0, 0.0)) + 
  tm_layout(frame = TRUE, 
            bg.color = 'grey85',
            outer.bg.color = 'black',
            outer.margins=c(0,0,0,0),
            # inner.margins=c(0,0,0,0), 
            asp=4/4)
# fig

tiff(file.path(datadir, 'figures/heritage-preserves-bw.tiff'), units = 'in',
     height = 4, width = 4, res = 300, compression = 'lzw')
fig
dev.off()
