rm(list=ls())

library(sf)
library(tidycensus)
library(tidyverse)
library(readxl)
library(stringr)
library(tmap)
library(tigris)

#define data directory
datadir <- file.path('/Users/dhardy/Dropbox/r_data/sapelo')

cl <- st_read(file.path(datadir, 'cl19'), stringsAsFactors = F) %>%
  rename(GIS_name = name) %>%
  mutate(GIS_name = str_squish(GIS_name)) %>%
  mutate(GIS_name = str_trim(GIS_name)) %>%
  st_transform(4326)

clst <- cl %>%
  filter(GIS_name == 'LITTLE TYBEE-CABBAGE ISLAND TRACT')

# hp <- read_excel(file.path(datadir, 'heritage-preserves/heritage-preserves.xlsx'), 
#                  sheet = 'preserves') %>%
#   filter(!is.na(GIS_name))

hp <- read.csv(file.path(datadir, 'heritage-preserves/preserves.csv'), stringsAsFactors = F) %>%
  mutate(GIS_name = str_squish(GIS_name)) %>%
  mutate(GIS_name = str_trim(GIS_name)) %>%
  filter(GIS_name != "")

hp2 <- inner_join(cl, hp, by = 'GIS_name')

tm_shape(hp2) + 
  tm_fill(col = 'green')

## download ancillary data for cartographic use
## https://www.census.gov/geo/maps-data/maps/2010ua.html
st <- states(cb = FALSE, resolution = '500k', year = NULL) %>%
  st_as_sf() %>%
  filter(NAME %in% c('Georgia', 'South Carolina', 'North Carolina', 'Alabama', 'Florida', 'Tennessee')) %>%
  st_transform(4326)

ga <- states(cb = FALSE, resolution = '500k', year = NULL) %>%
  st_as_sf() %>%
  filter(NAME == 'Georgia') %>%
  st_transform(4326)

cnty <- counties('GA', resolution = '500k', year = 2018) %>%
  st_as_sf() %>%
  st_transform(4326)

urb <- urban_areas(cb = TRUE, year = 2018) %>%
  st_as_sf() %>%
  filter(NAME10 %in% c('Athens-Clarke County, GA', 'Savannah, GA', 'Albany, GA', 'Macon, GA',
                       'Atlanta, GA', 'Darien, GA', 'Augusta-Richmond County, GA--SC')) %>%
  st_centroid() %>%
  mutate(name = ifelse(NAME10 == 'Athens-Clarke County, GA', 'Athens',
                       ifelse(NAME10 == 'Savannah, GA', 'Savannah',
                              ifelse(NAME10 == 'Atlanta, GA', 'Atlanta',
                                     ifelse(NAME10 == 'Macon, GA', 'Macon',
                                            ifelse(NAME10 == 'Darien, GA', 'Darien',
                                                   ifelse(NAME10 == 'Augusta-Richmond County, GA--SC', 'Augusta', 
                                                          ifelse(NAME10 == 'Albany, GA', 'Albany', NA)))))))) %>%
  st_transform(4326)

## map of GA HPs region
fig <- 
  tm_shape(ga) + tm_fill(col = 'white') +
  tm_shape(st) + tm_fill(col = 'grey90') +
  # tm_shape(t2) + tm_borders(col = 'grey65') +
  # tm_shape(t3) + tm_fill(col = 'grey95') +
  tm_shape(ga) + tm_fill(col = 'grey100') +
  tm_shape(cnty) + tm_borders(col = 'grey50') +
  tm_shape(hp2) + tm_fill(col = 'green') +
  tm_shape(st) + tm_borders(col = 'black') + 
  tm_shape(urb) + tm_dots(col = 'black', size = 0.2, shape = 15, legend.show = FALSE) + 
  tm_text(text = 'name', just = 'left', xmod = 0.3, ymod = -0.3, shadow = TRUE) + 
  # tm_compass(type = 'arrow', size = 3, position = c(0.83,0.08)) +
  # tm_scale_bar(breaks = c(0,40, 80), size = 1, position= c(0.8, 0.0)) +
  # tm_add_legend(type = c('fill'), labels = c('NCED', 'PADUS', 'TNC'), 
  #               col = clr, 
  #               title = "Data Source") +
  # tm_legend(position = c(0.75, 0.04),
  #           bg.color = 'white',
  #           frame = TRUE,
  #           legend.text.size = 1.2,
  #           legend.title.size = 1.4) + 
  tm_layout(frame = TRUE, 
            bg.color = 'skyblue',
            outer.bg.color = 'black',
            outer.margins=c(0,0,0,0),
            # inner.margins=c(0,0,0,0), 
            asp=3/2)
# fig

tiff(file.path(datadir, 'figures/heritage-preserves.tiff'), units = 'in',
     height = 4, width = 6, res = 300, compression = 'lzw')
fig
dev.off()
