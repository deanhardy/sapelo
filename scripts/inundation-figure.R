rm(list=ls())

#library(tidyverse)
library(sf)
library(raster)
library(lubridate)
library(tmap)
library(raster)
library(fasterize)

## define data directory
datadir <- '/Users/dhardy/Dropbox/r_data/sapelo'

## import parcel owner data and trans
df <- st_read(file.path(datadir, 'spatial-data/parcel_data_export/parcel_data.shp'), stringsAsFactors = F) %>%
  st_transform(4326) %>%
  mutate(owner = ifelse(is.na(owner), 'unknown', owner)) %>%
  filter(own4cat != 'County' & gis_acres != 'NA')

## filter out just companies
comp <- df %>%
  filter(own4cat == 'Company')

## import ag data
ag <- st_read(file.path(datadir, 'spatial-data/ag_plots/'), stringsAsFactors = F)[-1,] %>%
  st_transform(4326)

ag_cntr <- st_centroid(ag)

## import water level data
hobo <- st_read(file.path(datadir, 'spatial-data/hobo_sites/'), stringsAsFactors = F) %>%
  st_transform(4326) %>%
  rename(site = Id)
info <- read.csv(file.path(datadir, 'water-level/datums.csv'), stringsAsFactors = F) %>%
  mutate(install_date = as.Date(install.date, '%m/%d/%y'),
         site = as.numeric(site)) %>%
  dplyr::select(site, install_date)
hobo <- left_join(hobo, info)

inund <- raster(file.path(datadir, 'spatial-data/inundation/inund2100hc.tif'))

## define map variables
clr4 <- c('black', 'grey60', 'orange')
leafIcon <- tmap_icons("http://leafletjs.com/examples/custom-icons/leaf-green.png")

map.own3cat <- 
  tm_shape(df) + 
    tm_fill('own3cat', palette = clr4, 
            title = 'B)\nOwner Category') + 
  tm_shape(df) + tm_borders(lwd = 0.5) + 
  tm_shape(comp) + tm_borders('yellow') + 
  tm_shape(ag) + tm_fill('green') + 
  tm_shape(inund) + 
    tm_raster(alpha = 0.5,
              n = 5) + 
  tm_shape(ag_cntr) + 
    tm_symbols(shape = leafIcon, border.lwd = NA,
             just = c(0.4, 2),
             size = 0.5) + 
  tm_shape(hobo) +
    tm_markers(just = c(0,2),
             size = 0.1) + 
  tm_layout(frame = FALSE,
            legend.text.size = 0.8,
            legend.title.size = 1)

map.own3cat

tiff(file.path(datadir, 'figures/inundation-figure-annals.tiff'), units = 'in', width = 3.5, height = 3.5, 
     res = 600, compression = 'lzw')
map.own3cat
dev.off()


 