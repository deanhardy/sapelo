rm(list=ls())

#library(tidyverse)
library(sf)
library(raster)
library(lubridate)
library(tmap)
library(raster)
library(fasterize)
library(tidyverse)

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

# inund <- raster(file.path(datadir, 'spatial-data/inundation/inund2100hc.tif'))
tidal <- raster(file.path(datadir, 'spatial-data/inundation/is01.tif'))

##import inundation data
inund <- st_read(file.path(datadir, 'spatial-data/inundation/inund2100hc_poly.shp'), stringsAsFactors = F) %>%
  st_transform(4326) %>%
  filter(prb_smplfy != '1%') %>%
  mutate(prb_smplfy = ifelse(prblty %in% c('50%', '95%'), '50%', '5%'))

## define map variables
clr.own <- c('black', 'grey60', 'orange')
clr.ind <- c('#00a9e6', '#004c73')
leafIcon <- tmap_icons("http://leafletjs.com/examples/custom-icons/leaf-green.png")

map.own.ind <- 
  tm_shape(df) + 
    tm_fill('own3cat', palette = clr.own, 
            title = 'Owner Category') + 
  tm_shape(df) + tm_borders(lwd = 0.5) + 
  tm_shape(comp) + tm_borders('yellow') + 
  tm_shape(ag) + tm_fill('green') + 
  # tm_shape(inund,
  #          raster.downsample = FALSE) + 
  #   tm_raster(alpha = 0.5,
  #             n = 4,
  #             as.count = FALSE) + 
  tm_shape(inund) + 
    tm_fill('prb_smplfy', alpha = 0.5, palette = clr.ind,
            title = 'Inundation Probability') + 
  tm_shape(tidal, raster.downsample = FALSE) + 
    tm_raster(title = '', alpha = 1, palette = '#89cd66', legend.show = FALSE) + 
  tm_shape(ag_cntr) + 
    tm_symbols(shape = leafIcon, border.lwd = NA,
             just = c(0.4, 2),
             size = 0.5) + 
  tm_shape(hobo) +
    tm_markers(shape = marker_icon(),
               just = c(0,2),
             size = 0.1) + 
  tm_layout(frame = FALSE,
            legend.text.size = 0.6,
            legend.title.size = 0.7) +
  tm_add_legend(type = 'fill',
                labels = 'MHHW Extent',
                col = '#89cd66',
                border.lwd = 0) 
  # tm_add_legend(type = 'symbol',
  #               label = 'Agricultural Plot',
  #               shape = leafIcon) + 
  # tm_add_legend(type = 'symbol',
  #               shape = marker_icon(),
  #               label = 'Water Level Logger')
map.own.ind

tiff(file.path(datadir, 'figures/doubleD-fig.tiff'), units = 'in', width = 3.5, height = 3.5, 
     res = 300, compression = 'lzw')
map.own.ind
dev.off()


## separate inundation map
map.ind <- 
  tm_shape(df) + tm_borders(lwd = 0.5) + 
  tm_shape(inund) + 
  tm_fill('prb_smplfy', alpha = 0.5, palette = clr.ind,
          title = 'A)\nInundation Probability') + 
  tm_shape(tidal, raster.downsample = FALSE) + 
  tm_raster(title = '', alpha = 1, palette = '#89cd66', legend.show = FALSE) + 
  tm_layout(frame = FALSE,
            legend.text.size = 0.6,
            legend.title.size = 0.7) +
  tm_add_legend(type = 'fill',
                labels = 'MHHW Extent',
                col = '#89cd66',
                border.lwd = 0) 
# tm_add_legend(type = 'symbol',
#               label = 'Agricultural Plot',
#               shape = leafIcon) + 
# tm_add_legend(type = 'symbol',
#               shape = marker_icon(),
#               label = 'Water Level Logger')
map.ind

## separate owner map with ag and water loggers
map.own <- 
  tm_shape(df) + 
  tm_fill('own3cat', palette = clr.own, 
          title = 'B)\nOwner Category') + 
  tm_shape(df) + tm_borders(lwd = 0.5) + 
  tm_shape(comp) + tm_borders('yellow') + 
  tm_shape(ag) + tm_fill('green') + 
  # tm_shape(inund,
  #          raster.downsample = FALSE) + 
  #   tm_raster(alpha = 0.5,
  #             n = 4,
  #             as.count = FALSE) + 
  tm_shape(tidal, raster.downsample = FALSE) + 
  tm_raster(title = '', alpha = 1, palette = '#89cd66', legend.show = FALSE) + 
  tm_shape(ag_cntr) + 
  tm_symbols(shape = leafIcon, border.lwd = NA,
             just = c(0.4, 2),
             size = 0.5) + 
  tm_shape(hobo) +
  tm_markers(shape = marker_icon(),
             just = c(0,2),
             size = 0.1) + 
  tm_layout(frame = FALSE,
            legend.text.size = 0.6,
            legend.title.size = 0.7) 
  # tm_add_legend(type = 'fill',
  #               labels = 'MHHW Extent',
  #               col = '#89cd66',
  #               border.lwd = 0) 
  # tm_add_legend(type = 'symbol', border.lwd = NA,
  #             labels = 'Agricultural Plot',
  #             shape = tmap_icons("http://leafletjs.com/examples/custom-icons/leaf-green.png"),
  #             size = 0.5)
# tm_add_legend(type = 'symbol',
#               shape = marker_icon(),
#               labels = 'Water Level Logger')
map.own

library(grid)
library(gridExtra)
tiff(file.path(datadir, 'figures/doubleD-fig-option1.tiff'), units = 'in', width = 7, height = 3.5, 
     res = 150, compression = 'lzw')
tmap_arrange(map.ind, map.own, widths = c(0.5, 0.5))
dev.off()



 