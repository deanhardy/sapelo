rm(list=ls())

#library(tidyverse)
library(sf)
library(raster)
library(lubridate)
library(tmap)
library(fasterize)
library(tidyverse)
library(rgdal)
library(grid)
library(gridExtra)

## define data directory
datadir <- '/Users/dhardy/Dropbox/r_data/sapelo'

## import community boundaries
df <- st_read(file.path(datadir, 'spatial-data/communities/communities.shp'), stringsAsFactors = F) %>%
  st_transform(4326)
rb <- df %>% filter(name == 'Raccoon Bluff')
hh <- df %>% filter(name == 'Hog Hammock')

# inund <- raster(file.path(datadir, 'spatial-data/inundation/inund2100hc.tif'))
tidal_hh <- raster(file.path(datadir, 'spatial-data/inundation/is01.tif'))
tidal_rb <- raster(file.path(datadir, 'spatial-data/inundation/is0_rc.tif'))

##import inundation data
inund <- st_read(file.path(datadir, 'spatial-data/inundation/inund2100hc_poly.shp'), stringsAsFactors = F) %>%
  st_transform(4326) %>%
  # filter(prb_smplfy != '1%') %>%
  mutate(prb_smplfy = ifelse(prblty %in% c('50%', '95%'), '50%', prblty)) %>%
  st_make_valid()

## define map variables
clr.ind <- c('#BFE8FF', '#00a9e6','#004c73', '#004c73')

tmap_options(check.and.fix = TRUE)

## separate inundation map
hh.ind <- 
  tm_shape(hh, unit = 'mi') + tm_borders() + 
  tm_shape(inund) + 
  tm_fill('prb_smplfy', alpha = 0.5, palette = clr.ind,
          title = 'Inundation Probability', group = 
            'funky') + 
  tm_shape(tidal_hh, raster.downsample = FALSE) + 
  tm_raster(title = '', alpha = 1, palette = '#89cd66', legend.show = FALSE) + 
  tm_shape(hh) + tm_borders(lwd = 1, col = 'black') + 
  tm_credits('A)', fontface = "bold", position = c("right", 'top')) + 
  tm_layout(frame = TRUE,
            legend.text.size = 0.7,
            legend.title.size = 0.8,
            legend.bg.color = 'white',
            legend.frame = 'black',
            title.snap.to.legend = TRUE) +
  tm_add_legend(type = 'fill',
                labels = 'Tidal Marsh',
                col = '#89cd66',
                border.lwd = 0,
                group = 'funky') +
  tm_scale_bar(breaks = c(0,0.25), text.size = 0.7, position = c(0.65,0))  
hh.ind

## separate inundation map
rb.ind <- 
  tm_shape(rb, unit = 'mi') + tm_borders() + 
  tm_shape(inund) + 
   tm_fill('prb_smplfy', alpha = 0.5, palette = clr.ind,
          group = 'funky', legend.show = FALSE) + 
  tm_shape(tidal_rb, raster.downsample = FALSE) + 
  tm_raster(title = '', alpha = 1, palette = '#89cd66', legend.show = FALSE) + 
  tm_shape(rb) + tm_borders(lwd = 1, col = 'black') + 
  tm_credits('B)', fontface = "bold", position = c("right", 'top')) + 
  tm_scale_bar(breaks = c(0,0.25), text.size = 0.7, position = c(0.7,0)) + 
  tm_compass(position = c(0.75, 0.11), text.size = 0.7)
rb.ind

tiff(file.path(datadir, 'figures/inundation/hh.v.rb-inundation.tiff'), units = 'cm', width = 19, height = 9.5, 
     res = 300, compression = 'lzw')
tmap_arrange(hh.ind, rb.ind, widths = c(0.5, 0.5))
dev.off() 
