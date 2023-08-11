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

## import parcel owner data and trans
df <- st_read(file.path(datadir, 'spatial-data/parcel_data_export/parcel_data.geojson'), stringsAsFactors = F) %>%
  st_transform(4326) %>%
  mutate(owner = ifelse(is.na(owner), 'unknown', owner),
         own3cat = ifelse(own3cat == 'Non-traditional', 'Non-Descendant', own3cat)) %>%
  filter(own4cat != 'County' & gis_acres != 'NA')

## temporary fix to empty parcels
df <- df %>% filter(!st_is_empty(.))

tmap_options(check.and.fix = TRUE)
ts <- 1.3 ## set figure text size 


ggplot() + geom_sf(data = df)
  
  
## fema flood map
map.flood <- 
  tm_shape(df) + tm_borders() +
  # tm_shape(inund) + 
  # tm_fill('prb_smplfy', alpha = 0.5, palette = ,
  #         title = 'A)\nInundation Probability', group = 
  #           'funky') + 
  # tm_shape(tidal, raster.downsample = FALSE) + 
  # tm_raster(title = '', alpha = 1, palette = '#89cd66', legend.show = FALSE) + 
  tm_shape(df) + tm_borders(lwd = 1, col = 'black') + 
  tm_layout(frame = TRUE,
            legend.text.size = ts,
            legend.title.size = ts,
            legend.title.fontface = 1,
            legend.bg.color = 'white',
            legend.frame = 'black',
            legend.width = 1.1,
            inner.margins=c(0.05,0,0,0.05), 
            title.snap.to.legend = FALSE) +
  tm_add_legend(type = 'fill',
                labels = 'Tidal Marsh',
                col = '#89cd66',
                border.lwd = 0,
                group = 'funky',
                size = ts)
map.flood

## export for presentations
toff(file.path(datadir, 'figures/femafloodmap.tiff'), units = 'in', width = 6, height = 4, 
    res = 300)
map.flood
dev.off()
