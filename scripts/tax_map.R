rm(list=ls())

library(tidyverse)
library(sf)
library(tmap)
library(tmaptools)

utm <- 2150 ## NAD83 17N

## define data directory
datadir <- '/Users/dhardy/Dropbox/r_data/sapelo'

p <- st_read(file.path(datadir, 'property/prcl_id_sap2015.shp'), stringsAsFactors = F) %>%
  st_transform(utm) %>%
  rename(parcel_id = PARCEL_ID)

t <- read.csv(file.path(datadir, 'property/taxes/parcels_assessed_values.txt')) %>%
  rename(parcel_id = PARCEL_ID) %>%
  mutate(assval_chg = ((HISTVAL2 - HISTVAL3)/HISTVAL3) * 100) %>%
  select(parcel_id, assval_chg)

pt <- left_join(p, t, by = 'parcel_id') %>%
  filter(grepl('101A|102A', parcel_id))

plt = get_brewer_pal('YlOrRd', n = 7)

## map appraisal value changes
taxmap <- tm_shape(pt) + 
  tm_fill('assval_chg', title = 'A)\nAssessed Value\nChange (%)',
          breaks = c(0,250,500,750,1000,5000), palette = plt) + 
  tm_borders(col = 'black') +
  # tm_scale_bar(breaks = c(0, 0.4), size = 0.8, position = c(0.65, 0)) + 
  # tm_compass(type = 'arrow', size = 3, position = c(0.71, 0.09)) +
  tm_layout(frame = FALSE,
           legend.text.size = 0.8,
           legend.title.size = 1)
taxmap 

tiff(file.path(datadir, 'figures/map_assval_chg.tiff'), res = 300, units = 'in',
     width = 5, height = 5)
taxmap
dev.off()

## map owners by category
prcl <- st_read(file.path(datadir, 'spatial-data/parcel-data-export/'))

map <- tm_shape(po2) + 
  tm_fill('own_cat', title = 'Owner Category') + 
  tm_borders(col = 'black') +
  tm_scale_bar(breaks = c(0, 0.4), size = 0.8, position = c(0.71, 0)) + 
  tm_compass(type = 'arrow', size = 3, position = c(0.77, 0.09)) + 
  tm_layout(frame = FALSE)
map 

tiff(file.path(datadir, 'figures/map_owner_category.tiff'), res = 300, units = 'in',
     width = 5, height = 5)
map
dev.off()

