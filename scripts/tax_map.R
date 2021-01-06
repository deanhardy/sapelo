rm(list=ls())

library(tidyverse)
library(sf)
library(tmap)
library(tmaptools)

utm <- 2150 ## NAD83 17N

## define data directory
datadir <- '/Users/dhardy/Dropbox/r_data/sapelo'

p <- st_read(file.path(datadir, 'spatial-data/prcl_id_sap2015'), stringsAsFactors = F) %>%
  st_transform(utm) %>%
  rename(parcel_id = PARCEL_ID)

t <- read.csv(file.path(datadir, 'property/taxes/parcels_assessed_values.txt')) %>%
  rename(parcel_id = PARCEL_ID) %>%
  mutate(assval_chg = ((HISTVAL2 - HISTVAL3)/HISTVAL3) * 100) %>%
  select(parcel_id, assval_chg)

pt <- left_join(p, t, by = 'parcel_id') %>%
  filter(grepl('101A|102A', parcel_id)) %>%
  filter(assval_chg >= 0 & !is.na(assval_chg) & assval_chg != 'Inf')

plt = get_brewer_pal('YlOrRd', n = 7)

## map appraisal value changes
taxmap <- tm_shape(pt) + 
  tm_fill('assval_chg', title = '(a)\nAssessed Value\nChange (%)',
          breaks = c(0,250,500,750,1000,5000), palette = plt) + 
  tm_borders(col = 'black') +
  # tm_scale_bar(breaks = c(0, 0.4), size = 0.8, position = c(0.65, 0)) + 
  # tm_compass(type = 'arrow', size = 3, position = c(0.71, 0.09)) +
  tm_layout(frame = FALSE,
           legend.text.size = 0.5,
           legend.title.size = 0.6)
taxmap 

tiff(file.path(datadir, 'figures/map_assval_chg.tiff'), res = 300, units = 'in',
     width = 2.5, height = 2.5)
taxmap
dev.off()

## map owners by category
prcl <- st_read(file.path(datadir, 'spatial-data/parcel_data_export/parcel_data.shp'), stringsAsFactors = F) %>%
  st_transform(4326) %>%
  mutate(owner = ifelse(is.na(owner), 'unknown', owner)) %>%
  filter(own4cat != 'County' & gis_acres != 'NA') %>%
  mutate(own3cat = ifelse(own3cat == 'Non-traditional', 'Outsider', own3cat))

clr3 <- c('grey20', 'grey60', 'grey90')

ownmap <- tm_shape(prcl) + 
  tm_fill('own3cat', title = '(b)\nOwner Category', palette = clr3) + 
  tm_borders(col = 'black') +
  tm_scale_bar(breaks = c(0, 0.25), size = 0.5, position = c(0.72, 0)) + 
  tm_compass(type = 'arrow', size = 1.5, position = c(0.73, 0.1)) + 
  tm_layout(frame = FALSE,
            legend.text.size = 0.5,
            legend.title.size = 0.6)
ownmap

tiff(file.path(datadir, 'figures/map_owner_category.tiff'), res = 300, units = 'in',
     width = 2.5, height = 2.5)
ownmap
dev.off()

tiff(file.path(datadir, 'figures/epe2020-figure4.tiff'), res = 300, units = 'in',
     width = 5, height = 2.5)
tmap_arrange(taxmap, ownmap, widths = c(0.5,0.5))
dev.off()
