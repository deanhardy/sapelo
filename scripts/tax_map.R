rm(list=ls())

library(tidyverse)
library(sf)
library(tmap)
library(tmaptools)

utm <- 2150 ## NAD83 17N

## define data directory
datadir <- 'C:/Users/dhardy/Dropbox/r_data/sapelo'

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
map <- tm_shape(pt) + 
  tm_fill('assval_chg', title = 'Assessed Value Change (%)',
          breaks = c(0,200,400,600,800,1000, 5000), palette = plt) + 
  tm_borders(col = 'black') +
  tm_scale_bar(breaks = c(0, 0.4), size = 1.5, position = c(0.68, 0)) + 
  tm_compass(type = 'arrow', size =5, position = c(0.76, 0.1)) + 
  tm_layout(frame = FALSE,
            legend.text.size = 1.5,
            legend.title.size = 1.5)
map 

tiff(file.path(datadir, 'figures/map_assval_chg.tiff'), res = 300, units = 'in',
     width = 7.5, height = 7.5)
map
dev.off()
