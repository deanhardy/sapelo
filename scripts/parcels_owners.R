rm(list=ls())

library(tidyverse)
library(lubridate)
library(readxl)
library(sf)
library(tmap)

utm <- 2150 ## NAD83 17N

## define data directory
datadir <- 'C:/Users/dhardy/Dropbox/r_data/sapelo'

## import property owner data
o <- read.csv(file.path(datadir, 'property/owners_sapelo_master.csv'), stringsAsFactors = F) %>%
  mutate(own3cat = ifelse(own_cat %in% c('LLC', 'LLP', 'INC', 'Outsider'), 'Outsider',
                          ifelse(own_cat == 'NA', 'Descendant', own_cat)))

p <- st_read(file.path(datadir, 'property/parcels.shp'), stringsAsFactors = F) %>%
  st_transform(utm) %>%
  rename(parcel_id = PARCEL_ID)

p <- full_join(p, o, by = 'parcel_id')

new_o <- o %>%
  filter(!(parcel_id %in% p$parcel_id))
  
## summarize by owner categories
sum <- p %>%
  group_by(own3cat) %>%
  summarise(num = n(), acres = sum(gis_acres, na.rm = T)) %>%
  filter(!(own3cat %in% c(NA, 'County')))

## plot freq of land holdings by owner category

sumplot <- ggplot(sum, aes(own3cat, acres)) +
  geom_col(aes(fill = own3cat), show.legend = F, width = 0.5) + 
  # geom_text(aes(own3cat, acres+5), label = sum$num) + 
  labs(x = '', y = "Acres") + 
  scale_y_continuous(limits = c(0,200), expand = c(0,0)) +
  scale_fill_manual(values = c('grey10', 'grey30', 'grey70')) +
  theme(panel.background = element_rect(fill = 'white'),
        panel.grid = element_blank(),
        axis.line.y = element_line(color = 'black'),
        axis.text = element_text(color = 'black'),
        axis.ticks.x = element_line(colour = 'white'))
sumplot

tiff(file.path(datadir, "figures/owner_category_sums.tif"), height = 5, width = 5, unit = "in", 
     compression = "lzw", res = 300)
sumplot
dev.off()
