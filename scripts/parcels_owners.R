rm(list=ls())

library(tidyverse)
library(lubridate)
library(readxl)
library(sf)
library(tmap)

utm <- 2150 ## NAD83 17N
clr3 <- c('grey30', 'grey60', 'grey90')
clr4 <- c('grey30', 'grey60', 'grey90', 'black')

## define data directory
datadir <- 'C:/Users/dhardy/Dropbox/r_data/sapelo'

## import property owner data
o <- read.csv(file.path(datadir, 'property/owners_sapelo_master.csv'), stringsAsFactors = F) %>%
  mutate(own3cat = ifelse(own_cat %in% c('LLC', 'LLP', 'INC', 'Outsider'), 'Outsider', own_cat))

p <- st_read(file.path(datadir, 'property/parcels.shp'), stringsAsFactors = F) %>%
  st_transform(utm) %>%
  rename(parcel_id = PARCEL_ID)

# as.data.frame(table(unique(p$parcel_id)))

po <- full_join(p, o, by = 'parcel_id') %>%
  mutate(gis_acres = 
           ifelse(parcel_id == '0102A  0134002', 1.21, 
                  ifelse(parcel_id == '0102A  0134', gis_acres - 1.21,
                         ifelse(parcel_id == '0102A  0090', 1.15, 
                                ifelse(parcel_id == '0101A  0068001', 1, 
                                       ifelse(parcel_id == '0101A  0106001', 0.25,
                                              ifelse(parcel_id == '0102A 0025', 1,
                                                     ifelse(parcel_id == '0102A  0045001', 2,
                                ifelse(parcel_id %in% c('0101A  0071', '0101A  0071001', '0101A  0071002', '0101A  0071003', '0101A  0071004'), 0.4, gis_acres))))))))) %>%
  mutate(own_cat = ifelse(is.na(own_cat), 'Unknown', own_cat)) %>%
  mutate(own3cat = ifelse(is.na(own3cat), 'Unknown', own3cat))

# new_o <- o %>%
#   filter(!(parcel_id %in% p$parcel_id))
  
## summarize by owner categories
sum <- po %>%
  group_by(own3cat) %>%
  summarise(num = n(), acres = sum(gis_acres, na.rm = T)) %>%
  filter(!(own3cat %in% c('County', 'Unknown')))

## plot freq of land holdings by owner category

sumplot <- ggplot(sum, aes(own3cat, acres)) +
  geom_col(aes(fill = own3cat), show.legend = F, width = 0.5) + 
  # geom_text(aes(own3cat, acres+5), label = sum$num) + 
  labs(x = '', y = "Acres") + 
  scale_y_continuous(limits = c(0,200), expand = c(0,0)) +
  scale_fill_manual(values = clr3) +
  theme(panel.background = element_rect(fill = 'white'),
        panel.grid = element_blank(),
        axis.line.y = element_line(color = 'black'),
        axis.text = element_text(color = 'black'),
        axis.ticks.x = element_line(colour = 'white'))
sumplot

tiff(file.path(datadir, "figures/owner3category_sums.tif"), height = 5, width = 5, unit = "in", 
     compression = "lzw", res = 300)
sumplot
dev.off()

# new_o <- o %>% group_by(owner) %>%
#   summarise(table(owner), own_cat = first(own_cat)) %>%
#   mutate(own_cat_freq)

# who <- po %>%
#   filter(notes == 'guessed own category based on sales') %>%
#   st_centroid('geometry')

po2 <- po %>% 
  mutate(own3cat = ifelse(own3cat %in% c('Unknown', 'County'), 'Other', own3cat)) %>%
  mutate(own3cat = as_factor(own3cat))

## map owners by category
map <- tm_shape(po2, unit = 'miles') + 
  tm_fill('own3cat', palette = clr5, title = 'Owner Category') + 
  tm_borders(col = 'black') +
  tm_scale_bar(breaks = c(0, 0.2), size = 0.8, position = c(0.71, 0)) + 
  tm_compass(type = 'arrow', size = 3, position = c(0.75, 0.09)) + 
  tm_layout(frame = FALSE)
# map 

tiff(file.path(datadir, 'figures/map_owner3category.tiff'), res = 300, units = 'in',
    width = 5, height = 5)
map
dev.off()

ownsum <- po %>%
  group_by(own3cat) %>%
  summarise(n())

llcsum <- po %>%
  filter(own_cat %in% c('LLC', 'INC', 'LLP')) %>%
  group_by(owner) %>%
  summarise(n(), acres = sum(gis_acres, na.rm = T))

outsidersum <- po %>%
  filter(own3cat == 'Outsider') %>%
  group_by(owner) %>%
  summarise(n(), acres = sum(gis_acres, na.rm = T))

descendantsum <- po %>%
  filter(own3cat == 'Descendant') %>%
  group_by(owner) %>%
  summarise(n())
