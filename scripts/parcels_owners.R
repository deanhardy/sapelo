rm(list=ls())

library(tidyverse)
library(stringr)
library(lubridate)
library(readxl)
library(sf)
library(tmap)

## https://stat.ethz.ch/pipermail/r-sig-mac/2020-November/013783.html

utm <- 2150 ## NAD83 17N
clr3 <- c('grey30', 'grey60', 'grey90')
clr4 <- c('grey30', 'grey60', 'grey90', 'grey10')
clr5 <- c('black', 'grey60', 'red', 'grey10')


## define data directory
datadir <- '/Users/dhardy/Dropbox/r_data/sapelo'

## import property owner data
o <- read.csv(file.path(datadir, 'property/owners_sapelo_primary.csv'), stringsAsFactors = F) %>%
  mutate(own3cat = ifelse(own_cat %in% c('LLC', 'LLP', 'INC', 'Non-traditional'), 'Non-traditional', own_cat)) %>%
  mutate(own4cat = ifelse(own_cat %in% c('LLC', 'LLP', 'INC'), 'Company', own_cat))

p <- st_read(file.path(datadir, 'spatial-data/parcels/'), stringsAsFactors = F) %>%
  st_transform(utm) %>%
  rename(parcel_id = PARCEL_ID)
p$parcel_id <- str_squish(p$parcel_id)

# as.data.frame(table(unique(p$parcel_id)))

## manual entry of acreage for those without shapefile boundary
po <- full_join(p, o, by = 'parcel_id') %>%
  mutate(gis_acres = 
           ifelse(parcel_id == '0102A 0134002', 1.21, 
                  ifelse(parcel_id == '0102A 0134', gis_acres - 1.21,
                         ifelse(parcel_id == '0102A 0090', 1.15, 
                                ifelse(parcel_id == '0101A 0068001', 1, 
                                       ifelse(parcel_id == '0101A 0106001', 0.25,
                                              ifelse(parcel_id == '0102A 0025', 1,
                                                     ifelse(parcel_id == '0102A 0045001', 2,
                                ifelse(parcel_id %in% c('0101A 0071', '0101A 0071001', '0101A 0071002', '0101A 0071003', '0101A 0071004'), 0.4, gis_acres))))))))) %>%
  mutate(own_cat = ifelse(is.na(own_cat), 'Unknown', own_cat)) %>%
  mutate(own3cat = ifelse(is.na(own3cat), 'Unknown', own3cat)) %>%
  mutate(own4cat = ifelse(is.na(own4cat), 'Unknown', own4cat))

# new_o <- o %>%
#   filter(!(parcel_id %in% p$parcel_id))
  
## summarize by owner 3 class categories
sum <- po %>%
  group_by(own3cat) %>%
  summarise(num = n(), acres = sum(gis_acres, na.rm = T)) %>%
  filter(!(own3cat %in% c('County', 'Unknown')))

## plot freq of land holdings by owner 3 class category
sumplot <- ggplot(sum, aes(own3cat, acres)) +
  geom_col(fill = 'black', show.legend = F, width = 0.2) + 
  # geom_text(aes(own3cat, acres+5), label = sum$num) + 
  labs(x = '', y = "Acres") + 
  scale_y_continuous(limits = c(0,200), expand = c(0,0)) +
  # scale_fill_manual(values = 'black') +
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


## summarize by owner 4 class categories
sum2 <- po %>%
  group_by(own4cat) %>%
  summarise(num = n(), acres = sum(gis_acres, na.rm = T)) %>%
  filter(!(own4cat %in% c('County', 'Unknown')))

## plot freq of land holdings by owner 4 class category
sumplot2 <- ggplot(sum2, aes(reorder(own4cat, -acres), acres)) +
  geom_col(fill = 'black', show.legend = F, width = 0.3) +
  labs(x = 'Owner Category', y = "Acres") + 
  scale_y_continuous(limits = c(0,200), expand = c(0,0)) +
  scale_fill_manual(values = clr4) +
  theme(panel.background = element_rect(fill = 'white'),
        panel.grid = element_blank(),
        axis.line.y = element_line(color = 'black'),
        axis.text = element_text(color = 'black'),
        axis.ticks.x = element_line(colour = 'white'))
sumplot2

tiff(file.path(datadir, "figures/owner4category_sums.tif"), height = 5, width = 5, unit = "in", 
     compression = "lzw", res = 300)
sumplot2
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

st_write(po2, file.path(datadir, 'spatial-data/parcel_data_export/parcel_data.shp'), 'ESRI Shapefile', delete_dsn=TRUE)

## map owners by category
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

## map owners by 3 class category
map2 <- tm_shape(po2) + 
  tm_fill('own3cat', palette = clr4, title = 'B)\nOwner Category') + 
  tm_borders(col = 'black') +
  tm_scale_bar(breaks = c(0,0.5), size = 0.7, position = c(0.65, 0)) + 
  tm_compass(type = 'arrow', size = 3, position = c(0.72, 0.09)) +
  tm_layout(frame = FALSE,
            legend.text.size = 0.8,
            legend.title.size = 1)
map2

tiff(file.path(datadir, 'figures/map_owner3category.tiff'), res = 300, units = 'in',
    width = 5, height = 5)
map2
dev.off()


## map owners by 3 class category with companies highlighted
map3 <- tm_shape(po2) + 
  tm_fill('own3cat', palette = clr5, title = 'Owner Category') + 
  tm_borders(col = 'black') +
  tm_scale_bar(breaks = c(0,0.5), size = 0.7, position = c(0.65, 0)) + 
  tm_compass(type = 'arrow', size = 3, position = c(0.72, 0.09)) +
  tm_layout(frame = FALSE,
            legend.text.size = 0.8,
            legend.title.size = 1)
map3

tiff(file.path(datadir, 'figures/map_owner4category_llc-highlighted.tiff'), res = 300, units = 'in',
     width = 5, height = 5)
map3
dev.off()

ownsum <- po %>%
  group_by(own3cat) %>%
  summarise(n(), ha = sum(gis_acres * 0.404686, na.rm = T))

## descendant hectares percentage
ownsum[[2,3]]/sum(ownsum$ha)
  
llcsum <- po %>%
  filter(own_cat %in% c('LLC', 'INC', 'LLP')) %>%
  group_by(owner) %>%
  summarise(n(), ha = sum(gis_acres * 0.404686, na.rm = T))
sum(llcsum$`n()`)
sum(llcsum$ha)/sum(ownsum$ha)

outsidersum <- po %>%
  filter(own3cat == 'Non-traditional') %>%
  group_by(owner) %>%
  summarise(n(), ha = sum(gis_acres* 0.404686, na.rm = T))

descendantsum <- po %>%
  filter(own3cat == 'Descendant') %>%
  group_by(owner) %>%
  summarise(n())
sum(descendantsum$`n()`)

heirs <- po %>%
  filter(own3cat == 'Descendant') %>%
  filter(str_detect(owner, c('EST', 'ETAL', 'C/O')))


