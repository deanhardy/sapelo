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
clr5 <- c('grey30', 'grey60', 'grey90', 'white', '#BA0C2F')

## define data directory
datadir <- '/Users/dhardy/Dropbox/r_data/sapelo'

## import property owner data
o <- read.csv(file.path(datadir, 'property/owners_sapelo_primary.csv'), stringsAsFactors = F) %>%
  mutate(own3cat = ifelse(own_cat %in% c('LLC', 'LLP', 'INC', 'Non-descendant'), 'Non-descendant', own_cat)) %>%
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

## summarize state claims into classes
sum.st <- po %>%
  st_drop_geometry() %>%
  group_by(state_claim) %>%
  summarise(num = n(), acres = sum(gis_acres, na.rm = T))
sum.st

## plot acres of land holdings by owner 3 class category
own.acres <- ggplot(sum, aes(own3cat, acres)) +
  geom_col(fill = 'black', show.legend = F, width = 0.2) + 
  # geom_text(aes(own3cat, acres+5), label = sum$num) + 
  labs(x = '', y = "Acres") + 
  scale_y_continuous(limits = c(0,200), breaks = seq(0,200,20), expand = c(0,0)) +
  # scale_fill_manual(values = 'black') +
  theme(panel.background = element_rect(fill = 'white'),
        panel.grid = element_blank(),
        axis.line.y = element_line(color = 'black'),
        axis.line.y.right = element_line(color = 'black'),
        panel.grid.major.y = element_line(color = 'grey40'),
        panel.grid.minor.y = element_line(color = 'grey60'),
        axis.text = element_text(color = 'black'),
        axis.ticks.x = element_line(colour = 'white')) + 
  ggtitle(paste0('Printed'), Sys.Date())
own.acres

png(file.path(datadir, "figures/owners/owner3class_acres.png"), height = 5, width = 5, unit = "in", res = 150)
own.acres
dev.off()

## plot freq of land holdings by owner 3 class category
own.num <- ggplot(sum, aes(own3cat, num)) +
  geom_col(fill = 'black', show.legend = F, width = 0.2) + 
  # geom_text(aes(own3cat, acres+5), label = sum$num) + 
  labs(x = '', y = "Number Parcels") + 
  scale_y_continuous(limits = c(0,200), breaks = seq(0,200,20), expand = c(0,0)) +
  # scale_fill_manual(values = 'black') +
  theme(panel.background = element_rect(fill = 'white'),
        panel.grid = element_blank(),
        axis.line.y = element_line(color = 'black'),
        axis.line.y.right = element_line(color = 'black'),
        panel.grid.major.y = element_line(color = 'grey40'),
        panel.grid.minor.y = element_line(color = 'grey60'),
        axis.text = element_text(color = 'black'),
        axis.ticks.x = element_line(colour = 'white')) + 
  ggtitle(paste0('Printed'), Sys.Date())
own.num

png(file.path(datadir, "figures/owners/owner3class_num.png"), height = 5, width = 5, unit = "in", res = 150)
own.num
dev.off()

## summarize by owner 3 class categories no SIHA
sum2 <- po %>%
  group_by(own4cat) %>%
  st_drop_geometry() %>%
  summarise(num = n(), acres = sum(gis_acres, na.rm = T)) %>%
  filter(!(own4cat %in% c('County', 'Unknown', 'Heritage Authority'))) %>%
  mutate(own4cat = factor(own4cat, levels = c('Descendant', 'Non-descendant', 'Company'))) %>%
  mutate(percent = round(100 * acres/sum(acres), 0))

sum.colors <- c('#BA0C2F', 'grey30', 'grey90')
names(sum.colors) <- levels(sum2$own4cat)

## plot freq of land holdings by owner 3 class category with no SIHA
sumplot2 <- ggplot(sum2, aes(reorder(own4cat, -percent), percent)) +
  geom_col(fill = sum.colors, show.legend = F, width = 0.8) +
  labs(x = '', y = "Percent") + 
  scale_y_continuous(limits = c(0,100), expand = c(0,0), breaks = seq(0,100,10)) +
  theme(panel.background = element_rect(fill = 'white'),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(linetype = 'dashed'),
        plot.margin = margin(1, 0, 0, 0.5, "cm"),
        axis.line.y = element_line(color = 'black'),
        axis.line.x = element_line(color = 'black'),
        axis.text = element_text(color = 'black'),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank()
        )
sumplot2

tiff(file.path(datadir, "figures/owners/owner3category_sums_noSIHA.tif"), height = 3.9, width = 3.75, unit = "in", 
     compression = "lzw", res = 600)
sumplot2
dev.off()

attach(sum2)
sum3 <- sum2[order(percent),]
detach(sum2)

## pie chart of the above percent ownership categories
tiff(file.path(datadir, "figures/owners/owner3category_sums_noSIHA.tif"), height = 3.9, width = 3.75, unit = "in", 
     compression = "lzw", res = 600)
pie(sum3$percent, col = c('#BA0C2F', 'grey90', 'grey30'), labels = paste0(sum3$percent, '%'), 
    init.angle = 90, radius = 1, main = 'Percentage Acres Owned\nby Ownership Category')
dev.off()

# plot categorical proportion ownership
## https://r-graph-gallery.com/piechart-ggplot2.html
# Compute the position of labels
data <- sum2 %>% 
  arrange(desc(percent)) %>%
  mutate(prop = percent / sum(sum2$percent) *100) %>%
  mutate(ypos = cumsum(prop)- 0.1*prop )

prop.colors <- c('grey30', 'grey60', 'grey90', '#BA0C2F')
names(prop.colors) <- levels(data$own4cat)

# propplot <- 
#   ggplot(data, aes(x="", y=acres)) +
#   geom_bar(fill=prop.colors, stat="identity", width=1, show.legend = F) +
#   coord_polar("y", start=0) + 
#   theme_void() + 
#   geom_text(aes(y = ypos, label = percent), color = "white", size=6) +
#   scale_fill_brewer(palette= prop.colors)
# propplot
# 
# tiff(file.path(datadir, "figures/owner4category_prop.tif"), height = 3.9, width = 3.75, unit = "in", 
#      compression = "lzw", res = 600)
# propplot
# dev.off()


# new_o <- o %>% group_by(owner) %>%
#   summarise(table(owner), own_cat = first(own_cat)) %>%
#   mutate(own_cat_freq)

# who <- po %>%
#   filter(notes == 'guessed own category based on sales') %>%
#   st_centroid('geometry')

po2 <- po %>% 
  mutate(own3cat = ifelse(own3cat %in% c('Unknown', 'County'), 'Other', own3cat)) %>%
  mutate(own3cat = as_factor(own3cat)) 

## remove empty geometries
po3 <- po2 %>% filter(!st_is_empty(.)) %>%
  mutate(own4cat = if_else(own3cat == 'Other', 'Other', own4cat)) %>%
  mutate(own4cat = factor(own4cat, levels = c('Descendant', 'Heritage Authority', 'Non-descendant', 'Company', 'Other')))

st_write(po3, file.path(datadir, 'spatial-data/parcel_data_export/parcel_data.geojson'), 'GEOJSON', delete_dsn=TRUE)
st_write(po3, file.path(datadir, 'spatial-data/parcel_data_export/parcel_data.shp'), 'ESRI Shapefile', delete_dsn=TRUE)

## map owners by category
map <- tm_shape(po3) + 
  tm_fill('own_cat', title = 'Owner Category') + 
  tm_borders(col = 'black') +
  tm_scale_bar(breaks = c(0, 0.4), size = 0.8, position = c(0.71, 0)) + 
  tm_compass(type = 'arrow', size = 3, position = c(0.77, 0.09)) + 
  tm_layout(frame = FALSE)
map 

tiff(file.path(datadir, 'figures/owners/map_owner_category.tiff'), res = 300, units = 'in',
     width = 5, height = 5)
map
dev.off()

map2.colors <- c('grey30', 'grey60', 'grey90', '#BA0C2F', 'black')
names(map2.colors) <- levels(po3$own4cat)

## map owners by 4 class category
map2 <- tm_shape(po3) + 
  tm_fill('own4cat', palette = map2.colors, title = 'Owner Category') + 
  tm_borders(col = 'black') +
  # tm_shape(filter(po3, own4cat == 'Company')) +
  # tm_fill(col = 'red', lwd = 1) +
  tm_compass(type = 'arrow', size = 3, position = c(0.75, 0.09)) +
  tm_layout(frame = FALSE,
            title = 'Hog Hammock Community Ownership',
            title.position = c('center', "TOP"),
            legend.text.size = 0.8,
            legend.title.size = 1,
            legend.position = c(0.01,0.675))
map2

tiff(file.path(datadir, 'figures/owners/map_owner4category.tiff'), res = 600, units = 'in',
    width = 3.75, height = 3.9)
map2
dev.off()


ownsum <- po %>%
  group_by(own3cat) %>%
  summarise(n(), ac = sum(gis_acres, na.rm = T), ha = sum(gis_acres * 0.404686, na.rm = T))

## descendant hectares percentage
ownsum[[2,3]]/sum(ownsum$ha)
  
llcsum <- po %>%
  filter(own_cat %in% c('LLC', 'INC', 'LLP')) %>%
  group_by(owner) %>%
  summarise(n(), ha = sum(gis_acres * 0.404686, na.rm = T))
sum(llcsum$`n()`)
sum(llcsum$ha)/sum(ownsum$ha)

outsidersum <- po %>%
  filter(own3cat == 'Non-descendant') %>%
  group_by(owner) %>%
  summarise(n(), ha = sum(gis_acres* 0.404686, na.rm = T))
sum(outsidersum$`n()`)

descendantsum <- po %>%
  filter(own3cat == 'Descendant') %>%
  group_by(owner) %>%
  summarise(n(), ha = sum(gis_acres* 0.404686, na.rm = T))
sum(descendantsum$`n()`)

# heirs <- po %>%
#   filter(own3cat == 'Descendant') %>%
#   filter(str_detect(owner, c('EST', 'ETAL', 'C/O')))


#######################
## TAX HIKE EVAL
#######################

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
  tm_fill('assval_chg', title = 'B)\n2011 Assessed Value\nChange (%)',
          breaks = c(0,250,500,750,1000,5000), palette = plt) + 
  tm_borders(col = 'black') +
  # tm_scale_bar(breaks = c(0, 0.4), size = 0.8, position = c(0.65, 0)) + 
  # tm_compass(type = 'arrow', size = 3, position = c(0.71, 0.09)) +
  tm_layout(frame = FALSE,
            legend.text.size = 0.8,
            legend.title.size = 1)
taxmap 


## export combined figures
# tiff(file.path(datadir, 'figures/hh-taxhike-ownercat-rb02.tiff'), res = 600, units = 'in',
#      width = 7.5, height = 3.9)
# tmap_arrange(map2, sumplot2, widths = c(0.5,0.5))
# dev.off()
