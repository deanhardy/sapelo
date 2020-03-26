rm(list=ls())

library(tidyverse)
library(stringr)
library(lubridate)
library(readxl)
library(sf)
library(tmap)

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

p <- st_read(file.path(datadir, 'property/parcels.shp'), stringsAsFactors = F) %>%
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
sumplot <- ggplot(sum, aes(own3cat, acres * 0.404686)) +
  geom_col(fill = 'black', show.legend = F, width = 0.2) + 
  # geom_text(aes(own3cat, acres+5), label = sum$num) + 
  labs(x = '', y = "Hectares") + 
  scale_y_continuous(limits = c(0,80), expand = c(0,0)) +
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
sumplot2 <- ggplot(sum2, aes(reorder(own4cat, -acres), acres * 0.404686)) +
  geom_col(fill = 'black', show.legend = F, width = 0.3) +
  labs(x = '', y = "Hectares") + 
  scale_y_continuous(limits = c(0,80), expand = c(0,0)) +
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

## make interactive map of parcel owner data
library(leaflet)
library(leaflet.extras)
library(sf)

df <- st_transform(po2, 4326) %>%
  mutate(owner = ifelse(is.na(owner), 'unknown', owner)) %>%
  filter(gis_acres != 'NA')
pal4 <- colorFactor(rainbow(4), df$own4cat)

comp <- df %>%
  filter(own4cat == 'Company')
  
m <- leaflet() %>%
  addTiles(group = 'Open Street Map') %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Esri World Imagery") %>%
  setView(lng = -81.26, lat = 31.43, zoom = 14) %>%
  addPolygons(data = df,
              popup = paste("Parcel ID:", df$parcel_id, "<br>",
                            "Owner:", df$owner, "<br>",
                            "GIS Acres:", round(df$gis_acres, 1)),
              group = 'Parcels',
              fillColor = ~pal4(df$own4cat),
              fillOpacity = 0.5,
              weight = 1) %>%
  addLayersControl(baseGroups = c("Open Street Map", "Esri World Imagery"), 
                   overlayGroups = c("Parcels"),
                   options = layersControlOptions(collapsed = TRUE)) %>%
  addLegend("bottomright",
            pal = pal4,
            values = df$own4cat,
            title = "Owner Category") %>%
  addScaleBar("bottomright")
m

## exporting as html file for exploration
library(htmlwidgets)
saveWidget(m, 
           file="/Users/dhardy/Dropbox/r_data/sapelo/hh_parcels.html",
           title = "Hog Hammock Parcel Data")

# ## owner plus tax map combo, but must run "tax_map" script first
# <<<<<<< HEAD
# # tiff(file.path(datadir, 'figures/owner_tax_combo.tiff'), res = 300, units = 'in',
# #      width = 8, height = 5)
# # tmap_arrange(taxmap, map2, ncol = 2)
# # dev.off()
# =======
# tiff(file.path(datadir, 'figures/owner_tax_combo.tif'), res = 300, units = 'in',
#      width = 8, height = 5, compression = "lzw")
# tmap_arrange(taxmap, map2, ncol = 2)
# dev.off()
# >>>>>>> 47c05dd62499bdc8c2a0264813f1b3bf7aef38ee

ownsum <- po %>%
  group_by(own3cat) %>%
  summarise(n(), ha = sum(gis_acres * 0.404686, na.rm = T))

ownsum[[2,3]]/sum(ownsum$ha)
  
llcsum <- po %>%
  filter(own_cat %in% c('LLC', 'INC', 'LLP')) %>%
  group_by(owner) %>%
  summarise(n(), ha = sum(gis_acres * 0.404686, na.rm = T))

outsidersum <- po %>%
  filter(own3cat == 'Non-traditional') %>%
  group_by(owner) %>%
  summarise(n(), ha = sum(gis_acres* 0.404686, na.rm = T))

descendantsum <- po %>%
  filter(own3cat == 'Descendant') %>%
  group_by(owner) %>%
  summarise(n())

###########################
## want to add search function, see here: https://stackoverflow.com/questions/37798690/search-button-for-leaflet-r-map
## want to also add Zillow API with props listed for sale
###########################

sales <- read.csv(file.path(datadir, "property/transactions_sapelo_primary.csv"), stringsAsFactors = F) %>%
  mutate(date = as.Date(date, "%m/%d/%y")) %>%
  rename(parcel_id = parcel.id)

## select most recent sales for each property
latest_sales <- sales %>%
  group_by(parcel_id) %>%
  slice(which.max(date))

## select oldest sales for each property
oldest_sales <- sales %>%
  group_by(parcel_id) %>%
  slice(which.min(date))

df2 <- left_join(df, latest_sales, by = 'parcel_id')

clr4 <- c('black', 'grey60', 'orange', 'white')

pal3 <- colorFactor(clr4, df$own3cat)

## last updated 3/26/2020 from Zillow 
## want to use API to pull info realtime with price etc.
forsale <- df2 %>% filter(parcel_id %in% c('0101A 0004003', '0101A 0019003', '0102A 0051', '0102A 0026')) %>%
  st_centroid() ## last one deduced from legal desc online, parcel id wrong
  
weblink <- '<a href=https://www.zillow.com/sapelo-island-ga/?searchQueryState={%22pagination%22:{},%22usersSearchTerm%22:%22Sapelo%20Island,%20GA%22,%22mapBounds%22:{%22west%22:-81.44322331103515,%22east%22:-81.04153568896484,%22south%22:31.34861701591248,%22north%22:31.562411869538245},%22mapZoom%22:12,%22savedSearchEnrollmentId%22:%22X1-SS0h1ut76czy191000000000_4hjdl%22,%22regionSelection%22:[{%22regionId%22:54314,%22regionType%22:6}],%22isMapVisible%22:true,%22filterState%22:{},%22isListVisible%22:true}>Zillow Sales Data</a>'

parcel_popup <- paste0(
  "<strong>PARCEL INFO</strong>", "<br>",
  "Parcel ID:", df$parcel_id, "<br>",
  "Owner: ", df$owner, "<br>",
  "GIS Acres: ", round(df$gis_acres, 1),"<br>", "<br>",
  "<strong>MOST RECENT TRANSACTION</strong>", "<br>",
  "Date: ", df2$date, "<br>",
  "Grantor: ", df2$grantor, "<br>",
  "Grantee: ", df2$grantee, "<br>",
  "Price: $", df2$price, "<br>",
  "Price per Acre: $", df2$price.acre, "<br>",
  "Sale Type: ", df2$sale.type)

m <- leaflet() %>%
  addTiles(group = 'Open Street Map') %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Esri World Imagery") %>%
  setView(lng = -81.26, lat = 31.43, zoom = 15) %>%
  addPolylines(data = comp,
               color = "yellow",
               group = 'Companies',
               opacity = 1,
               weight = 3) %>%
  addMarkers(data = forsale, 
             group = 'For Sale (updated 3/25/20)',
             popup = weblink) %>%
  addPolygons(data = df,
              popup = parcel_popup,
              group = 'Parcels',
              fillColor = ~pal3(df$own3cat),
              fillOpacity = 0.8,
              weight = 1) %>%
  addLayersControl(baseGroups = c("Open Street Map", "Esri World Imagery"), 
                   overlayGroups = c("Parcels", "Companies", "For Sale (updated 3/25/20)"),
                   options = layersControlOptions(collapsed = FALSE)) %>%
  addLegend("bottomright",
            pal = pal3,
            group = 'Parcels',
            values = df$own3cat,
            title = "Owner Category") %>%
  # addLegend("bottomleft",
  #           color = blue,
  #           group = 'For Sale (updated 3/25/20)',
  #           title = "Properties For Sale") %>%
  addScaleBar("bottomright") %>%
  hideGroup(c('For Sale (updated 3/25/20)', 'Companies'))
m

## exporting as html file for exploration
saveWidget(m, 
           file="/Users/dhardy/Dropbox/r_data/sapelo/hh_property_data.html",
           title = "Hog Hammock Property Data")

saveWidget(m, 
           file="/Users/dhardy/Dropbox/Sapelo_NSF/maps-gis/interactive-maps/hh_property_data.html",
           title = "Hog Hammock Property Data")



#   list(sales$date)
# }
# 
# if(df$parcel_id == sales$parcel_id){
#   list(sales$date)


