rm(list=ls())

## want to add search function, see here: https://stackoverflow.com/questions/37798690/search-button-for-leaflet-r-map

library(tidyverse)
library(sf)
library(leaflet)
library(leaflet.extras)

## define data directory
datadir <- '/Users/dhardy/Dropbox/r_data/sapelo'

## import parcel owner data and trans
df <- st_read(file.path(datadir, 'spatial-data/parcel_data_export/'), stringsAsFactors = F) %>%
  st_transform(4326) %>%
  mutate(owner = ifelse(is.na(owner), 'unknown', owner)) %>%
  filter(gis_acres != 'NA')

## filter out just companies
comp <- df %>%
  filter(own4cat == 'Company')

## import transactions data
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

## attach parcel owner data to transactions data
df2 <- left_join(df, latest_sales, by = 'parcel_id')

## import ag data
ag <- st_read(file.path(datadir, 'spatial-data/ag_plots/'), stringsAsFactors = F) %>%
  st_transform(4326)

ag_cntr <- st_centroid(ag)
  
## import water level data
hobo <- st_read(file.path(datadir, 'spatial-data/hobo_sites/'), stringsAsFactors = F) %>%
  st_transform(4326) %>%
  rename(site = Id)
info <- read.csv(file.path(datadir, 'water-level/datums.csv'), stringsAsFactors = F)
hobo2 <- left_join(hobo, info)

## last updated 3/31/2020 from Zillow 
## want to use API to pull info realtime with price etc.
forsale <- df2 %>% filter(parcel_id %in% c('0101A 0004003', '0101A 0019003', '0102A 0051', '0102A 0026')) %>%
  st_centroid() ## last one deduced from legal desc online, parcel id wrong

weblink <- '<a href=https://www.zillow.com/sapelo-island-ga/?searchQueryState={%22pagination%22:{},%22usersSearchTerm%22:%22Sapelo%20Island,%20GA%22,%22mapBounds%22:{%22west%22:-81.44322331103515,%22east%22:-81.04153568896484,%22south%22:31.34861701591248,%22north%22:31.562411869538245},%22mapZoom%22:12,%22savedSearchEnrollmentId%22:%22X1-SS0h1ut76czy191000000000_4hjdl%22,%22regionSelection%22:[{%22regionId%22:54314,%22regionType%22:6}],%22isMapVisible%22:true,%22filterState%22:{},%22isListVisible%22:true}>Zillow Sales Data</a>'

## define map variables
clr4 <- c('black', 'grey60', 'orange', 'white')
pal3 <- colorFactor(clr4, df$own3cat)
forsale_group <- paste('For Sale (updated ', format(Sys.Date(), format="%m/%d/%y"), ')', sep = '')
iconred <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = 'red'
)
iconblue <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'white',
  library = 'ion',
  markerColor = 'blue'
)
leafIcons <- icons(
  iconUrl = "http://leafletjs.com/examples/custom-icons/leaf-green.png",
  iconWidth = 38, iconHeight = 75,
  iconAnchorX = 22, iconAnchorY = 74
  # shadowUrl = "http://leafletjs.com/examples/custom-icons/leaf-shadow.png",
  # shadowWidth = 50, shadowHeight = 64,
  # shadowAnchorX = 4, shadowAnchorY = 62
)

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

ag_popup <- paste0(
  "<strong>PLOT INFO</strong>", "<br>",
  "Acres: ", round(ag$acres, 2), "<br>",
  "Square Feet: ", round(ag$sqft, 0))

hobo_popup <- paste0(
  "<strong>LOGGER INFO</strong>", "<br>",
  "Site #", hobo$site, "<br>",
  "Site Name: ", hobo$name, "<br>",
  "MLLW Elevation (ft): ", round(hobo$mllw_elvft, 2))

## generate interactive leaflet map
m <- leaflet() %>%
  addTiles(group = 'Open Street Map') %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Esri World Imagery") %>%
  setView(lng = -81.26, lat = 31.43, zoom = 15) %>%
  addAwesomeMarkers(data = forsale, 
             group = forsale_group,
             icon = iconred,
             popup = weblink) %>%
  # addMarkers(data = hobo, 
  #            group = 'Water Loggers') %>%
  addAwesomeMarkers(data = hobo,
                    group = 'Water Loggers',
                    popup = hobo_popup,
                    icon = iconblue) %>%
  addMarkers(data = ag_cntr,
             popup = ag_popup,
             group = 'Agriculture',
             icon = leafIcons) %>%
  addPolygons(data = ag,
              popup = ag_popup,
              group = 'Agriculture',
              fillColor = '#66ff00',
              fillOpacity = 0.8,
              weight = 1) %>%
  addPolylines(data = comp,
               color = "yellow",
               group = 'Companies',
               opacity = 1,
               weight = 3) %>%
  addPolygons(data = df,
              popup = parcel_popup,
              group = 'Parcels',
              fillColor = ~pal3(df$own3cat),
              fillOpacity = 0.8,
              weight = 1) %>%
  addLayersControl(baseGroups = c("Open Street Map", "Esri World Imagery"), 
                   overlayGroups = c("Parcels", "Companies", forsale_group, 'Agriculture', 'Water Loggers'),
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
  hideGroup(c(forsale_group, 'Companies', 'Agriculture', 'Water Loggers'))
m

library(htmlwidgets)
## exporting as html file for exploration
saveWidget(m, 
           file="/Users/dhardy/Dropbox/r_data/sapelo/hh_property_data.html",
           title = "Hog Hammock Data")

saveWidget(m, 
           file="/Users/dhardy/Dropbox/Sapelo_NSF/maps-gis/interactive-maps/hh_property_data.html",
           title = "Hog Hammock Data")


