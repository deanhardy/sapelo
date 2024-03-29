rm(list=ls())

## want to add search function, see here: https://stackoverflow.com/questions/37798690/search-button-for-leaflet-r-map
## Useful info: https://info.courthousedirect.com/blog/bid/366511/how-to-recognize-a-grantor-grantee-on-a-legal-document

library(tidyverse)
library(sf)
library(leaflet)
library(leaflet.extras)
library(raster)
library(lubridate)
library(readxl)
library(htmlwidgets)

# devtools::install_github("statnmap/HatchedPolygons")

## define data directory
datadir <- '/Users/dhardy/Dropbox/r_data/sapelo'
## hobo data info

# datasets <- readxl_example("/Users/dhardy/Library/CloudStorage/Dropbox/Sapelo_NSF/water_level_survey/data/sapelo-water-level-survey.xlsx")

# ## updated 4/20/20
# zillow_listings <- c('0102A 0095001',
#                      '0101A 0019003', 
#                      '0102A 0051',
#                      '0102A 0029',
#                      '0101A 0004003',
#                      '0102A 0095',
#                      '0102A 0134001')
# ## for implementing zillow scraping later
# ## https://github.com/notesofdabbler/blog_notesofdabbler/blob/master/learn_rvest/exploreZillow_w_rvest.R
# ## http://thatdatatho.com/2018/12/14/an-introduction-to-scraping-real-estate-data-with-rvest-and-rselenium/


## import parcel owner data and trans
df <- st_read(file.path(datadir, 'spatial-data/parcel_data_export/parcel_data.geojson'), stringsAsFactors = F) %>%
  st_transform(4326) %>%
  mutate(owner = ifelse(is.na(owner), 'unknown', owner)) %>%
  filter(gis_acres != 'NA') %>%
  mutate(own3cat = if_else(own3cat == 'Outsider', 'Non-traditional', 
                           if_else(own3cat == 'Other', 'County', own3cat))) %>%
  mutate(own3cat = fct_relevel(own3cat, c('Descendant', 'Heritage Authority', 'Non-traditional', 'County'))) %>%
  arrange(own3cat)

## likely heirs
heirs <- df %>%
  filter(own3cat == 'Descendant') %>%
  filter(str_detect(owner, c('EST|ETAL|C/O')))

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

## select most recent sales for each property
latest_cashsales <- sales %>%
  filter(price > 0) %>%
  group_by(parcel_id) %>%
  slice(which.max(date))
  # arrange(desc(date), .by_group = TRUE) %>%
  # filter(first(date)) %>%
  # dplyr::select(parcel_id, grantee)

## strip all but parcel id from owner data (ie df)
sp <- df %>% dplyr::select(parcel_id)

## join latest_cashsales with spatial data
sp_cashsales <- left_join(sp, latest_cashsales, by = "parcel_id") %>%
  # filter(!(is.na(date))) %>%
  mutate(year = year(date))

## used to double check current owners are correct, but update needs to be manual bc so many 
## "differences" are due to capitalization or spelling, not actual diff owner
# owner.diff <- left_join(df, latest_cashsales, by = "parcel_id") %>%
#   mutate(owner2 = if_else(owner == grantee, owner, grantee, missing = owner)) %>%
#   filter(grantee != owner)

## filter out just companies
comp <- df %>%
  filter(own4cat == 'Company')

## attach parcel owner data to transactions data
df2 <- left_join(df, latest_sales, by = 'parcel_id')

## wanting to add all sales data to map
## https://stackoverflow.com/questions/49938532/r-possible-to-create-a-leaflet-map-and-a-rendering-table-without-shiny
sales.spatial <- left_join(df, sales, by = 'parcel_id') %>%
  group_by(parcel_id) %>%
  dplyr::select(parcel_id, date:geometry) %>%
  arrange(date, .by_group = TRUE)

## create centroids for search feature
df2_cntrd <- st_centroid(df2)
  
## import title search data and adjoin to spatial data and hatch
title <- read.csv(file.path(datadir, 'property/title_search_outsiders.csv'), stringsAsFactors = F) %>%
  rename(parcel_id = parcel.id) %>%
  mutate(date = as.Date(date, format = '%m/%d/%y'),
         parcel_id = str_replace(parcel_id, '  ', ' ')) %>%
  group_by(parcel_id) %>%
  slice(which.max(date)) %>%
  dplyr::select(status) 

df3 <- left_join(df2, title, by = 'parcel_id') %>%
  filter(status != 'NA') 

## https://gist.github.com/johnbaums/c6a1cb61b8b6616143538950e6ec34aa
hatch <- function(x, density) {
  # x: polygon object (SpatialPolgyons* or sf)
  # density: approx number of lines to plot
  require(sp)
  require(raster)
  e <- extent(x)
  w <- diff(e[1:2])
  x1 <- seq(xmin(e), xmax(e)+w, length.out=floor(density*2))
  x0 <- seq(xmin(e)-w, xmax(e), length.out=floor(density*2))
  y0 <- rep(ymin(e), floor(density*2))
  y1 <- rep(ymax(e), floor(density*2))
  ll <- spLines(mapply(function(x0, y0, x1, y1) {
    rbind(c(x0, y0), c(x1, y1))
  }, x0, y0, x1, y1, 
  SIMPLIFY=FALSE))  
  if(is(x, 'sf')) {
    require(sf)
    ll <- st_as_sf(ll)
    st_crs(ll) <- st_crs(x)
    st_intersection(ll, x)
  } else {
    proj4string(ll) <- proj4string(x)
    raster::intersect(ll, x)
  }
}

df3.hatch <- hatch(df3, 60)

# den <- rep(5, length(df3))
# ang <- rep(45, length(df3))
# df3.hatch <- HatchedPolygons::hatched.SpatialPolygons(df3, density = den, angle = ang)

## import ag data
ag <- st_read(file.path(datadir, 'spatial-data/ag_plots/'), stringsAsFactors = F)[-1,] %>%
  st_transform(4326)

ag_cntr <- st_centroid(ag)
  
## import water level data
hobo <- st_read(file.path(datadir, 'spatial-data/hobo_sites/hobo_sites2.shp'), stringsAsFactors = F) %>%
  st_transform(4326) %>%
  mutate(site = as.numeric(site)) %>%
  dplyr::select(site)
#   rename(site = Id)
# info <- read.csv(file.path(datadir, 'water-level/site-elevations.csv'), stringsAsFactors = F) %>%
#   mutate(install_date = as.Date(install.date, '%m/%d/%y'),
#          site = as.numeric(site)) %>%
#   dplyr::select(site, install_date)
hobo.info <- read_xlsx(file.path(datadir, 'sapelo-water-level-survey.xlsx'), 
                       sheet = 'site info') %>%
  rename(site = SiteID) %>%
  mutate(site = as.numeric(site))

hobo2 <- left_join(hobo, hobo.info)

## read in zillow data
zdata <- read.csv(file.path(datadir, 'zdata.csv'), stringsAsFactors = F)[-1]

matches <- regmatches(zdata$date, gregexpr("[[:digit:]]+", zdata$date))
# unlist(matches)

unlist(map(matches, c(2,1)))

##import inundation data
inund <- st_read(file.path(datadir, 'spatial-data/inundation/inund2100hc_poly.shp'), stringsAsFactors = F) %>%
  st_transform(4326) %>%
  filter(prb_smplfy != '1%') %>%
  mutate(prb_smplfy = ifelse(prblty %in% c('50%', '95%'), '50%', '5%'))

forsale <- merge(df2, zdata, by = "parcel_id") %>%
  rename(saledate = date.y, saleprice = price.y) %>%
  mutate(link = paste0('<a href=', link, ' target=_blank>Link to Zillow</a>')) %>%
  st_centroid() %>%
  dplyr::select(parcel_id, addr, saledate, saleprice, link, geometry)

# weblink <- '<a href=https://www.zillow.com/sapelo-island-ga/?searchQueryState={%22pagination%22:{},%22usersSearchTerm%22:%22Sapelo%20Island,%20GA%22,%22mapBounds%22:{%22west%22:-81.44322331103515,%22east%22:-81.04153568896484,%22south%22:31.34861701591248,%22north%22:31.562411869538245},%22mapZoom%22:12,%22savedSearchEnrollmentId%22:%22X1-SS0h1ut76czy191000000000_4hjdl%22,%22regionSelection%22:[{%22regionId%22:54314,%22regionType%22:6}],%22isMapVisible%22:true,%22filterState%22:{},%22isListVisible%22:true}>Zillow Sales Data</a>'

## import 1938 map
# dem <- raster(x = file.path(datadir, "spatial-data/sapelo_hog_hammock/sapelo_hog_hammock.tif"))
# GDALinfo(file.path(datadir, "spatial-data/sapelo_hog_hammock/sapelo_hog_hammock.tif"))

## define map variables
# clr4 <- c('black', 'grey60', 'orange', 'white')
clr4 <- c('grey30', 'grey60', 'grey90','black')
pal3 <- colorFactor(clr4, df$own3cat)
pal4 <- colorFactor("RdYlBu", sp_cashsales$year)
clr3 <- c('green', 'yellow', 'red')
clr.ind <- c('#00a9e6', '#004c73')
pal5 <- colorFactor(clr.ind, inund$prb_smplfy)
tit3 <- colorFactor(clr3, df3$status)
tit3.h <- colorFactor(clr3, df3.hatch$status)
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
  "<i>Search Parcel ID in Search Bar</i>", "<br>",
  "Parcel ID: ", df$parcel_id, "<br>",
  "Owner: ", df$owner, "<br>",
  "GIS Acres: ", round(df$gis_acres, 1),"<br>", "<br>",
  "<strong>MOST RECENT TRANSACTION</strong>", "<br>",
  "Date: ", df2$date, "<br>",
  "Grantor: ", df2$grantor, "<br>",
  "Grantee: ", df2$grantee, "<br>",
  "Price: $", df2$price, "<br>",
  "Price per Acre: $", df2$price.acre, "<br>",
  "Sale Type: ", df2$sale.type)

sales_popup <- paste0(
  "<strong>SALE INFO</strong>", "<br>",
  # "<i>Search Parcel ID in Search Bar</i>", "<br>",
  "Parcel ID: ", sp_cashsales$parcel_id, "<br>",
  # "Owner: ", df$owner, "<br>",
  "GIS Acres: ", round(df$gis_acres, 1),"<br>", "<br>",
  "Date: ", sp_cashsales$date, "<br>",
  "Grantor: ", sp_cashsales$grantor, "<br>",
  "Grantee: ", sp_cashsales$grantee, "<br>",
  "Price: $",sp_cashsales$price, "<br>",
  "Price per Acre: $", sp_cashsales$price.acre, "<br>",
  "Sale Year: ", sp_cashsales$year, "<br>",
  "Sale Price: $", sp_cashsales$price)

z_popup <- paste0(
  "<strong>LISTING INFO</strong>", "<br>",
  "Listing Date: ", forsale$saledate, "<br>",
  "Sale Price: ", forsale$saleprice, "<br>",
  "Zillow Link: ", forsale$link)

ag_popup <- paste0(
  "<strong>PLOT INFO</strong>", "<br>",
  "Acres: ", round(ag$acres, 2), "<br>",
  "Square Feet: ", round(ag$sqft, 0))

# file.img <- 'site-pictures/site02-snagtree.JPG'

hobo_popup <- paste0(
  paste0("<img src = ", hobo2$Image, ">"), "<br>",
  "<strong>LOGGER INFO</strong>", "<br>",
  "Transect-Site#: ", hobo2$TransectSite, "<br>",
  "Site Name: ", hobo2$SiteName, "<br>",
  "Site Height (m NAVD88): ", hobo2$`Height(m_NAVD88)`, "<br>",
  "Install Date: ", hobo2$Installed, "<br>",
  "Active? ", hobo2$Active, "<br>",
  "Days Deployed: ", hobo2$DaysDeployed
  )

targetGroups <- c('Parcels')

library(htmlTable)

## generate interactive leaflet map
m <- leaflet() %>%
  addTiles(group = 'Open Street Map') %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Esri World Imagery") %>%
  setView(lng = -81.26, lat = 31.43, zoom = 15) %>%
  # addAwesomeMarkers(data = forsale, 
  #            group = forsale_group,
  #            icon = iconred,
  #            popup = z_popup) %>%
  addAwesomeMarkers(data = hobo,
                    group = 'Water Loggers',
                    popup = hobo_popup,
                    icon = iconblue) %>%
  # addMarkers(data = ag_cntr,
  #            popup = ag_popup,
  #            group = 'Agriculture',
  #            icon = leafIcons) %>%
  addMarkers(data = df2_cntrd,
             group = 'Parcels_Cntrd',
             label = df2_cntrd$parcel_id) %>%
  # addPolygons(data = ag,
  #             popup = ag_popup,
  #             group = 'Agriculture',
  #             fillColor = '#66ff00',
  #             color = 'black', 
  #             fillOpacity = 0.8,
  #             weight = 1) %>%
  addPolygons(data = comp,
               fillColor = "#BA0C2F",
               group = 'Companies',
               fillOpacity = 1,
               weight = 0) %>%
  addPolygons(data = heirs,
               fillColor = "yellow",
               group = 'Heirs',
               fillOpacity = 0.8,
               weight = 1) %>%
  # addPolylines(data = df3,
  #              color = ~tit3(df3$status),
  #              group = 'Title Search Status',
  #              opacity = 1,
  #              weight = 3) %>%
  # addPolylines(data = df3.hatch,
  #              color = ~tit3(df3.hatch$status),
  #              group = 'Title Search Status',
  #              opacity = 1,
  #              weight = 3) %>%
  addPolygons(data = df,
              popup = parcel_popup,
              # stroke = F,
              color = 'black',
              group = 'Parcels',
              fillColor = ~pal3(df$own3cat),
              fillOpacity = 1,
              weight = 1) %>%
  # addPolygons(data = inund,
  #             group = 'Inundation',
  #             fillColor = ~pal5(inund$prb_smplfy),
  #             fillOpacity = 0.8,
  #             weight = 1) %>%
  addPolygons(data = sp_cashsales,
              popup = sales_popup,
              color = 'black',
              group = 'Latest Sales',
              fillColor = ~pal4(sp_cashsales$year),
              fillOpacity = 1,
              weight = 1) %>%
  addLayersControl(baseGroups = c("Open Street Map", "Esri World Imagery"), 
                   # overlayGroups = c("Parcels", "Title Search Status", "Companies", 'Latest Sales',  'Agriculture', 'Water Loggers', 'Inundation'),
                   overlayGroups = c("Parcels", "Heirs", "Companies", 'Latest Sales', 'Water Loggers'),
                   options = layersControlOptions(collapsed = FALSE)) %>%
  addLegend("bottomright",
            pal = pal3,
            group = 'Parcels',
            values = df$own3cat,
            opacity = 1.0,
            title = "Owner Category") %>%
  # addLegend("bottomleft",
  #           pal = tit3,
  #           group = 'Title Search Status',
  #           values = df3$status,
  #           title = "Title Search Status") %>%
  addLegend("bottomleft",
            pal = pal4,
            group = 'Latest Sales',
            values = sp_cashsales$year,
            title = "Latest Sale Year") %>%
  # addLegend("bottomleft",
  #           pal = pal5,
  #           group = 'Inundation',
  #           values = inund$prb_smplfy,
  #           title = "Inundation") %>%
  addSearchFeatures(targetGroups = 'Parcels_Cntrd', 
                    options = searchFeaturesOptions(propertyName = "label",
                                                    zoom = 18)) %>%
  # addLegend("bottomleft",
  #           color = blue,
  #           group = 'For Sale (updated 3/25/20)',
  #           title = "Properties For Sale") %>%
  addScaleBar("bottomright") %>%
  hideGroup(c('Sales', 'Heirs', 'Latest Sales', 'Parcels_Cntrd', "Title Search Status", 'Companies', 'Agriculture', 'Water Loggers', 'Inundation'))
m

## exporting as html file for exploration
saveWidget(m, 
           file="/Users/dhardy/Dropbox/r_data/sapelo/hh_property_data.html",
           title = "Hog Hammock Data")

saveWidget(m, 
           file="/Users/dhardy/Dropbox/Sapelo_NSF/maps-gis/interactive-maps/hh_property_data.html",
           title = "Hog Hammock Data")


######
## making water level map only
######

## generate interactive leaflet map
wls <- leaflet() %>%
  addTiles(group = 'Open Street Map') %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Esri World Imagery") %>%
  setView(lng = -81.26, lat = 31.43, zoom = 14) %>%
  addAwesomeMarkers(data = hobo,
                    group = 'Water Loggers',
                    popup = hobo_popup,
                    icon = iconblue) %>%
  addPolygons(data = inund,
              group = 'Inundation',
              fillColor = ~pal5(inund$prb_smplfy),
              fillOpacity = 0.8,
              weight = 1) %>%
  addLayersControl(baseGroups = c("Open Street Map", "Esri World Imagery"), 
                   overlayGroups = c('Water Loggers', 'Inundation'),
                   options = layersControlOptions(collapsed = FALSE)) %>%
  addLegend("bottomleft",
            pal = pal5,
            group = 'Inundation',
            values = inund$prb_smplfy,
            title = "Inundation") %>%
  addScaleBar("bottomright") %>%
  hideGroup(c('Inundation'))
wls


## exporting as html file for exploration
saveWidget(wls, 
           file="/Users/dhardy/Dropbox/r_data/sapelo/hoghammock-water-level-survey/hh-water-level-survey.html",
           title = "Hog Hammock Water Level Survey")

saveWidget(wls, 
           file="/Users/dhardy/Dropbox/Sapelo_NSF/maps-gis/interactive-maps/hh-water-level-survey.html",
           title = "Hog Hammock Water Level Survey")

