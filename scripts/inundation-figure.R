rm(list=ls())

#library(tidyverse)
library(sf)
library(raster)
library(lubridate)
library(tmap)
library(fasterize)
library(tidyverse)
library(rgdal)
library(grid)
library(gridExtra)

## define data directory
datadir <- '/Users/dhardy/Dropbox/r_data/sapelo'

## import parcel owner data and trans
df <- st_read(file.path(datadir, 'spatial-data/parcel_data_export/parcel_data.shp'), stringsAsFactors = F) %>%
  st_transform(4326) %>%
  mutate(owner = ifelse(is.na(owner), 'unknown', owner),
         own3cat = ifelse(own3cat == 'Non-traditional', 'Non-Descendant', own3cat)) %>%
  filter(own4cat != 'County' & gis_acres != 'NA')

## filter out just companies
comp <- df %>%
  filter(own4cat == 'Company')

## import ag data
ag <- st_read(file.path(datadir, 'spatial-data/ag_plots/'), stringsAsFactors = F)[-1,] %>%
  st_transform(4326)

ag_cntr <- st_centroid(ag) %>%
  filter(name != 'PH1')

## import water level data
hobo <- st_read(file.path(datadir, 'spatial-data/hobo_sites/'), stringsAsFactors = F) %>%
  st_transform(4326) %>%
  rename(site = Id)
info <- read.csv(file.path(datadir, 'water-level/site-elevations.csv'), stringsAsFactors = F) %>%
  mutate(install_date = as.Date(install.date, '%m/%d/%y'),
         site = as.numeric(site)) %>%
  dplyr::select(site, install_date)
hobo <- left_join(hobo, info)

# inund <- raster(file.path(datadir, 'spatial-data/inundation/inund2100hc.tif'))
tidal <- raster(file.path(datadir, 'spatial-data/inundation/is01.tif'))

##import inundation data
inund <- st_read(file.path(datadir, 'spatial-data/inundation/inund2100hc_poly.shp'), stringsAsFactors = F) %>%
  st_transform(4326) %>%
  # filter(prb_smplfy != '1%') %>%
  mutate(prb_smplfy = ifelse(prblty %in% c('50%', '95%'), '50%', prblty)) %>%
  st_make_valid()

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
  
comp.hatch <- hatch(comp, 60)
  
## define map variables
clr.own <- c('black', 'grey60', 'grey95')
clr.ind <- c('#BFE8FF', '#00a9e6','#004c73', '#004c73')
# clr.ind <- c('#ACD1E6', '#86A2B3','#607480', '#607480') ## meager attempt at 10/30/50 tint shades
leafIcon <- tmap_icons("http://leafletjs.com/examples/custom-icons/leaf-green.png")

## temporary fix to empty parcels
df <- df %>% filter(!st_is_empty(.))

tmap_options(check.and.fix = TRUE)

## separate inundation map
map.ind <- 
  tm_shape(df) + tm_borders() + 
  tm_shape(inund) + 
  tm_fill('prb_smplfy', alpha = 0.5, palette = clr.ind,
          title = 'A)\nInundation Probability', group = 
            'funky') + 
  tm_shape(tidal, raster.downsample = FALSE) + 
  tm_raster(title = '', alpha = 1, palette = '#89cd66', legend.show = FALSE) + 
  tm_shape(df) + tm_borders(lwd = 1, col = 'black') + 
  tm_layout(frame = TRUE,
            legend.text.size = 0.7,
            legend.title.size = 0.8,
            legend.bg.color = 'white',
            legend.frame = 'black',
            title.snap.to.legend = TRUE) +
  tm_add_legend(type = 'fill',
                labels = 'Tidal Marsh',
                col = '#89cd66',
                border.lwd = 0,
                group = 'funky')
# map.ind

## separate owner map with ag and water loggers
map.own <- 
  tm_shape(df) + 
  tm_fill('B)\nown3cat', palette = clr.own, 
          title = 'Owner Category') + 
  tm_shape(df) + tm_borders(lwd = 1) + 
  # tm_shape(comp) + tm_borders('yellow') + 
  tm_shape(comp.hatch) + tm_lines() + 
  tm_shape(tidal, raster.downsample = FALSE) + 
  tm_raster(title = '', alpha = 1, palette = '#89cd66', legend.show = FALSE) + 
  tm_shape(ag_cntr) + 
  tm_squares(col = 'green', 
             size = 0.4,
             border.col = 'black',
             border.lwd = 1) + 
  tm_shape(hobo) +
  tm_symbols(col = 'steelblue1',
             size = 0.5,
             border.lwd = 1, 
             border.col = 'black') + 
  tm_layout(frame = TRUE,
            legend.text.size = 0.7,
            legend.title.size = 0.8) +
  tm_add_legend(type = 'fill',
                shape = 19,
                col = 'green',
                labels = 'Agricultural Plots') + 
  tm_add_legend(type = 'symbol',
                shape = 21,
                col = 'steelblue1',
                labels = 'Water Loggers') + 
  tm_scale_bar(breaks = c(0,0.3), text.size = 0.7, position = c(0.7,0)) + 
  tm_compass(position = c(0.74, 0.11), text.size = 0.7)
map.own

tiff(file.path(datadir, 'figures/HardyFigure1.tiff'), units = 'in', width = 7, height = 3.5, 
     res = 300, compression = 'lzw')
tmap_arrange(map.ind, map.own, widths = c(0.5, 0.5))
dev.off()

## presentation slide for owner categories
ts <- 1.3
map.own <- 
  tm_shape(df, unit = 'mi') + 
  tm_fill('own3cat', palette = clr.own, 
          title = 'Hog Hummock\nOwner Category') + 
  tm_shape(df) + tm_borders(lwd = 1) + 
  # tm_shape(comp) + tm_borders('yellow') + 
  tm_shape(comp.hatch) + tm_lines() + 
  # tm_shape(tidal, raster.downsample = FALSE) + 
  # tm_raster(title = '', alpha = 1, palette = '#89cd66', legend.show = FALSE) + 
  tm_shape(ag_cntr) + 
  tm_squares(col = 'green', 
             size = 0.4,
             border.col = 'black',
             border.lwd = 1) + 
  tm_shape(hobo) +
  tm_symbols(col = 'steelblue1',
             size = 0.5,
             border.lwd = 1, 
             border.col = 'black') + 
  tm_layout(frame = TRUE,
            legend.text.size = ts,
            legend.title.size = ts+0.5,
            legend.title.fontface = 2) +
  tm_add_legend(type = 'symbol',
                col = 'black',
                shape = 12,
                size = 1.2,
                labels = 'Company-owned') + 
  tm_add_legend(type = 'symbol',
                shape = 22,
                size = 1.2,
                col = 'green',
                labels = 'Agricultural Plot') + 
  tm_add_legend(type = 'symbol',
                shape = 21,
                size = 1.2,
                col = 'steelblue1',
                labels = 'Water Level Site') + 
  tm_scale_bar(breaks = c(0,0.2), text.size = ts, position = c(0.7,0)) + 
  tm_compass(position = c(0.74, 0.11), text.size = ts)
map.own

jpeg(file.path(datadir, 'figures/hog-hummock_owner-categories.jpeg'), units = 'in', width = 7, height = 7, 
     res = 300)
map.own
dev.off()
 
 