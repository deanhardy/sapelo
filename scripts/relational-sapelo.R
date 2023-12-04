###############################################################################################
## PURPOSE: assesssing relational geographies between real property and tax addresses 
## BY: Dean Hardy
###############################################################################################
rm(list=ls())

library(tidyverse)
library(sf)
library(tmap)
utm <- 2150 ## NAD83 17N

## define data directory
datadir <- '/Users/dhardy/Dropbox/r_data/sapelo'

p <- st_read(file.path(datadir, 'spatial-data/parcel_data_export/parcel_data.shp'), stringsAsFactors = FALSE)

## import data
r00d <- st_read(file.path(datadir, 'spatial-data/sydneyRA/shapefiles8_21/Descendant2000MSA.shp'), stringsAsFactors = FALSE)
r10d <- st_read(file.path(datadir, 'spatial-data/sydneyRA/shapefiles8_21/descendantMSA2010.shp'), stringsAsFactors = FALSE)
r22d <- st_read(file.path(datadir, 'spatial-data/sydneyRA/shapefiles8_21/DescendantMSA_2022.shp'), stringsAsFactors = FALSE)

r00nd <- st_read(file.path(datadir, 'spatial-data/sydneyRA/shapefiles8_21/nondescendant2000MSA.shp'), stringsAsFactors = FALSE)
r10nd <- st_read(file.path(datadir, 'spatial-data/sydneyRA/shapefiles8_21/nondescendantMSA2010.shp'), stringsAsFactors = FALSE)
r22nd <- st_read(file.path(datadir, 'spatial-data/sydneyRA/shapefiles8_21/nondescendantMSA_2022.shp'), stringsAsFactors = FALSE)

## create sf vector for sapelo point
df <- data.frame(x = -81.26206, y = 31.42655)
sap <- st_as_sf(df, coords = c('x', 'y'), crs = 4326) %>%
  mutate(owncat = 'NA', Match_addr = 'sapelo', Year = 'NA', City = 'Sapelo Island', State = 'GA', ZIP_Code = '31327', MSA = 'NA', ParcelPerM = 'NA') %>%
  relocate(geometry, .after = ParcelPerM)

## clean & join data by year
## # variables ranges from 11 to 20 for each of six objects
r00d_2 <- r00d %>%
  mutate(owncat = 'descendant') %>%
  relocate(owncat, .before = Match_addr) %>%
  select(owncat, Match_addr, Year, City, State, ZIP_Code, MSA, ParcelPerM, geometry)
r00nd_2 <- r00nd %>%
  select(Match_addr, Year, City, State, ZIP_Code, MSA, ParcelsPer, geometry) %>%
  mutate(owncat = 'nondescendant') %>%
  relocate(owncat, .before = Match_addr) %>%
  rename(ParcelPerM = ParcelsPer)
r00 <- rbind(r00d_2, r00nd_2)
r00_2 <- rbind(r00, sap)

## maybe some help making lines between pairs of points, but not using here
## https://gis.stackexchange.com/questions/270725/r-sf-package-points-to-multiple-lines-with-st-cast

## create points to lines function, went this direction
## https://www.jla-data.net/eng/lines-from-points/
points_to_lines <- function(data, ids, names, order_matters = TRUE) {
  
  # dataframe of combinations - based on row index
  idx <- expand.grid(start = seq(1, nrow(data), 1),
                     end = seq(1, nrow(data), 1)) %>%
    # no line with start & end being the same point
    dplyr::filter(start != end) %>%  
    # when order doesn't matter just one direction is enough
    dplyr::filter(order_matters | start > end) 
  
  
  # cycle over the combinations
  for (i in seq_along(idx$start)) {
    
    # line object from two points
    wrk_line  <- data[c(idx$start[i], idx$end[i]), ] %>% 
      st_coordinates() %>% 
      st_linestring() %>% 
      st_sfc()
    
    # a single row of results dataframe
    line_data <- data.frame(
      start = pull(data, ids)[idx$start[i]],
      end = pull(data, ids)[idx$end[i]],
      label = paste(pull(data, names)[idx$start[i]], 
                    "-", 
                    pull(data, names)[idx$end[i]]),
      geometry = wrk_line
    )
    
    # bind results rows to a single object
    if (i == 1) {
      res <- line_data
      
    } else {
      res <- dplyr::bind_rows(res, line_data)
      
    } # /if - saving results
    
  } # /for
  
  # finalize function result
  res <- sf::st_as_sf(res, crs = sf::st_crs(data))
  
  res
  
} # /function


## manipulate point data into relational multilinestrings
r00_lines <- points_to_lines(r00_2, ids = 'Match_addr', names = 'owncat', order_matters = F) %>%
  filter(start == 'sapelo')


## map data
usa <- st_as_sf(maps::map("state", fill=TRUE, plot =FALSE))

ggplot() +
  geom_sf(data = usa) + 
  geom_sf(data = r00_lines, color = "black") + 
  geom_point(
    aes(color = owncat, size = ParcelPerM, geometry = geometry),
    data = r00_2,
    stat = "sf_coordinates"
  ) + 
  theme(legend.position = "bottom")
