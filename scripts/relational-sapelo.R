rm(list=ls())

library(tidyverse)
library(sf)
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

## clean & join data by year
## # variables ranges from 11 to 20 for each of six objects
r00d_2 <- r00d %>%
  mutate(owncat = 'descendant') %>%
  relocate(owncat, .before = Match_addr)
r00nd_2 <- r00nd %>%
  select(Match_addr, Year, City, State, ZIP_Code, POINT_X, POINT_Y, Sapelo_X, Sapelo_Y, MSA, ParcelsPer, geometry) %>%
  mutate(owncat = 'nondescendant') %>%
  relocate(owncat, .before = Match_addr) %>%
  rename(ParcelPerM = ParcelsPer)
r00 <- rbind(r00d_2, r00nd_2)

r10d_2 <- r10d %>%
  


## maybe some help making lines between pairs of points
## https://gis.stackexchange.com/questions/270725/r-sf-package-points-to-multiple-lines-with-st-cast