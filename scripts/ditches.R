rm(list=ls())

library(sf)
library(tidyverse)
library(nhdplusTools) # https://cran.r-project.org/web/packages/nhdplusTools/nhdplusTools.pdf
library(tmap)

nhddir <- '/Users/dhardy/r_projects/sapelo/nhd'
datadir <- '/Users/dhardy/Dropbox/r_data/sapelo'

huc4 <- c('0304', '0305', '0306', '0307') ## watersheds to download
fcodes <- c('33600', '33601', '33603') ## https://nhd.usgs.gov/userGuide/Robohelpfiles/NHD_User_Guide/Feature_Catalog/Hydrography_Dataset/Complete_FCode_List.htm

df <- download_nhdplushr(nhddir, huc4, download_files = TRUE)

require(rgdal)

## List all feature classes in a file geodatabase
# subset(ogrDrivers(), grepl("GDB", name))
# fc_list <- ogrListLayers(fgdb)
# print(fc_list)

nhd <- NULL ## for use in for loop

# Read the feature class and convert to sf object
for (i in 1:length(huc4)) {
fgdb <- file.path(df, paste('NHDPLUS_H_', huc4[[i]], '_HU4_GDB.gdb', sep = ''))

OUT <- readOGR(dsn=fgdb,layer="NHDFlowline") %>%
  st_as_sf() %>%
  filter(FCode %in% fcodes)

nhd <- rbind(nhd, OUT)
}

# import coastal counties
cnty <- st_read(file.path(datadir, 'coastal_county/USsouth_CoastalCounty_2010Census_DP1.shp'), stringsAsFactors = FALSE) %>%
  filter(str_detect(GEOID10, '^45|^13')) ## filter to GA and SC

## map canals ditches for AOI
tm_shape(cnty) + 
  tm_polygons() + 
tm_shape(nhd) + 
  tm_lines(col = 'blue')


