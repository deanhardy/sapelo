rm(list=ls())

library(sf)
library(tidyverse)
library(nhdplusTools) # https://cran.r-project.org/web/packages/nhdplusTools/nhdplusTools.pdf

datadir <- '/Users/dhardy/r_projects/sapelo/nhd'
huc4 <- c('0306', '0307') ## watersheds to download
fcodes <- c('33600', '33601', '33603') ## https://nhd.usgs.gov/userGuide/Robohelpfiles/NHD_User_Guide/Feature_Catalog/Hydrography_Dataset/Complete_FCode_List.htm

df <- download_nhdplushr(datadir, huc4, download_files = TRUE)

require(rgdal)

fgdb <- file.path(df, 'NHDPLUS_H_0307_HU4_GDB.gdb')

# List all feature classes in a file geodatabase
subset(ogrDrivers(), grepl("GDB", name))
fc_list <- ogrListLayers(fgdb)
print(fc_list)

# Read the feature class
fc <- readOGR(dsn=fgdb,layer="NHDFlowline")

# convert to sf object
fc2 <- st_as_sf(fc) %>%
  filter(FCode %in% fcodes)

# Determine the FC extent, projection, and attribute information
summary(fc2)

# View the feature class
qtm(fc2)
