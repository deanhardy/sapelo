###############################################################################################
## PURPOSE: geolocating points inside block groups to run demographic analysis of BGs
## BY: Dean Hardy
###############################################################################################
rm(list=ls())

library(tidyverse)
library(sf)
library(tidygeocoder)
library(tidycensus)
library(units)

## define data directory
datadir <- '/Users/dhardy/Dropbox/r_data/sapelo/'

## import geocoded data
geo <- st_read(file.path(datadir, 'spatial-data/geocoded/tax_geocode.GEOJSON'), stringsAsFactors = F) %>%
  st_transform(., st_crs(4269)) %>%
  rowid_to_column(., var = 'row.id')

ST <- unique(geo$state)
# ST <- 'NM'

v22 <- load_variables(2022, "acs5", cache = TRUE)
geo.CNTY <- NULL

## download all counties within all states in geo
for (i in 1:length(ST)) {

OUT <- get_acs(geography = 'county',
                      variables = c("B01003_001"), ## total population for BG
                      state = ST[[i]],
                      year = 2020,
                      geometry = T) %>%
  rowid_to_column(., var = 'cnty.id') 

OUT.df <- st_drop_geometry(OUT) %>%
  rename(cnty.name = NAME)

## filter to ith state
geo.ST <- geo %>%
  filter(state == ST[[i]])

## identify counties of geocoded addresses
int <- data.frame(st_intersects(geo, OUT)) %>%
  rename(cnty.id = col.id)

## join cnty info with geocoded data and remove sapelo addresses
OUT2 <- geo.ST %>%
  left_join(., int, by = 'row.id') %>%
  left_join(., OUT.df, by = 'cnty.id') %>%
  filter(!str_detect(address, 'Sapelo'))

geo.CNTY <- rbind(geo.CNTY, OUT2)
}



geo.CNTY2 <- geo.CNTY %>%
  filter(GEOID != 'NA')
  
COUNTY <- unique(geo.CNTY2$GEOID)

# ST <- 'NM'

geo.BG <- NULL

## download all counties within all states in geo
for (i in 1:length(COUNTY)) {
  
  OUT <- get_acs(geography = 'cbg',
                 variables = c("B01003_001"), ## total population for CBG
                 state = substr(COUNTY[[i]], start = 1, stop = 2),
                 county = substr(COUNTY[[i]], start = 3, stop = 5),
                 year = 2020,
                 geometry = T) %>%
    rowid_to_column(., var = 'bg.id') 
  
  OUT.df <- st_drop_geometry(OUT) %>%
    rename(bg.name = NAME)
  
  ## filter to ith county
  geo.COUNTY <- geo.CNTY2 %>%
    filter(GEOID == COUNTY[[i]])
  
  ## identify counties of geocoded addresses
  int <- data.frame(st_intersects(geo, OUT)) %>%
    rename(bg.id = col.id)
  
  ## join cnty info with geocoded data and remove sapelo addresses
  OUT2 <- geo.COUNTY %>%
    left_join(., int, by = 'row.id') %>%
    left_join(., OUT.df, by = 'bg.id')
  
  geo.BG <- rbind(geo.BG, OUT2)
}
