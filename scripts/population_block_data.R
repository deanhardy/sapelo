################################################################
## 2000 & 2010 census data access (2010) and tidying (both)
################################################################
rm(list=ls())

library(tidycensus)
library(sf)
library(ipumsr)

## define data directory
datadir <- '/Users/dhardy/Dropbox/r_data/sapelo'

## import nhgis data
nhgis <- read_nhgis_sf(
  data_file = file.path(datadir, "population/nhgis/nhgis0017_csv.zip"),
  shape_file = file.path(datadir, "population/nhgis/nhgis0017_shape.zip")
)

nhgis_ddi <- read_ipums_codebook(file.path(datadir, "population/nhgis/nhgis0017_csv.zip"))

## import 2010 pop data
vvv <- load_variables(2000, 'sf1', cache = TRUE)
vars = c('P012001', 'P012A001', 'P012B001', 'P012C001', 'P012D001', 'P012E001', 'P012F001',
         'P012G001', 'P012H001', 'P012I001')
pop10 <- get_decennial(
  geography = 'block',
  variables = vars, 
  year = 2010, 
  state = 'GA',
  county = 'McIntosh',
  geometry = TRUE,
  output = 'wide')

# pop10 %>% ggplot(aes(fill = value)) + geom_sf()

pop10 %>% st_write(file.path(datadir,'pop10-blocks.shp'), driver = 'ESRI Shapefile')