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
# nhgis <- read_nhgis_sf(
#   data_file = file.path(datadir, 'population/nhgis/nhgis0017_csv', 
#                         data_layer = 'nhgis0017_ds120_1990_block.csv'),
#   shape_file = file.path(datadir, 'population/nhgis/nhgis0017_shape',
#                          data_layer = 'GA_block_1990.shp'),
#   var_attrs = 'val_label'
# )

nhgis_data <- read.csv(file.path(datadir, 'population/nhgis/nhgis0017_csv/nhgis0017_ds120_1990_block.csv'))
nhgis <- read_sf(file.path(datadir, 'population/nhgis/nhgis0017_shape/GA_block_1990.shp')) %>%
  st_as_sf()

mc <- filter(test, FIPSSTCO == '13001')
  
nhgis_ddi <- read_ipums_codebook(file.path(datadir, 'population/nhgis/nhgis0017_csv'))

ipums_val_labels(nhgis_data$COUNTY)

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