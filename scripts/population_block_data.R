################################################################
## 2000 & 2010 census data access (2010) and tidying (both)
################################################################
rm(list=ls())

library(tidyverse)
library(tidycensus)
library(sf)
library(ipumsr)

## define data directory
datadir <- '/Users/dhardy/Dropbox/r_data/sapelo'

## import nhgis data
# nhgis_ddi <- read_ipums_codebook(file.path(datadir, 'population/nhgis/nhgis0017_csv'))
#
nhgis <- read_nhgis_sf(
  data_file = file.path(datadir, 'population/nhgis/ga_block_data90_00',
                        data_layer = 'nhgis0017_ds120_1990_block.csv'),
  shape_file = file.path(datadir, 'population/nhgis/ga_block_1990',
                         data_layer = 'GA_block_1990.shp'),
  var_attrs = 'val_label'
)


## import 1990 NHGIS block data
nhgis_90data <- read.csv(file.path(datadir, 'population/nhgis/ga_block_data/nhgis0017_ds120_1990_block.csv'),
                         stringsAsFactors = FALSE) %>%
  mutate(COUNTY = as.character(COUNTY))
mc_90data <- filter(nhgis_90data, COUNTY == 'McIntosh') %>%
  mutate_at(vars(starts_with('ET')), funs(as.numeric)) %>%
  mutate(white = rowSums(.[27:88]), black = rowSums(.[89:150]), other = rowSums(.[151:336]),
         total = rowSums(.[27:336]))
mc_90shp <- read_sf(file.path(datadir, 'population/nhgis/ga_block_1990/')) %>%
  st_as_sf() %>%
  filter(FIPSSTCO == '13191')
mc90 <- left_join(mc_90shp, mc_90data)

qtm(mc90, fill = 'black')

mc90 %>% st_write(file.path(datadir, 'population/nhgis/mc_block_1990.shp'), driver = 'ESRI Shapefile')


## import 2000 NHGIS block data
nhgis_00data <- read.csv(file.path(datadir, 'population/nhgis/ga_block_data/nhgis0017_ds147_2000_block.csv'),
                         stringsAsFactors = FALSE) %>%
  mutate(COUNTY = as.character(COUNTY))
mc_00data <- filter(nhgis_00data, COUNTY == 'McIntosh') %>%
  mutate_at(vars(starts_with('FY')), funs(as.numeric)) %>%
  mutate(white = rowSums(.[14:59]), black = rowSums(.[60:105]), other = rowSums(.[106:335]),
         total = rowSums(.[14:335]))
mc_00shp <- read_sf(file.path(datadir, 'population/nhgis/ga_block_2000/')) %>%
  st_as_sf() %>%
  filter(FIPSSTCO == '13191')
mc00 <- left_join(mc_00shp, mc_00data)

qtm(mc00, fill = 'black')

mc00 %>% st_write(file.path(datadir, 'population/nhgis/mc_block_2000.shp'), driver = 'ESRI Shapefile')


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