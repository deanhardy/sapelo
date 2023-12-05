###############################################################################################
## PURPOSE: assesssing relational geographies between real property and tax addresses 
## BY: Dean Hardy
###############################################################################################
rm(list=ls())

library(tidyverse)
library(sf)
library(ggthemes)

utm <- 2150 ## NAD83 17N

## define data directory
datadir <- '/Users/dhardy/Dropbox/r_data/sapelo/'

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
  mutate(owncat = 'NA', Match_addr = 'sapelo', Year = 'NA', City = 'Sapelo Island', State = 'GA', 
         ZIP_Code = '31327', MSA = 'NA', ParcelPerM = 'NA') %>%
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
r00_2 <- rbind(r00, sap) %>%
  mutate(ParcelPerM = as.numeric(ParcelPerM))

r10d_2 <- r10d %>%
  mutate(owncat = 'descendant', Match_addr = 'NA', MSA = 'NA') %>%
  relocate(owncat, .before = Match_addr) %>%
  rename(ParcelPerM = Parcelsown,) %>%
  mutate(Match_addr = paste(ZIP_Code, City, State, sep = ', ')) %>%
  select(owncat, Match_addr, Year, City, State, ZIP_Code, MSA, ParcelPerM, geometry)
r10nd_2 <- r10nd %>%
  mutate(owncat = 'nondescendant') %>%
  relocate(owncat, .before = Match_addr) %>%
  rename(ParcelPerM = Properties) %>%
  select(owncat, Match_addr, Year, City, State, ZIP_Code, MSA, ParcelPerM, geometry)
r10 <- rbind(r10d_2, r10nd_2)
r10_2 <- rbind(r10, sap) %>%
  mutate(ParcelPerM = as.numeric(ParcelPerM))

r22d_2 <- r22d %>%
  mutate(owncat = 'descendant', Year = 2022) %>%
  relocate(owncat, .before = Match_addr) %>%
  rename(ParcelPerM = ParcelsPer, State = RegionAbbr, ZIP_Code = Postal) %>%
  filter(!st_is_empty(.)) %>%
  select(owncat, Match_addr, Year, City, State, ZIP_Code, MSA, ParcelPerM, geometry)
r22nd_2 <- r22nd %>%
  mutate(owncat = 'nondescendant', Year = 2022) %>%
  relocate(owncat, .before = Match_addr) %>%
  rename(ParcelPerM = parcels_ow, ZIP_Code = zipcode, City = city, State = state) %>%
  select(owncat, Match_addr, Year, City, State, ZIP_Code, MSA, ParcelPerM, geometry)
r22 <- rbind(r22d_2, r22nd_2)
r22_2 <- rbind(r22, sap) %>%
  mutate(ParcelPerM = as.numeric(ParcelPerM)) %>%
  # mutate(Match_addr = paste(ZIP_Code, City, State, sep = ', '),
  mutate(MSA = replace_na(MSA, 'NA')) 

r.all <- rbind(r22_2, r10_2, r00_2)

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

## map data
usa <- st_as_sf(maps::map("state", fill=TRUE, plot =FALSE))

## manipulate point data into relational multilinestrings

r00_lines <- points_to_lines(r00_2, ids = 'Match_addr', names = 'owncat', order_matters = F) %>%
  filter(start == 'sapelo') %>%
  mutate(Year = 2000)

r10_lines <- points_to_lines(r10_2, ids = 'Match_addr', names = 'owncat', order_matters = F) %>%
  filter(start == 'sapelo') %>%
  mutate(Year = 2010)

r22_lines <- points_to_lines(r22_2, ids = 'Match_addr', names = 'owncat', order_matters = F) %>%
 filter(start == 'sapelo') %>%
  mutate(Year = 2022)

r_lines.all <- rbind(r00_lines, r10_lines, r22_lines)

YR <- c('2022', '2010', '2000')

for (z in seq_along(YR)) {

r.df <- filter(r.all, Year == YR[z])
r.l <- filter(r_lines.all, Year == YR[z])
  
fig <- ggplot() +
  geom_sf(data = usa) +   geom_sf(color = "#2b2b2b", fill = "white", size=0.125) +
  coord_sf(crs = st_crs("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"),
           datum = NA) +
  ggthemes::theme_map() +
  geom_sf(data = r.l, color = "black") + 
  geom_point(
    aes(color = owncat, size = ParcelPerM, geometry = geometry),
    data = filter(r.df, owncat != 'NA'),
    stat = "sf_coordinates",
    alpha = 0.8,
  ) + 
  scale_size(range = c(1, 10), name="Parcels Per MSA") + 
  theme(legend.position = "bottom") + 
  ggtitle(YR[z])

# save plots as .png
ggsave(fig, file=paste(datadir,
                        'figures/relational/', 'relational-', YR[z], ".png", sep=''), width = 6, height = 5, units = 'in', scale=2)

}
