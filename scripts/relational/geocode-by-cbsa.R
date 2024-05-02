###############################################################################################
## PURPOSE: geolocating points inside CBSAs
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

## download CBSAs including metro and micro statistical areas
cbsa <- get_acs(geography = 'cbsa',
                variables = c("B19013_001"), 
                year = 2022,
                geometry = T) %>%
  rowid_to_column(., var = 'cbsa.id') 

cbsa.df <- st_drop_geometry(cbsa) %>%
  rename(cbsa.name = NAME)

# cbsa %>%
#   ggplot(aes(fill = estimate)) + 
#   geom_sf(color = NA) + 
#   scale_fill_viridis_c(option = "magma") 

## import geocoded data
geo <- st_read(file.path(datadir, 'spatial-data/geocoded/tax_geocode.GEOJSON'), stringsAsFactors = F) %>%
  st_transform(., st_crs(cbsa)) %>%
  rowid_to_column(., var = 'row.id')

## identify CBSAs of geocoded addresses
## some help: https://gis.stackexchange.com/questions/282750/identify-polygon-containing-point-with-r-sf-package
int <- data.frame(st_intersects(geo, cbsa)) %>%
  rename(cbsa.id = col.id)

## join cbsa info with geocoded data and remove sapelo addresses
geo.cbsa <- geo %>%
  left_join(., int, by = 'row.id') %>%
  left_join(., cbsa.df, by = 'cbsa.id') %>%
  filter(!str_detect(address, 'Sapelo'))

## summarise counts by cbsa 
geo.fltr.cbsa <- geo.cbsa %>%
  # mutate(cbsa.id = coalesce(cbsa.id, address)) %>%
  group_by(year, category, cbsa.name) %>%
  summarise(count = n()) %>%
  st_centroid(geo.fltr.cbsa)

## create sf vector for sapelo point
sap <- data.frame(x = -81.26206, y = 31.42655)
sap2 <- st_as_sf(sap, coords = c('x', 'y'), crs = 4269) %>%
  mutate(year = 'NA', category = 'sapelo', cbsa.name = 'sapelo', count = 1)

r.all <- rbind(geo.fltr.cbsa, sap2)

## average distance by ownership (nonlocal addresses only)
geo.dist <- geo.cbsa %>%
  st_distance(., sap2) %>%
  as_tibble() %>%
  rename(sap.dist = value) %>%
  drop_units() %>%
  mutate(sap.dist = sap.dist * 0.000621371)

geo.cbsa2 <- cbind(geo.cbsa, geo.dist)

mn.dist <- geo.cbsa2 %>%
  group_by(year, category) %>%
  summarise(mn.dist = mean(sap.dist), max.dist = max(sap.dist), min.dist = min(sap.dist)) %>%
  rename(ownership = category)

boxplot(sap.dist ~ category, data = geo.cbsa2, outline = F, notch = T)

fig.dist <- mn.dist %>%
  filter(year < 2023, ownership != 'unknown') %>%
  ggplot(aes(year, mn.dist, color = ownership)) +
  geom_point() + 
  scale_y_continuous(name = "Nonlocal Tax Addresses (miles)",
                     breaks = seq(0,320, 20),
                     limits = c(0,320)) + 
  scale_x_continuous(breaks = seq(1999, 2022, 3), minor_breaks = seq(1999,2022,1)) + 
  ggtitle('Mean Distance to Nonlocal Tax Addresses by Ownership')
fig.dist

png(paste0(datadir, 'figures/relational/', 'taxaddress-distance-by-ownership.png'), 
    height = 6, width = 10.5, units = 'in', res = 300)
fig.dist
dev.off()

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

YR <- seq(1999, 2022, 1)
# YR <- c('2022')

for (z in seq_along(YR)) {
  
r.df <- filter(geo.fltr.cbsa, year == YR[z]) %>%
  st_transform(5070)

## generate relational lines between sapelo and tax address
r.l <- r.all %>%
  filter(year %in% c(YR[z], 'NA')) %>%
  points_to_lines(., ids = 'cbsa.name', names = 'category', order_matters = F) %>%
  filter(start == 'sapelo') %>%
  mutate(Year = YR[z]) %>%
  st_transform(5070)

fig <- ggplot() +
  geom_sf(data = usa) +   
  geom_sf(color = "#2b2b2b", fill = "white", size=0.125) +
  ggthemes::theme_map() +
  geom_sf(data = r.l, color = "black", ) +
  geom_point(
    aes(color = category, size = count, geometry = geometry),
    data = filter(r.df, category != 'unknown'),
    stat = "sf_coordinates",
    alpha = 0.8,
  ) + 
  coord_sf(crs = st_crs("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"),
           datum = NA) +
  scale_size(breaks = c(10,20,30,40,50), limits = c(0,50), name="Parcels Per CBSA") + 
  scale_color_discrete(name = 'Ownership', labels = c('Descendant', 'Nontraditional')) +
  theme(legend.position = c(0,0)) + 
  ggtitle(paste0('Year ', YR[z], '; ', sum(r.df$count),  ' Tax Addresses')) + 
  theme(legend.text = element_text(size=10),
        legend.title = element_text(size=12),
        plot.title = element_text(size = 20, face = "bold"))
  guides(color = guide_legend(order=1),
         size = guide_legend(order=2))

# legend help
# http://www.sthda.com/english/wiki/ggplot2-legend-easy-steps-to-change-the-position-and-the-appearance-of-a-graph-legend-in-r-software

# ggsave <- function(..., bg = 'white')
    
# save plots as .png
ggsave(fig, file=paste(datadir, 'figures/relational/maps/', 'relational-', YR[z], ".png", sep=''), 
       width = 1800, height = 1200, units = 'px', scale=2, bg = 'white')

}

## create animated gif of outputs using ImageMagick in Terminal
## change to directory of png files
## convert -delay 100 -loop 0 *.png animation.gif
## https://cran.r-project.org/web/packages/magick/vignettes/intro.html#Animated_Graphics
    