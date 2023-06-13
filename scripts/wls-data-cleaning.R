rm(list=ls())

library(tidyverse)
library(lubridate)
library(data.table)
library("rio")
Sys.setenv(TZ='GMT')

## define data directory
datadir <- '/Users/dhardy/Dropbox/r_data/sapelo/water-level/'

## define depth reference datum as NAVD88 or local substrate
# level.var <- c('water_depth_m')


#######################################
## import and clean data
#######################################

## import water level data files
filz <- list.files(path = file.path(datadir, 'new-logger-data/hobo'),
                   pattern= '*.csv',
                   full.names = TRUE,
                   recursive = TRUE) 
tidal <- NULL

filz.ve <- list.files(path = file.path(datadir, 'new-logger-data/vanessen'),
                   pattern= '*.CSV',
                   full.names = TRUE,
                   recursive = TRUE) 
tidal.ve <- NULL

## convert salinity Excel files to csv
sal <- list.files(path = file.path(datadir, 'new-logger-data/salinity'),
                       pattern= '*.xlsx',
                       full.names = TRUE,
                       recursive = TRUE) 
created <- mapply(convert, sal, gsub("xlsx", "csv", sal))
unlink(sal) # delete xlsx files

filz.psu <- list.files(path = file.path(datadir, 'new-logger-data/salinity'),
                      pattern= '*.csv',
                      full.names = TRUE,
                      recursive = TRUE) 
tidal.psu <- NULL

## import mllw elevation including lidar and RTK adjusted elevations 
elev <- read.csv(file.path(datadir, 'site-elevations.csv'))

## import & tidy hobo water level data
## note water level C is in meters and indicates water level in reference to top of wellcap (negative numbers indicate below for Hobo)
for(i in 1:length(filz)) {
  OUT <- fread(filz[i],
               select = c(2:5),
               col.names = c('date_time_gmt', 'abs_pres_psi', 'water_temp_c', 'water_level_C'),
               stringsAsFactors = FALSE) %>%
    slice(., 5:(n()-7)) %>% ## removes first and last ## readings
    mutate(date_time_gmt = mdy_hms(date_time_gmt),
           date = as.Date(date_time_gmt, '%m/%d/%y', tz = 'GMT'),
           site = str_sub(filz[i], -25,-24),
           serial = str_sub(filz[i], -22,-19),
           water_temp_c = as.numeric(water_temp_c),
           water_level_C = as.numeric(water_level_C)) %>%
    mutate(site = paste('Site', site, sep = '-')) %>%
    mutate(name = if_else(site == 'Site-02', 'Snagtree',
                          if_else(site == 'Site-03', 'St. Lukes',
                                  if_else(site == 'Site-05', 'Graball',
                                          if_else(site == 'Site-06', 'Dani Trap',
                                                  if_else(site == 'Site-07', 'Cactus Patch',
                                                          if_else(site == 'Site-09', 'Mr. Tracy',
                                                                  if_else(site == 'Site-11', 'Library',
                                                                          if_else(site == 'Site-12', 'Mr. Smith',
                                                                                  if_else(site == 'Site-13', 'Purple Ribbon',
                                                                                          if_else(site == 'Site-14', 'Tidal Gate', 
                                                                                                  if_else(site == 'Site-19', 'The Trunk',
                                                                                                          if_else(site == 'Site-16', 'South Oakdale', 
                                                                                                                  if_else(site == 'Site-18', 'NW Corner', 
                                                                                                                          if_else(site == 'Site-20', 'Walker', 
                                                                                                                                  if_else(site == 'Site-23', 'Hillery',
                                                                                                                                          if_else(site == 'Site-24', 'Johnson', site))))))))))))))))) %>%
    mutate(sitename = paste(site, name)) %>%
    select(!(abs_pres_psi))
  tidal <- rbind(OUT, tidal)
}

## select relevant hobo data columns
tidal.01 <- tidal %>%
  mutate(logger = 'hobo') %>%
  select(date_time_gmt, water_temp_c, water_level_C, date, site, name, sitename, logger, serial)

## import & tidy van essen water level data
## note water level C is in meters and indicates water level in reference to top of wellcap (negative numbers indicate below for VE data)
for(i in 1:length(filz.ve)) {
  OUT <- fread(filz.ve[i],
               select = c(1:3),
               col.names = c('date_time_gmt', 'water_level_C', 'water_temp_c'),
               stringsAsFactors = FALSE) %>%
    slice(., 1:(n()-2)) %>% ## removes first and last ## readings
    mutate(date_time_gmt = ymd_hms(date_time_gmt),
           date = as.Date(date_time_gmt, '%y/%m/%d', tz = 'GMT'),
           site = str_sub(filz.ve[i], 5,6),
           serial = str_sub(filz.ve[i], 8,12),
           water_level_C = as.numeric(water_level_C)/1000 * -1) %>%
    mutate(site = paste('Site', site, sep = '-')) %>%
    mutate(name = if_else(site == 'Site-07', 'Cactus Patch', 
                          if_else(site == 'Site-09', 'Mr. Tracy',
                                  if_else(site == 'Site-11', 'Library',
                                          if_else(site == 'Site-13', 'Purple Ribbon',
                                                  if_else(site == 'Site-14', 'Tidal Gate',
                                                          if_else(site == 'Site-15', 'Oakdale',
                                                                  if_else(site == 'Site-19', 'The Trunk', site)))))))) %>%
    mutate(sitename = paste(site, name))
  tidal.ve <- rbind(OUT, tidal.ve)
}

## select relevant ve data columns
tidal.ve2 <- tidal.ve %>%
  mutate(date_time_gmt = as.POSIXct(date_time_gmt),
         logger = 'van essen') %>%
  select(date_time_gmt, water_temp_c, water_level_C, date, site, name, sitename, logger, serial)

## merge hobo and van essan data
tidal1 <- rbind(tidal.01, tidal.ve2)

## import & tidy van essen specific conductivity/salinity data
## note water level C is in meters and indicates water level in reference to top of wellcap (negative numbers indicate below for VE data)
for(i in 1:length(filz.psu)) {
  OUT <- fread(filz.psu[i],
               select = c(2:8),
               col.names = c('date_time_est', 'pressure', 'water_temp_c', 'conductivity', 'water_level_C', 'datum_reference', 'salinity'),
               stringsAsFactors = FALSE) %>%
    # slice(., 5:(n()-7)) %>% ## removes first and last ## readings
    mutate(date_time_est = ymd_hms(date_time_est),
           date = as.Date(date_time_est, '%y/%m/%d', tz = 'EST'),
           site = str_sub(filz.psu[i], -26,-25),
           water_temp_c = as.numeric(water_temp_c),
           water_level_C = as.numeric(water_level_C)/1000 * -1) %>%
    mutate(site = paste('Site', site, sep = '-')) %>%
    mutate(name = if_else(site == 'Site-02', 'Snagtree',
                          if_else(site == 'Site-03', 'St. Lukes',
                                  if_else(site == 'Site-05', 'Graball',
                                          if_else(site == 'Site-06', 'Dani Trap',
                                                  if_else(site == 'Site-07', 'Cactus Patch',
                                                          if_else(site == 'Site-09', 'Mr. Tracy',
                                                                  if_else(site == 'Site-11', 'Library',
                                                                          if_else(site == 'Site-12', 'Mr. Smith',
                                                                                  if_else(site == 'Site-13', 'Purple Ribbon',
                                                                                          if_else(site == 'Site-14', 'Tidal Gate', 
                                                                                                  if_else(site == 'Site-15', 'Oakdale', 
                                                                                                          if_else(site == 'Site-19', 'The Trunk', site))))))))))))) %>%   
             mutate(sitename = paste(site, name))
  tidal.psu <- rbind(OUT, tidal.psu)
}

options(scipen=999)

## clean up salinity data
tidal.psu2 <- tidal.psu %>%
  mutate(date_time_gmt = as.POSIXct(date_time_est + hours(5))) %>%
  select(date_time_gmt, site, sitename, salinity) %>%
  filter(salinity < 50 | is.na(salinity))

## plot salinity at all sites
## this plot still needs lots of work as does the data collection process
# p <- ggplot(tidal.psu2, aes(date_time_gmt, salinity)) + geom_line(lwd = 0.1)
# q <- p + facet_grid(rows = vars(site))


# png(q, filename = paste(datadir, 'figures/', 'Salinity', '.png', sep = ''), width = 9, height = 6.5, units = 'in', res = 300)
# q
# dev.off()

## merge salinity data to water level data 
tidal1.1 <- full_join(tidal1, tidal.psu2, by = c('sitename', 'date_time_gmt')) %>%
  select(!(site.y))

## attach name to elevation data
SN <- elev$name

## adding references for water level from well cap to substrate (depth) and NAVD88
tidal2 <- NULL

for (i in 1:length(SN)) {
  
  el2 <- elev %>%
  filter(name == SN[[i]]) 
  
  OUT2 <- tidal1.1 %>%
    filter(name == SN[[i]]) %>%
    mutate(water_depth_m = water_level_C + el2$well_ht_m,
           water_level_navd88 = el2$wellcap_navd88_m + water_level_C,
           well_ht = el2$well_ht)
  
  tidal2 <- rbind(OUT2, tidal2)
}

## filter extreme values & convert F temps to C temps
tidal3 <- tidal2 %>% 
  filter(!(water_level_C < -4 | water_level_C >2.5)) %>%
  mutate(water_temp_c = as.numeric(water_temp_c)) %>%
  mutate(water_temp_c = if_else(water_temp_c >= 40, (water_temp_c -32) * 5/9, water_temp_c)) %>%
  rename(site = site.x)

## adding transects with idea that they work north to south then east to west, or clockwise around the community
tidal3.1 <- tidal3 %>%
  mutate(type = ifelse(site %in% c('Site-13', 'Site-11', 'Site-09', 'Site-05', 'Site-19', 'Site-14', 'Site-15', 'Site-18', 'Site-20', 'Site-23', 'Site-24'), 'ditch', 
                       ifelse(site %in% c('Site-02', 'Site-03', 'Site-07', 'Site-06', 'Site-12', 'Site-16'), 'creek', site))) %>% ## ditches sites
  # filter(site %in% c('Site-02', 'Site-03', 'Site-07', 'Site-06', 'Site-12')) %>% ## marsh sites
  mutate(transect = ifelse(site %in% c('Site-06', 'Site-12', 'Site-19', 'Site-13'), 'T1', 
                           ifelse(site %in% c('Site-02', 'Site-03', 'Site-05','Site-11', 'Site-23'), 'T3',
                                  ifelse(site %in% c('Site-07', 'Site-09', 'Site-24'), 'T4', 
                                         ifelse(site %in% c('Site-16', 'Site-15', 'Site-18'), "T5",
                                                ifelse(site %in% c('Site-20'), 'T2', site))))))

## working on renaming sites to be more logical related to transects; site-05 is a branch of T3
tidal3.2 <- tidal3.1 %>%
  mutate(site_new = if_else(site == 'Site-06', 'T1-01',
                            if_else(site == 'Site-12', 'T1-02', 
                                    if_else(site == 'Site-19', 'T1-03',
                                            if_else(site == 'Site-13', 'T1-04',
                                                    if_else(site == 'Site-18', 'T5-03',
                                                            if_else(site == 'Site-20', 'T2-01', 
                                                                    if_else(site == 'Site-02', 'T3-01',
                                                                            if_else(site == 'Site-03', 'T3-02',
                                                                                    if_else(site =='Site-05', 'T3-BR-01',
                                                                                            if_else(site == 'Site-11', 'T3-03',
                                                                                                    if_else(site == 'Site-23', 'T3-04',
                                                                                                            if_else(site == 'Site-07', 'T4-01',
                                                                                                                    if_else(site == 'Site-09', 'T4-02',
                                                                                                                            if_else(site == 'Site-24', 'T4S-01',
                                                                                                                                    if_else(site == 'Site-16', 'T5-01',
                                                                                                                                            if_else(site == 'Site-15', 'T5-02', site)))))))))))))))))


## export merged and cleaned data
write.csv(tidal3.2, paste(datadir, 'wls_data.csv'))
