rm(list=ls())

library(tidyverse)
library(lubridate)
library(data.table)
library("rio")
library(readxl)
Sys.setenv(TZ='GMT')
## define data directory
datadir <- '/Users/dhardy/Dropbox/r_data/sapelo/water-level/'

## set # measurements to "burn" pre and post data download
burn = 0

#######################################
## import field data measurements
#######################################

## import site characteristics/info 
wls.info <- read.csv(file.path(datadir, 'wls-info.csv')) %>%
  mutate(site_new = transect_site) %>%
  select(site_new, rtkcap_navd88)

## import field measurement data
wls.msmt <- read_xlsx("/Users/dhardy/Dropbox/Sapelo_NSF/water_level_survey/data/sapelo-water-level-survey.xlsx",
                      sheet = 'field_measurements',
                      skip = 6) %>%
  # mutate(launch_time = as.POSIXct(launch_time, format = '%m/%d/%Y %H:%M:%OS'))
  select(-c(launch_time)) %>%
  mutate(dist_A_mm = as.integer(dist_A_mm),
         dist_B_mm = as.integer(dist_B_mm),
         dist_C_mm = as.integer(dist_C_mm))

## assess and plot field measurement averages
msmt.A.avgs <- wls.msmt %>%
  group_by(site_new, serial) %>%
  summarise(lgr_length_avg = round(mean(dist_A_mm/1000, na.rm = T),3),
            lgr_length_sd = round(sd(dist_A_mm/1000, na.rm = T),3),
            lgr_length_n = n()) %>%
  drop_na() %>%
  filter(serial != 'X0976') %>%
  ungroup() %>%
  mutate(site_serial = paste0(site_new, " (", serial, ')'))  

a.mn <- ggplot(msmt.A.avgs, aes(site_serial, lgr_length_avg)) +
  geom_point() + 
  geom_errorbar(aes(ymin=lgr_length_avg-lgr_length_sd, ymax=lgr_length_avg+lgr_length_sd), width=.2,
                position=position_dodge(0.05)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  xlab('Site (Serial #)') + ylab('Length (m)') +
  ggtitle("Mean Logger Hanging Length (Distance A)")
a.mn

png(paste0(datadir, "figures/msmt-a-means.png"), bg = 'white', width = 13.33, height = 6.5, units = 'in', res = 150)
a.mn
dev.off()

msmt.B.avgs <- wls.msmt %>%
  group_by(site_new) %>%
  summarise(
    well_ht_avg = round(mean(dist_B_mm/1000, na.rm = T),3),
    well_ht_sd = round(sd(dist_B_mm/1000, na.rm = T),3),
    well_ht_n = n()) %>%
  drop_na()

b.mn <- ggplot(msmt.B.avgs, aes(site_new, well_ht_avg)) +
  geom_point() + 
  geom_errorbar(aes(ymin=well_ht_avg-well_ht_sd, ymax=well_ht_avg+well_ht_sd), width=.2,
                position=position_dodge(0.05)) + 
  # geom_text(msmt.B.avgs, well_ht_n, nudge_x = 0.1) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  xlab('Site') + ylab('Well Height (m)') +
  ggtitle("Mean Well Height (Distance B)")
b.mn

png(paste0(datadir, "figures/msmt-b-means.png"), bg = 'white', width = 13.33, height = 6.5, units = 'in', res = 150)
b.mn
dev.off()

## attach rtk elevations to msmt data
msmt.A.avgs2 <- left_join(msmt.A.avgs, wls.info)
msmt.A.avgs3 <- left_join(msmt.A.avgs2, msmt.B.avgs)

#########################################
## import water level data files adn tidy
#########################################
filz <- list.files(path = file.path(datadir, 'new-logger-data/'),
                   pattern= '*.csv',
                   full.names = TRUE,
                   recursive = F)
# 
# ## test file
# # filz <- "/Users/dhardy/Dropbox/r_data/sapelo/water-level//new-logger-data/hobo/site06_1316_190522-191022.csv"
# 
# filz.ve <- list.files(path = file.path(datadir, 'new-logger-data/vanessen'),
#                       pattern= '*.CSV',
#                       full.names = TRUE,
#                       recursive = TRUE) 
# 
# ## convert salinity Excel files to csv
# sal <- list.files(path = file.path(datadir, 'new-logger-data/salinity'),
#                   pattern= '*.xlsx',
#                   full.names = TRUE,
#                   recursive = TRUE) 
# created <- mapply(convert, sal, gsub("xlsx", "csv", sal))
# unlink(sal) # delete xlsx files
# 
# filz.psu <- list.files(path = file.path(datadir, 'new-logger-data/salinity'),
#                        pattern= '*.csv',
#                        full.names = TRUE,
#                        recursive = TRUE) 
## read test file
# tf <- read.csv(paste0(datadir, 'new-logger-data/site06_1316_181108-211213.csv'), skip = 1)

## import & tidy hobo water level data
## note water level C is in meters and indicates water level in reference to top of wellcap (negative numbers indicate below for Hobo)
tidal <- NULL
for(i in 1:length(filz)) {
  OUT <- fread(filz[i],
               select = c(2:5),
               col.names = c('date_time_gmt', 'abs_pres_psi', 'water_temp_c', 'sensor_depth'),
               stringsAsFactors = FALSE) %>%
    slice(., burn:(n())) %>% ## removes first ## and last ## readings
    mutate(date_time_gmt = if_else(
      str_detect(date_time_gmt, c('AM|PM')), 
      as.POSIXct(date_time_gmt, format = '%m/%d/%y %I:%M:%S %p', tz = 'GMT'),
      as.POSIXct(date_time_gmt, format = '%m/%d/%y %H:%M:%S', tz = 'GMT')),
      date = as.Date(date_time_gmt, format = '%m/%d/%y', tz = 'GMT'),
      site = str_sub(filz[i], -25,-24),
      serial = str_sub(filz[i], -22,-19),
      water_temp_c = as.numeric(water_temp_c),
      sensor_depth = as.numeric(sensor_depth),
      temp_max = max(water_temp_c)) %>%
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
                                                                                                                                          if_else(site == 'Site-24', 'Johnson', 
                                                                                                                                                  if_else(site == 'Site-15', 'Oakdale', site)))))))))))))))))) %>%
    mutate(sitename = paste(site, name)) %>%
    select(!(abs_pres_psi))
  tidal <- rbind(OUT, tidal)
}

## select relevant hobo data columns
tidal.01 <- tidal %>%
  mutate(logger = 'hobo') %>%
  select(date_time_gmt, water_temp_c, sensor_depth, date, site, name, sitename, logger, serial)

## adding transects with idea that they work north to south then east to west, or clockwise around the community
tidal3.1 <- tidal.01 %>%
  mutate(type = ifelse(site %in% c('Site-13', 'Site-11', 'Site-09', 'Site-05', 'Site-19', 'Site-14', 'Site-15', 'Site-18', 'Site-20', 'Site-23', 'Site-24'), 'ditch',
                       ifelse(site %in% c('Site-02', 'Site-03', 'Site-07', 'Site-06', 'Site-12', 'Site-16'), 'creek', site))) %>% ## ditches sites
  # filter(site %in% c('Site-02', 'Site-03', 'Site-07', 'Site-06', 'Site-12')) %>% ## marsh sites
  mutate(transect = ifelse(site %in% c('Site-06', 'Site-12', 'Site-19', 'Site-13', 'Site-18'), 'T1',
                           ifelse(site %in% c('Site-02', 'Site-03', 'Site-05','Site-11', 'Site-23'), 'T3',
                                  ifelse(site %in% c('Site-07', 'Site-09', 'Site-24'), 'T4',
                                         ifelse(site %in% c('Site-16', 'Site-15'), "T5",
                                                ifelse(site %in% c('Site-20'), 'T2',
                                                       if_else(site %in% c('Site-14'), 'T6', site)))))))

## renaming sites to be more logical related to transects; site-05 is a branch of T3
tidal3.2 <- tidal3.1 %>%
  mutate(site_new = if_else(site == 'Site-06', 'T1-01',
                            if_else(site == 'Site-12', 'T1-02', 
                                    if_else(site == 'Site-19', 'T1-03',
                                            if_else(site == 'Site-13', 'T1-04',
                                                    if_else(site == 'Site-18', 'T1-05',
                                                            if_else(site == 'Site-20', 'T2-01', 
                                                                    if_else(site == 'Site-02', 'T3-01',
                                                                            if_else(site == 'Site-03', 'T3-02',
                                                                                    if_else(site =='Site-05', 'T3-BR-01',
                                                                                            if_else(site == 'Site-11', 'T3-03',
                                                                                                    if_else(site == 'Site-23', 'T3-04',
                                                                                                            if_else(site == 'Site-07', 'T4-01',
                                                                                                                    if_else(site == 'Site-09', 'T4-BR-01',
                                                                                                                            if_else(site == 'Site-24', 'T4-02',
                                                                                                                                    if_else(site == 'Site-16', 'T5-01',
                                                                                                                                            if_else(site == 'Site-15', 'T5-02', 
                                                                                                                                                    if_else(site == 'Site-14', 'T6-01', site)))))))))))))))))) %>%
  mutate(sitename_new = paste(site_new, name),
         site_serial = paste0(site_new, ' (', serial, ')'))


## attach name to elevation data
SN <- msmt.A.avgs$site_serial

## adding reference levels in NAVD88
tidal4 <- NULL

for (i in 1:length(SN)) {
  
  el2 <- msmt.A.avgs3 %>%
    filter(site_serial == SN[[i]])
  
  OUT2 <- tidal3.2 %>%
    filter(site_serial == SN[[i]]) %>%
    mutate(water_level_navd88 = el2$rtkcap_navd88 + sensor_depth,
           wellcap_navd88 = el2$rtkcap_navd88,
           subst_navd88 = el2$rtkcap_navd88 - el2$well_ht_avg,
           screen_bottom_navd88 = round(el2$rtkcap_navd88 - 0.6096,3), # screens are ~ top 2' of wells
           sensor_navd88 = el2$rtkcap_navd88 - el2$lgr_length_avg
           ) %>%
    mutate(water_depth = water_level_navd88 - subst_navd88)
  
  tidal4 <- rbind(OUT2, tidal4)
}

## still need to merge average field measurements, water depth, and cleanup columns
