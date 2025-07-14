rm(list=ls())

library(tidyverse)
library(lubridate)
library(data.table)
library("rio")
library(readxl)
Sys.setenv(TZ='GMT')
## define data directory
datadir <- '/Users/dhardy/Dropbox/r_data/sapelo/water-level/'

## define depth reference datum as NAVD88 or local substrate
# level.var <- c('water_depth_m')

## set # measurements to "burn" pre and post data download
burn = 1

#######################################
## import and clean data
#######################################

## import site characteristics/info 
wls.info <- read.csv(file.path(datadir, 'wls-info.csv'))

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

## import water level data files
filz <- list.files(path = file.path(datadir, 'new-logger-data/hobo'),
                   pattern= '*.csv',
                   full.names = TRUE,
                   recursive = TRUE) 

## test file
# filz <- "/Users/dhardy/Dropbox/r_data/sapelo/water-level//new-logger-data/hobo/site06_1316_190522-191022.csv"

filz.ve <- list.files(path = file.path(datadir, 'new-logger-data/vanessen'),
                   pattern= '*.CSV',
                   full.names = TRUE,
                   recursive = TRUE) 

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
## read test file
# tf <- read.csv(paste0(datadir, 'new-logger-data/hobo/site06_1316_190313-190522.csv'), skip = 1)

## import & tidy hobo water level data
## note water level C is in meters and indicates water level in reference to top of wellcap (negative numbers indicate below for Hobo)
tidal <- NULL
for(i in 1:length(filz)) {
  OUT <- fread(filz[i],
               select = c(2:5),
               col.names = c('date_time_gmt', 'abs_pres_psi', 'water_temp_c', 'water_level_C'),
               stringsAsFactors = FALSE) %>%
    # rename_at(vars(matches("F")), ~ seq_along(.) %>% as.character())
 
             # colnames(OUT) <- ifelse(str_detect(colnames(OUT),'F')==TRUE, OUT[,3]-32, OUT[.3])
    # 
    # id_vec <- colnames(OUT) %>% str_detect("F") %>% OUT[,3]-32*5/9
    # 
    # colnames(OUT) = ifelse(colnames(OUT) %>% str_detect("F") == TRUE, id_vec, colnames(OUT))
    # 
    # OUT2 <- OUT %<% mutate(ifelse(id_vec == TRUE, ))
    # setNames(c('date_time_gmt', 'abs_pres_psi', 'water_temp_c', 'water_level_C'))
  
    # colnames(OUT) <- c('date_time_gmt', 'abs_pres_psi', 'water_temp_c', 'water_level_C')

    slice(., burn:(n())) %>% ## removes first ## and last ## readings
    # slice_head(n = 5) %>% ## removes first # rows
    # slice_tail(n = 7) %>% ## removes last # rows
    mutate(date_time_gmt = if_else(
      str_detect(date_time_gmt, c('AM|PM')), 
      as.POSIXct(date_time_gmt, format = '%m/%d/%y %I:%M:%S %p', tz = 'GMT'),
      as.POSIXct(date_time_gmt, format = '%m/%d/%y %H:%M:%S', tz = 'GMT')),
           date = as.Date(date_time_gmt, format = '%m/%d/%y', tz = 'GMT'),
           site = str_sub(filz[i], -25,-24),
           serial = str_sub(filz[i], -22,-19),
           water_temp_c = as.numeric(water_temp_c),
           water_level_C = as.numeric(water_level_C),
           temp_max = max(water_temp_c)) %>%
    mutate(site = paste('Site', site, sep = '-')) %>%
    # mutate_if(name = case_when(site == wls.info$site ~ wls.info$name, TRUE ~ 'Fail')) %>%
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

## test file for AM/PM and temp F/C
temp <- tidal %>% filter(site == 'Site-06' & date == '2019-05-22')

boxplot(temp_max ~ site, data = tidal)

## select relevant hobo data columns
tidal.01 <- tidal %>%
  mutate(logger = 'hobo') %>%
  select(date_time_gmt, water_temp_c, water_level_C, date, site, name, sitename, logger, serial)

## import & tidy van essen water level data
## note water level C is in meters and indicates water level in reference to top of wellcap (negative numbers indicate below for VE data)
tidal.ve <- NULL
try(
for(i in 1:length(filz.ve)) {
  OUT <- fread(filz.ve[i],
               skip = 51,
               select = c(1:3),
               col.names = c('date_time_gmt', 'water_level_C', 'water_temp_c'),
               stringsAsFactors = FALSE) %>%
    slice(., burn:(n())) %>% ## removes first and last ## readings
    # slice_head(n = 5) %>% ## removes first # rows
    # slice_tail(n = 6) %>% ## removes last # rows
    mutate(
      date_time_gmt = as.POSIXct(date_time_gmt, format = '%Y/%m/%d %H:%M:%S', tz = 'GMT'),
           date = as.Date(date_time_gmt, '%m/%d/%y', tz = 'GMT'),
           site = str_sub(filz.ve[i], -26,-25),
           serial = str_sub(filz.ve[i], -23,-19),
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
                                                                                                  if_else(site == 'Site-19', 'The Trunk',
                                                                                                          if_else(site == 'Site-16', 'South Oakdale',
                                                                                                                  if_else(site == 'Site-18', 'NW Corner',
                                                                                                                          if_else(site == 'Site-20', 'Walker',
                                                                                                                                  if_else(site == 'Site-23', 'Hillery',
                                                                                                                                          if_else(site == 'Site-24', 'Johnson', 
                                                                                                                                                  if_else(site == 'Site-15', 'Oakdale', site)))))))))))))))))) %>%
    mutate(sitename = paste(site, name))
  tidal.ve <- rbind(OUT, tidal.ve)
}
)

## select relevant ve data columns
tidal.ve2 <- tidal.ve %>%
  mutate(logger = 'van essen') %>%
  select(date_time_gmt, water_temp_c, water_level_C, date, site, name, sitename, logger, serial)

## merge hobo and van essan data
tidal1 <- rbind(tidal.01, tidal.ve2)

## import & tidy van essen specific conductivity/salinity data
## note water level C is in meters and indicates water level in reference to top of wellcap (negative numbers indicate below for VE data)
tidal.psu <- NULL
for(i in 1:length(filz.psu)) {
  OUT <- fread(filz.psu[i],
               select = c(2:8),
               col.names = c('date_time_vartz', 'pressure', 'water_temp_c', 'conductivity', 'water_level_C', 'datum_reference', 'salinity'),
               stringsAsFactors = FALSE) %>%
    # slice(., 5:(n()-7)) %>% ## removes first and last ## readings
    mutate(date_time_vartz = ymd_hms(date_time_vartz),
           tz = str_sub(filz.psu[i], -7, -5),
           site = str_sub(filz.psu[i], -30, -29),
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
                                                                                                  if_else(site == 'Site-19', 'The Trunk',
                                                                                                          if_else(site == 'Site-16', 'South Oakdale',
                                                                                                                  if_else(site == 'Site-18', 'NW Corner',
                                                                                                                          if_else(site == 'Site-20', 'Walker',
                                                                                                                                  if_else(site == 'Site-23', 'Hillery',
                                                                                                                                          if_else(site == 'Site-24', 'Johnson', 
                                                                                                                                                  if_else(site == 'Site-15', 'Oakdale', site)))))))))))))))))) %>%
    mutate(sitename = paste(site, name))
  tidal.psu <- rbind(OUT, tidal.psu)
}

options(scipen=999)

## clean up salinity data
tidal.psu2 <- tidal.psu %>%
  mutate(date_time_gmt = as.POSIXct(
    if_else(tz == 'est', date_time_vartz + hours(5),
            if_else(tz == 'edt', date_time_vartz + hours(4), date_time_vartz)
    ))) %>%
  select(date_time_gmt, site, sitename, salinity) %>%
  filter(salinity < 50 | is.na(salinity))

## merge salinity data to water level data 
tidal1.1 <- full_join(tidal1, tidal.psu2, by = c('sitename', 'date_time_gmt')) %>%
  select(!(site.y))

## attach name to elevation data
SN <- wls.info$name

## adding references for water 'level' in NAVD88
tidal2 <- NULL

for (i in 1:length(SN)) {
  
  el2 <- wls.info %>%
  filter(name == SN[[i]])
  
  OUT2 <- tidal1.1 %>%
    filter(name == SN[[i]]) %>%
    mutate(water_level_navd88 = water_level_C + el2$rtkcap_navd88,
           screen_bottom_navd88 = round(el2$rtkcap_navd88 - 0.6096,3), # screens are ~ top 2' of wells
           wellcap_navd88 = el2$rtkcap_navd88)  
  
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
  mutate(sitename_new = paste(site_new, name))

nas <- tidal3.2 %>% filter(is.na(date_time_gmt))

## check for erroneous data points and remove them from data
err <- tidal3.2 %>% filter(water_level_C >= 2 | water_level_C <= -2)
err2 <- err %>% filter(!water_level_C >2 | water_level_C < -2)

## adding average well heights as measured to date
tidal3.21 <- left_join(tidal3.2, msmt.B.avgs)

## clarifying water level in reference to well cap
tidal3.22 <- tidal3.21 %>%
  rename(water_level_wellcap = water_level_C)

## adding average logger lengths as measured to date
## also adding water depth relative to substrate and logger as well as logger level relative to NAVD88
tidal3.23 <- left_join(tidal3.22, msmt.A.avgs, by = join_by(site_new == site_new, serial == serial)) %>%
  mutate(wtr2sbst_depth = well_ht_avg + water_level_wellcap, 
         wtr2lgr_depth = lgr_length_avg + water_level_wellcap,
         lgr_level_navd88 = wellcap_navd88 - lgr_length_avg)

ggplot(err, aes(date_time_gmt, water_level_C, color = site)) + geom_point()  

temp2 <- tidal3.23 %>% filter(site == 'Site-06' & date == '2019-05-22')

## export merged and cleaned data
tidal3.24 <- tidal3.23 %>%
  select(!site) %>%
  rename(site = site_new) %>%
  select(date_time_gmt, date, transect, site, name, type, logger, serial, 
         well_ht_avg, well_ht_sd, well_ht_n, wellcap_navd88,
         lgr_length_avg, lgr_length_sd, lgr_length_n,
         wtr2sbst_depth, wtr2lgr_depth,
         water_level_wellcap, water_level_navd88,
         water_level_navd88, lgr_level_navd88,
         screen_bottom_navd88, salinity, water_temp_c)

write.csv(tidal3.24, paste(datadir, 'wls_data.csv'))

## create metadata file

metadata <- data.frame(variable=names(tidal3.24),
                                description=c('YYYY-MM-DD HH:MM:SS GMT', 'YYYY-MM-DD',
                                              'Transect number', 'Site number', 'Name of site', 'Ditch or Marsh Site Type',
                                              'Logger Brand', 'Logger Serial #', 
                                              'Average height of well', 'Std Deviation of well',
                                              'Count of well height measurements',
                                              'Wellcap level relative to NAVD88', 
                                              'Average length of logger line from wellcap to logger tip (does not account for sensor position on logger', 
                                              'Std Deviation of logger line length',
                                              'Count of logger length measurements',
                                              'Depth of water above substrate/ground',
                                              'Depth of water above logger',
                                              'Water level relative to wellcap',
                                              'Water level relative to NAVD88',
                                              'Logger level relative to NAVD88',
                                              'Bottom of well screen relative to NAVD88 (well screen is approximately two feet below bottom of the wellcap. This measuremnt does not take into account the wellcap height itself, which would raise these measurements by about 8-10cm',
                                              'Salinity in parts per thousand',
                                              'Water temperaute in degrees Celsius'
                                              ))
write.csv(metadata, paste(datadir, 'metadata_wls_data.csv'))



#################################
## tidy data for SINERR report
#################################
# sinerr <- tidal3.2 %>%
#   filter(date >= '2022-01-01' & date <= '2023-06-30') %>%
#   mutate(water_level_mhhw = if_else(site_new == "T5-01", water_level_navd88 - 1.021, water_level_navd88 - 0.969),
#          water_level_mllw = water_level_mhhw + 2.264) %>%
#   rename(transect_site = site_new, water_level_cap = water_level_C, water_depth = water_depth_m) %>%
#   select(date, transect_site, type, logger, serial, well_ht, water_level_cap, water_depth, water_level_navd88, water_level_mhhw, water_level_mllw, salinity)
# 
# write.csv(sinerr, paste(datadir, 'cwbp_wls_data_20220701-20230630.csv'))
