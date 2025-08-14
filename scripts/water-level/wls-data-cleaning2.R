rm(list=ls())

library(tidyverse)
library(lubridate)
library(data.table)
library("rio")
library(readxl)
library(zoo)
Sys.setenv(TZ='GMT')
## define data directory
datadir <- '/Users/dhardy/Dropbox/r_data/sapelo/water-level/'

## set # measurements to "burn" pre and post data download
burn = 0

#####################################################
### import field data measurements & assess averages ###
#####################################################

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

#######################################################
## import logger sensor depth data files and tidy
#######################################################
filz <- list.files(path = file.path(datadir, 'new-logger-data/hobo-sensor-depth'),
                   pattern= '*.csv',
                   full.names = TRUE,
                   recursive = F)

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
# temp <- fread(paste0(datadir, 'new-logger-data/hobo-sensor-depth/t102_0000_181013-250426.csv'),
#               select = c(2:5),
#               col.names = c('date_time_gmt', 'abs_pres_psi', 'water_temp_c', 'sensor_depth'),
#               stringsAsFactors = FALSE)
  
## import & tidy hobo water level data
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
      transect = toupper(str_sub(filz[i], -27,-26)),
      site = toupper(paste0(str_sub(filz[i], -27,-26), '-', str_sub(filz[i], -25,-24))),
      serial = str_sub(filz[i], -22,-19),
      water_temp_c = as.numeric(water_temp_c),
      sensor_depth = as.numeric(sensor_depth),
      temp_max = max(water_temp_c)) %>%
    select(!(abs_pres_psi))
  tidal <- rbind(OUT, tidal)
}

## select relevant hobo data columns
tidal.01 <- tidal %>%
  mutate(logger = 'hobo',
         site_serial = paste0(site, ' (', serial, ')')) %>%
  select(date_time_gmt, water_temp_c, sensor_depth, date, transect, site, type, logger, serial, site_serial)

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
      mutate(type = ifelse(site %in% c('Site-13', 'Site-11', 'Site-09', 'Site-05', 'Site-19', 'Site-14', 'Site-15', 'Site-18', 'Site-20', 'Site-23', 'Site-24'), 'ditch',
                           ifelse(site %in% c('Site-02', 'Site-03', 'Site-07', 'Site-06', 'Site-12', 'Site-16'), 'creek', site))) %>% ## ditches sites
      # filter(site %in% c('Site-02', 'Site-03', 'Site-07', 'Site-06', 'Site-12')) %>% ## marsh sites
      mutate(transect = ifelse(site %in% c('Site-06', 'Site-12', 'Site-19', 'Site-13', 'Site-18'), 'T1',
                               ifelse(site %in% c('Site-02', 'Site-03', 'Site-05','Site-11', 'Site-23'), 'T3',
                                      ifelse(site %in% c('Site-07', 'Site-09', 'Site-24'), 'T4',
                                             ifelse(site %in% c('Site-16', 'Site-15'), "T5",
                                                    ifelse(site %in% c('Site-20'), 'T2',
                                                           if_else(site %in% c('Site-14'), 'T6', site))))))) %>%
    mutate(site = if_else(site == 'Site-06', 'T1-01',
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
      mutate(site_serial = paste0(site, ' (', serial, ')'))
    tidal.ve <- rbind(OUT, tidal.ve)
  }
)

## select relevant ve data columns
tidal.ve2 <- tidal.ve %>%
  mutate(logger = 'van essen') %>%
  select(date_time_gmt, water_temp_c, water_level_C, date, transect, site, type, logger, serial, site_serial)

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
           water_level_C = as.numeric(water_level_C)/1000 * -1,
           serial = 'X0976') %>%
    mutate(site = paste('Site', site, sep = '-')) %>%
    mutate(site = ifelse(site %in% c('Site-13', 'Site-11', 'Site-09', 'Site-05', 'Site-19', 'Site-14', 'Site-15', 'Site-18', 'Site-20', 'Site-23', 'Site-24'), 'ditch',
                         ifelse(site %in% c('Site-02', 'Site-03', 'Site-07', 'Site-06', 'Site-12', 'Site-16'), 'creek', site))) %>% ## ditches sites
    # filter(site %in% c('Site-02', 'Site-03', 'Site-07', 'Site-06', 'Site-12')) %>% ## marsh sites
    mutate(transect = ifelse(site %in% c('Site-06', 'Site-12', 'Site-19', 'Site-13', 'Site-18'), 'T1',
                             ifelse(site %in% c('Site-02', 'Site-03', 'Site-05','Site-11', 'Site-23'), 'T3',
                                    ifelse(site %in% c('Site-07', 'Site-09', 'Site-24'), 'T4',
                                           ifelse(site %in% c('Site-16', 'Site-15'), "T5",
                                                  ifelse(site %in% c('Site-20'), 'T2',
                                                         if_else(site %in% c('Site-14'), 'T6', site))))))) %>%
    mutate(site = if_else(site == 'Site-06', 'T1-01',
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
    mutate(site_serial = paste0(site, ' (', serial, ')'))

  tidal.psu <- rbind(OUT, tidal.psu)
}

options(scipen=999)

## clean up salinity data
tidal.psu2 <- tidal.psu %>%
  mutate(date_time_gmt = as.POSIXct(
    if_else(tz == 'est', date_time_vartz + hours(5),
            if_else(tz == 'edt', date_time_vartz + hours(4), date_time_vartz)
    ))) %>%
  select(date_time_gmt, site, salinity) %>%
  filter(salinity < 50 | is.na(salinity))

## merge salinity data to water level data 
tidal1.1 <- full_join(tidal1, tidal.psu2, by = c('site', 'date_time_gmt')) %>%
  select(!(site.y))

## attach name to elevation data
SN <- msmt.A.avgs$site_serial

#### adding reference levels in NAVD88 ####
# test 2
df <- NULL

for (i in 1:length(SN)) {
  
  el2 <- msmt.A.avgs3 %>%
    filter(site_serial == SN[[i]])
  
  OUT2 <- tidal.01 %>%
    filter(site_serial == SN[[i]]) %>%
    mutate(
           wellcap_navd88 = el2$rtkcap_navd88,
           subst_navd88 = el2$rtkcap_navd88 - el2$well_ht_avg,
           screen_bottom_navd88 = round(el2$rtkcap_navd88 - 0.6096,3), # screens are ~ top 2' of wells
           sensor_navd88 = el2$rtkcap_navd88 - (el2$lgr_length_avg - 0.0026) ## sensor face is 2.6 cm above logger tip
           ) %>%
    mutate(water_level_navd88 = sensor_navd88 + sensor_depth) %>%
    mutate(water_depth = water_level_navd88 - subst_navd88)
  
  df <- rbind(OUT2, df)
}

## still need to merge average field measurements, water depth, and cleanup columns

## filter to download date
df2 <- df %>%
  # mutate(ma1hr = rollmean(water_level_navd88, k=5, fill=NA, align = 'center')) %>%
  filter(date_time_gmt >= "2022-10-20 12:00:00" & date_time_gmt <= "2022-11-03 12:00:00") 

library(geomtextpath)
TEXT = 15 ## set font size for figures

ggplot(df2)  + 
  geom_line(aes(date_time_gmt, water_level_navd88, color = site)) +  
  # geom_vline(aes(xintercept = GMT), data = dl, lty = 'dashed') +
  # geom_textvline(aes(xintercept = GMT), label = "data download", hjust = 0.8,
  #                vjust = 1.3, color = "blue4", data = dl, show.legend = F) +
  # geom_texthline(aes(yintercept = 0), label = "wellcap",
  #                hjust = 0.9, color = "grey70", linetype = 2, data = df3, show.legend = F) + ## wellcap
  geom_texthline(aes(yintercept = subst_navd88, color = site), label = "avg substrate",
                 hjust = 0.9, data = df2, show.legend = F) + ## substrate relative to wellcap
  # geom_texthline(aes(yintercept = -0.61, color = site), label = "bottom of screen",
  #                hjust = 0.9, data = df2, show.legend = F) + ## bottom of screen relative to wellcap
  # geom_texthline(aes(yintercept =  0-lgr_length_avg), label = "tip of logger",
  #                hjust = 0.9, color = "grey30", data = df3, show.legend = F) + ## tip of logger relative to wellcap
  # scale_fill_manual(values = c('white', 'black')) + 
  scale_x_datetime(name = 'Day', date_breaks = '1 day', date_labels = '%m/%d/%y') + 
  scale_y_continuous(name = 'Water Level (m NAVD88)', 
                     breaks = seq(0,1.8,0.2), limits = c(0,1.8), expand = c(0,0)) +
  theme(axis.title = element_text(size = TEXT),
        axis.text = element_text(color = "black", size = TEXT),
        axis.ticks.length = unit(-0.2, 'cm'),
        axis.ticks = element_line(color = 'black'),
        axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), 
        axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")),
        axis.line = element_line(color = 'black'),
        panel.background = element_rect(fill = FALSE, color = 'black'),
        panel.grid = element_blank(),
        panel.grid.major.x = element_line('grey', linewidth = 0.5, linetype = "dotted"),
        plot.margin = margin(0.5,0.5,0.5,0.5, 'cm'),
        plot.title = element_text(size = TEXT, face = "bold"))
  # ggtitle(paste0(sites_dates[i], ', Logger Accuracy: ', df3$accuracy, 'm', ', Abs Diff: ', df3$abs_diff, 'm'))
