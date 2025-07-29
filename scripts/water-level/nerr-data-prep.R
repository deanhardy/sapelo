##############################
## Author: Dean Hardy
## Purpose: Configures NERR barometric pressure data available in 15 min intervals from https://cdmo.baruch.sc.edu/ 
##    to match pressure transducer loggers in field at 12 min intervals via interpolations
## Requested citation format: NOAA National Estuarine Research Reserve System (NERRS). 
##    System-wide Monitoring Program. Data accessed from the NOAA NERRS Centralized Data Management Office 
##    website: http://cdmo.baruch.sc.edu/. 
##############################

rm(list=ls())

library(tidyverse)
library(lubridate)
library(timetk)
Sys.setenv(TZ='GMT')

## define data directory
datadir <- '/Users/dhardy/Dropbox/r_data/sapelo'

## import "best available" NERR Wx data for Sapelo @ Marsh Landing
nerr_wx_ba <- read.csv(file.path(datadir, 'water-level/nerr-data/sapmlmet-data/250729-sapmlmet/SAPMLMET.csv'),
    header = TRUE, skip = 2, stringsAsFactors = FALSE) %>%
    slice(., 1:n()) %>%
  mutate(date_time_gmt = with_tz(mdy_hm(DateTimeStamp, tz = 'EST')),
         BP = as.numeric(BP),
         F_BP = (F_BP),
         TP = as.numeric(TotPrcp),
         Temp = as.numeric(ATemp),
         quality = 'best available') %>%
  select(date_time_gmt, BP, TP, Temp, quality) %>%
  filter(BP > 990) %>% ## filters erroneous data on 3/7/22
  # filter(!str_detect(F_BP, '<-3>|<-4>')) %>% ## filters rejected data, but keeps suspect data
  # filter_by_time(date_time_gmt, .start_date = '2022-03-07 00:00:00', .end_date = '2022-03-07 23:59:00') %>%
  drop_na()

## import "all inclusive" NERR Wx data for Sapelo @ Marsh Landing
## run all inclusive only if necessary ie missing data
## as of 7/29/25, decided to always run all inclusive and "patch" it into best available dataset 
## All inclusive missing dates used are "10/21/20 16:45:00" to "06/26/21 17:15:00" during which the BA data set reads BP as 102.1
nerr_wx_ai <- read.csv(file.path(datadir, 'water-level/nerr-data/sapmlmet-data/250729-sapmlmet-allinclusive/SAPMLMET.csv'),
                       header = TRUE, skip = 2, stringsAsFactors = FALSE) %>%
  slice(., 1:n()) %>%
  mutate(date_time_gmt = with_tz(mdy_hm(DateTimeStamp, tz = 'EST')),
         BP = as.numeric(BP),
         F_BP = (F_BP),
         TP = as.numeric(TotPrcp),
         Temp = as.numeric(ATemp),
         quality = 'all inclusive') %>%
  select(date_time_gmt, BP, TP, Temp, quality) %>%
  filter_by_time(date_time_gmt, .start_date = '2020-10-21 16:45:00', .end_date = '2021-06-26 17:15:00') %>%
  drop_na()

nerr_wx <- rbind(nerr_wx_ba, nerr_wx_ai)

## calculate and export total daily precipitation values in mm 
totprcp <- nerr_wx %>%
  select(date_time_gmt, TP) %>%
  summarise_by_time(
    .date_var = date_time_gmt,
    .by = 'day',
    value = sum(TP))

TP <- totprcp %>%
  rename(TP_mm = value) %>%
  mutate(date_time_gmt = date_time_gmt + hours(12) + minutes(00) + seconds(00))

write.csv(TP, file.path(datadir, 'water-level/nerr-data/SAPMLMET_TP.csv')) 

## create datetime sequence that matches logger datetime stamps
## then interpolate baro press values and export for use in HOBOware
date_time_gmt <- seq(as.POSIXct('2018-10-01 05:00:00', tz = 'UTC'), as.POSIXct(Sys.Date()), 
                     by = '12 mins')
date_time_gmt <- seq(as.POSIXct('2018-10-01 05:00:00', tz = 'UTC'), as.POSIXct(last(nerr_wx$date_time_gmt)), 
                     by = '12 mins')
tail(date_time_gmt)

lgr_ts <- as.data.frame(date_time_gmt)

## interpolate values
ip_pres_values <- approx(nerr_wx$date_time_gmt, nerr_wx$BP, xout = lgr_ts$date_time_gmt, 
                    rule = 2, method = "linear", ties = mean)
ip_temp_values <- approx(nerr_wx$date_time_gmt, nerr_wx$Temp, xout = lgr_ts$date_time_gmt, 
                         rule = 2, method = "linear", ties = mean)

nerr_wx2 <- data.frame(ip_pres_values)
nerr_wx2.1 <- data.frame(ip_temp_values)
nerr_wx2.2 <- merge(nerr_wx2, nerr_wx2.1, by = "x")


## prep formatting to match HOBO requirements
nerr_wx3h <- nerr_wx2.2 %>%
  mutate(Date = as.Date(as.character(x)), Time = format(x, '%H:%M:%S'), pres = y.x) %>%
  mutate(Date = format(Date,'%m/%d/%y')) %>%
  # mutate(Date = noquote(Date)) %>%
  mutate(temp = y.y) %>%
  mutate(date_time_gmt = as.POSIXct(x, format = '%m/%d/%y %H:%M:%S')) %>%
  mutate(date.est = date_time_gmt - hours(5)) %>%
  select(Date, Time, pres) # for HOBOs

colnames(nerr_wx3h) <- c('Date', 'Time (GMT)', 'pres (mbar)') ## for HOBOs

write.table(nerr_wx3h, file.path(datadir, 'water-level/nerr-data/SAPMLMETADJ-hobo.txt'), sep = ',', row.names = FALSE, col.names = TRUE,
            quote = FALSE)


## prep formatting to match Diver requirements
nerr_wx3d <- nerr_wx2.2 %>%
  mutate(Date = as.Date(as.character(x)), Time = format(x, '%H:%M:%S'), pres = y.x) %>%
  mutate(Date = format(Date,'%m/%d/%y')) %>%
  mutate(Date = noquote(Date)) %>%
  mutate(temp = y.y) %>%
  mutate(date_time_gmt = as.POSIXct(x, format = '%m/%d/%y %H:%M:%S')) %>%
  mutate(date.est = date_time_gmt - hours(5), date.edt = date_time_gmt - hours(4)) %>%
  select(date_time_gmt, date.est, date.edt, Date, Time, pres, temp) ## for Divers

colnames(nerr_wx3d) <- c(' Date (GMT)', 'Date (EST)', 'Date (EDT)', 'Date', 'Time (GMT)', 'pres (mbar)', 'temp (C)') ## for Divers

nerr_wx4d <- nerr_wx3d %>%
  filter(date_time_gmt >= first('2018-10-12 00:00:00') & date_time_gmt <= Sys.time())

write.table(nerr_wx4d, file.path(datadir, 'water-level/nerr-data/SAPMLMETADJ-diver.txt'), sep = ',', row.names = FALSE, col.names = TRUE,
            quote = FALSE)

