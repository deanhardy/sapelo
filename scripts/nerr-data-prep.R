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

## import NERR Wx data for Sapelo @ Marsh Landing
nerr_wx <- read.csv(file.path(datadir, 'water-level/nerr-data/sapmlmet-data/220221-sapmlmet/SAPMLMET.csv'),
           header = TRUE, skip = 2, stringsAsFactors = FALSE) %>%
    slice(., 1:n()) %>%
  mutate(date_time_gmt = with_tz(mdy_hm(DateTimeStamp, tz = 'EST')),
         BP = as.numeric(BP),
         TP = as.numeric(TotPrcp),
         Temp = as.numeric(ATemp)) %>%
  select(date_time_gmt, BP, TP, Temp) %>%
  drop_na()

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
nerr_wx3 <- nerr_wx2.2 %>%
  mutate(Date = as.Date(as.character(x)), Time = format(x, '%H:%M:%S'), pres = y.x) %>%
  mutate(Date = format(Date,'%m/%d/%y')) %>%
  mutate(Date = noquote(Date)) %>%
  mutate(temp = y.y) %>%
  mutate(date_time_gmt = as.POSIXct(x, format = '%m/%d/%y %H:%M:%S')) %>%
  mutate(date.est = date_time_gmt - hours(5)) %>%
  select(date_time_gmt, date.est, Date, Time, pres, temp)

colnames(nerr_wx3) <- c(' Date (GMT)', 'Date (EST)', 'Date', 'Time (GMT)', 'pres (mbar)', 'temp (C)')

nerr_wx4 <- nerr_wx3 %>%
  filter(date_time_gmt >= first('2018-10-12 00:00:00') & date_time_gmt <= Sys.time())
  
write.table(nerr_wx4, file.path(datadir, 'water-level/nerr-data/SAPMLMETADJ.txt'), sep = ',', row.names = FALSE, col.names = TRUE,
            quote = FALSE)

