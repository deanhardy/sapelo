rm(list=ls())

library(tidyverse)
library(lubridate)
Sys.setenv(TZ='GMT')

## define data directory
datadir <- 'C:/Users/dhardy/Dropbox/r_data/sapelo'

## import and combine NERR Wx data
nerr_wx <- rbind(
  read.csv(file.path(datadir, 'water-level/nerr-data/marshlanding_realtime_jan18-nov18.csv'),
           header = TRUE, skip = 2, stringsAsFactors = FALSE) %>%
    slice(., 3:n()),
  read.csv(file.path(datadir, 'water-level/nerr-data/marshlanding_realtime_dec18.csv'),
           header = TRUE, skip = 2, stringsAsFactors = FALSE) %>%
    slice(., 3:n()),
  read.csv(file.path(datadir, 'water-level/nerr-data/marshlanding_realtime_jan19.csv'),
           header = TRUE, skip = 2, stringsAsFactors = FALSE) %>%
    slice(., 3:n())
) %>%
  mutate(date_time_gmt = ymd_hms(Date, tz = 'UTC'),
         Baro_Pressure = as.numeric(Baro_Pressure)) %>%
  filter(date_time_gmt >= first('2018-10-13 00:00:00') & date_time_gmt <= Sys.time()) %>%
  select(date_time_gmt, Baro_Pressure)

## create datetime sequence that matches logger datetime stamps
## then interpolate baro press values and export for use in HOBOware
date_time_gmt <- seq(as.POSIXct('2018-10-13 00:00:00', tz = 'UTC'), as.POSIXct(Sys.Date()), 
                     by = '12 mins')

lgr_ts <- as.data.frame(date_time_gmt)

ip_values <- approx(nerr_wx$date_time_gmt, nerr_wx$Baro_Pressure, xout = lgr_ts$date_time_gmt, 
                    rule = 2, method = "linear", ties = mean)

nerr_wx2 <- data.frame(ip_values)

## prep formatting to match HOBO requirements
nerr_wx3 <- nerr_wx2 %>%
  mutate(Date = as.Date(x), Time = format(x, '%H:%M:%S'), pres = y) %>%
  select(Date, Time, pres) 

colnames(nerr_wx3) <- c('Date', 'Time', 'pres (mbar)')

write.csv(nerr_wx3, file.path(datadir, 'water-level/nerr-data/nerr-wx-adj.csv'), row.names = FALSE)
