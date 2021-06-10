##############################
## Author: Dean Hardy
## Purpose: Configures NERR barometric pressure data available in 15 min intervals from https://cdmo.baruch.sc.edu/ 
##    to match pressure transducer loggers in field at 12 min intervals via interpolations
## Requested citation format: NOAA National Estuarine Research Reserve System (NERRS). 
##    System-wide Monitoring Program. Data accessed from the NOAA NERRS Centralized Data Management Office 
##    website: http://cdmo.baruch.sc.edu/; accessed 08 July 2020. 
##############################

rm(list=ls())

library(tidyverse)
library(lubridate)
Sys.setenv(TZ='GMT')

## define data directory
datadir <- '/Users/dhardy/Dropbox/r_data/sapelo'

## import NERR Wx data for Sapelo @ Marsh Landing
nerr_wx <- read.csv(file.path(datadir, 'water-level/nerr-data/sapmlmet-data/200708-sapmlmet/SAPMLMET.csv'),
           header = TRUE, skip = 2, stringsAsFactors = FALSE) %>%
    slice(., 1:n()) %>%
  mutate(date_time_gmt = with_tz(mdy_hm(DateTimeStamp, tz = 'EST')),
         BP = as.numeric(BP)) %>%
# filter(date_time_gmt >= first('2018-10-13 00:00:00') & date_time_gmt <= Sys.time()) %>%
  select(date_time_gmt, BP)

## create datetime sequence that matches logger datetime stamps
## then interpolate baro press values and export for use in HOBOware
date_time_gmt <- seq(as.POSIXct('2018-10-01 05:00:00', tz = 'UTC'), as.POSIXct(Sys.Date()), 
                     by = '12 mins')

lgr_ts <- as.data.frame(date_time_gmt)

## interpolate values
ip_values <- approx(nerr_wx$date_time_gmt, nerr_wx$BP, xout = lgr_ts$date_time_gmt, 
                    rule = 2, method = "linear", ties = mean)

nerr_wx2 <- data.frame(ip_values)

## prep formatting to match HOBO requirements
nerr_wx3 <- nerr_wx2 %>%
  mutate(Date = as.Date(as.character(x)), Time = format(x, '%H:%M:%S'), pres = y) %>%
  mutate(Date = format(Date,'%m/%d/%y')) %>%
  mutate(Date = noquote(Date)) %>%
  select(Date, Time, pres)

colnames(nerr_wx3) <- c('Date', 'Time', 'pres (mbar)')

write.table(nerr_wx3, file.path(datadir, 'water-level/nerr-data/SAPMLMETADJ.txt'), sep = ',', row.names = FALSE, col.names = TRUE,
            quote = FALSE)
# write.csv(nerr_wx3, file.path(datadir, 'water-level/nerr-data/SAPMLMETADJ.csv'), row.names = FALSE)

