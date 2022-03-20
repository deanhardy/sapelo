rm(list=ls())

library(tidyverse)
library(httr)
library(jsonlite)
library(lubridate)
Sys.setenv(TZ='GMT')

## https://www.dataquest.io/blog/r-api-tutorial/
## https://aa.usno.navy.mil/data/api#phase

## define data directory
datadir <- '/Users/dhardy/Dropbox/r_data/sapelo/water-level/'

lunar <- NULL
YR <- c("https://aa.usno.navy.mil/api/moon/phases/year?year=2002",
        "https://aa.usno.navy.mil/api/moon/phases/year?year=2018",
       "https://aa.usno.navy.mil/api/moon/phases/year?year=2019",
       "https://aa.usno.navy.mil/api/moon/phases/year?year=2020",
       "https://aa.usno.navy.mil/api/moon/phases/year?year=2021",
       "https://aa.usno.navy.mil/api/moon/phases/year?year=2022")

for (i in 1:length(YR)) {
  
res = GET(YR[i])

data = fromJSON(rawToChar(res$content))

OUT <- data$phasedata %>%
  mutate(date = as.Date(with(., paste(year, month, day, time, sep = '/')), 
                                 format = '%Y/%m/%d')) %>%
  mutate(date_time_gmt = ymd_hm(paste(date, time))) %>%
  select(date_time_gmt, phase)

lunar <- rbind(OUT, lunar)
}

write.csv(lunar, file.path(datadir, 'lunar.csv'))
