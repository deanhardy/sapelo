#######################################
## comparing mean high water of sites to Meridian landing
#######################################
rm(list=ls())

library(tidyverse)
library(lubridate)
library(readxl)
library(dataRetrieval) ## https://cran.r-project.org/web/packages/dataRetrieval/vignettes/dataRetrieval.html
Sys.setenv(TZ='GMT')

## define data directory
datadir <- '/Users/dhardy/Dropbox/r_data/sapelo/water-level/'

######################
## import & tidy data
#####################

## define column classes
## import cleaned water level data
df <- read_csv(paste(datadir, 'wls_data.csv'))[,-1] %>%
  mutate(date_time_gmt = as.POSIXct(date_time_gmt, format = "%Y-%m-%d %H:%M:%S", tz = 'GMT'),
         date = as.POSIXct(date)) %>%
  arrange(date_time_gmt)

# Hudson River, Meridian, GA
siteNo <- "022035975"
pCode <- "00065" ## gage height data
statCode <- "00021" ## tidal high-high values
start.date <- first(df$date) ## earliest available date
end.date <- last(df$date)

ml <- readNWISdv(siteNumbers = siteNo,
                 parameterCd = pCode,
                 startDate = start.date,
                 endDate = end.date,
                 statCd = statCode) %>%
  rename(water_level_navd88 = X_00065_00021,
         quality = X_00065_00021_cd,
         date = Date) %>%
  mutate(type = 'high', water_level_navd88 = water_level_navd88 * 0.3048)

ml$date <- as.Date(ml$date) ## convert datetime column to correct format

## monthly high water means at sites
df.mhhw <- df %>%
  mutate(prd = floor_date(date_time_gmt, "day")) %>%
  group_by(transect, site_new, prd) %>%
  summarise(max = max(water_level_navd88)) %>%
  mutate(month = floor_date(prd, "month")) %>%
  group_by(transect, site_new, month) %>%
  summarize(avg = mean(max)) %>%
  mutate(source = 'CWBP')

## monthly high water means at ML
ml.mhhw <- ml %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month) %>%
  summarize(avg = mean(water_level_navd88)) %>%
  mutate(transect = 'Hudson Creek', site_new = 'ML', source = 'USGS') %>%
  select(transect, site_new, month, avg, source)

mhhw <- rbind(df.mhhw, ml.mhhw)
  
ggplot(mhhw, aes(month, avg, group = site_new)) + 
  geom_point(aes(color = site_new))
  # geom_smooth(method = lm, se = F) + 
  # facet_wrap(~ transect)

