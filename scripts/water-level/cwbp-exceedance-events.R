rm(list=ls())

library(tidyverse)
library(lubridate)
library(data.table)
library(readxl)
library(dataRetrieval)
library("rio")
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
         date = as.POSIXct(date)) 

## monthly high water means
df.mhhw <- df %>%
  mutate(date = floor_date(date_time_gmt, "day")) %>%
  group_by(transect, site_new, date) %>%
  summarise(max = max(water_level_navd88)) %>%
  mutate(mhhw_max_ft = max*3.28084+3.9566929) %>% ## VDATUM for Cabretta Creek mouth near HH community
  rename(site = site_new)

########################################################################################################
## adding automated download process for USGS data at Meridian to compare to CWBP 
########################################################################################################

# Hudson River, Meridian, GA
siteNo <- "022035975"
pCode <- "00065" ## gage height data
statCode <- "00021" ## tidal high-high values
start.date <- "2019-01-01" ## earliest available date
end.date <- "2024-01-01"

df2 <- readNWISdv(siteNumbers = siteNo,
                 parameterCd = pCode,
                 startDate = start.date,
                 endDate = end.date,
                 statCd = statCode) %>%
  rename(mhhw_max_ft = X_00065_00021,
         quality = X_00065_00021_cd,
         date = Date,
         site = site_no,
         transect = agency_cd) %>%
  mutate(type = 'high', max = 0) %>%
  select(transect, site, date, max, mhhw_max_ft) %>%
  mutate(mhhw_max_ft = mhhw_max_ft + 4.18)

df2$date <- as.POSIXct(df2$date) ## convert datetime column to correct format

df.mhhw2 <- rbind(df.mhhw, df2)

############################################
## count number of events above flood stage
############################################
df4 <- df.mhhw2 %>%
  filter(mhhw_max_ft > (9.2) & site %in% c('T1-02', 'T3-02', 'T4-01', '022035975') & date < '2024-01-01' & date > '2019-01-01' ) %>%
  mutate(x = floor_date(date, "year")) %>%
  mutate(x = year(x)) %>%
  group_by(site, x) %>%
  summarise(y = n())

## https://stackoverflow.com/questions/37329074/geom-smooth-and-exponential-fits
linear.model <-lm(y ~ x, df4)
log.model <-lm(log(y) ~ x, df4)
# exp.model <-lm(y ~ exp(x), df4)

log.model.df <- data.frame(x = df4$x,
                           y = exp(fitted(log.model)))

ext <- ggplot(df4, aes(x, y, label = y)) + 
  geom_line(color = 'blue') +
  # geom_smooth(method="lm", aes(color="Exp Model"), formula= (y ~ exp(x)), se=FALSE, linetype = 1) +
  # geom_line(data = log.model.df, aes(x, y, color = "Log Model"), size = 1, linetype = 1, show.legend = F) + 
  # guides(color = guide_legend("Model Type")) + 
  geom_label() + 
  # geom_text(size = 10) + 
  scale_x_continuous(breaks = seq(2019, 2023, 1), minor_breaks = seq(2019,2023,1), limits = c(2019, 2023)) + 
  scale_y_continuous() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.position = 'none',
        text = element_text(size = 24),
        panel.background = element_rect(color = 'grey10', fill = 'white', linewidth = 0.5),
        panel.grid = element_line(color = 'grey90')) +  
  labs(x = 'Year', y = '# Events > 9.2 ft') + 
  facet_wrap('site')
ext

# tiff(file.path(datadir, 'figures/cwbp_t1-02action_stage.tiff'), res = 300, unit = 'in',
#      width = 6.5, height = 4, compression = 'lzw')
# ext
# dev.off()
# 
png(file.path(datadir, 'figures/cwbp_gt9.2ft_slide.png'), res = 150, unit = 'in',
    width = 13.33, height = 6.5)
ext
dev.off()
