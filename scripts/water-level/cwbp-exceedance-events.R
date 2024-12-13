rm(list=ls())

library(tidyverse)
library(lubridate)
library(data.table)
library(readxl)
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
  mutate(prd = floor_date(date_time_gmt, "day")) %>%
  group_by(transect, site_new, prd) %>%
  summarise(max = max(water_level_navd88)) %>%
  mutate(mhhw_max_ft = max*3.28084+4.177)

############################################
## count number of events above flood stage
############################################
df4 <- df.mhhw %>%
  filter(mhhw_max_ft > (10.2-0.58)) %>% ## flood stage = 10.2 feet
  mutate(x = floor_date(prd, "year")) %>%
  mutate(x = year(x)) %>%
  group_by(x) %>%
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
  geom_line(data = log.model.df, aes(x, y, color = "Log Model"), size = 1, linetype = 1, show.legend = F) + 
  # guides(color = guide_legend("Model Type")) + 
  geom_label() + 
  # geom_text(size = 10) + 
  scale_x_continuous(breaks = seq(2018, 2024, 2), minor_breaks = seq(2018,2024,1), limits = c(2018, 2024)) + 
  scale_y_continuous(breaks = seq(0,100,10)) + 
  theme_bw(base_size = 24) + 
  labs(x = 'Year', y = 'Action Stage Events (#)')
ext

tiff(file.path(datadir, 'figures/cwbp_t1-02action_stage.tiff'), res = 300, unit = 'in',
     width = 6.5, height = 4, compression = 'lzw')
ext
dev.off()

png(file.path(datadir, 'figures/meridian_landing_action_stage_slide.png'), res = 150, unit = 'in',
    width = 13.33, height = 7)
ext
dev.off()