################################################################
## comparing tide of CWBP sites against other projects
################################################################
rm(list=ls())

library(tidyverse)
library(lubridate)
library(readxl)
library(dataRetrieval) ## https://cran.r-project.org/web/packages/dataRetrieval/vignettes/dataRetrieval.html
Sys.setenv(TZ='GMT')
# options(scipen=999)

## define data directory
datadir <- '/Users/dhardy/Dropbox/r_data/sapelo/water-level/'

# set dates for transect graphs
int.date1 <- as.Date('2022-11-08') 
int.date2 <- as.Date('2022-11-12')

# define transects
TR1 <- c('ML', 'T1-01', 'T1-02', 'T1-03', 'T1-04', 'T1-05', 'T5-02')
TR3 <- c('ML', 'T3-01', 'T3-02', 'T3-03','T3-04', 'T2-01')
TR4 <- c('ML', 'T4-01', 'T4-02', 'T4-BR-01')


######################
## import & tidy data
#####################

## define column classes
## import cleaned water level data
df <- read_csv(paste(datadir, 'wls_data.csv'))[,-1] %>%
  mutate(date_time_gmt = as.POSIXct(date_time_gmt, format = "%Y-%m-%d %H:%M:%S", tz = 'GMT'),
         date = as.POSIXct(date)) %>%
  arrange(date_time_gmt) %>%
  select(date_time_gmt, site_new, water_level_navd88)

# Hudson River, Meridian, GA
siteNo <- "022035975"
pCode <- "00065" ## gage height data
start.date <- as.Date(first(df$date_time_gmt)) ## earliest available date
end.date <- as.Date(last(df$date_time_gmt))

## trying to work out how to dl realtime data in 15 min intervals
ml <- readNWISuv(siteNumbers = siteNo,
                    parameterCd = pCode,
                    startDate = start.date,
                    endDate = end.date) %>%
  rename(water_level_navd88 = X_00065_00000,
         quality = X_00065_00000_cd,
         date_time_gmt = dateTime) %>%
  mutate(water_level_navd88 = water_level_navd88 * 0.3048,
         site_new = 'ML') %>%
  select(date_time_gmt, site_new, water_level_navd88)

# ml$date <- as.Date(ml$date) ## convert datetime column to correct format

df2 <- rbind(df, ml)

## set dates and sites
df3 <- filter(df2, site_new %in% TR1 & date_time_gmt >= int.date1 & date_time_gmt <= int.date2)

TEXT = 8
plot <- 
  ggplot(df3)  + 
  geom_line(aes(date_time_gmt, water_level_navd88, color = site_new)) +
  scale_x_datetime(name = 'Date', date_breaks = '12 hours', date_minor_breaks = '1 hour', date_labels = '%m/%d/%y %H:%M') + 
  scale_y_continuous(name = 'Water Level (m NAVD88)', minor_breaks = seq(-1.5,2,5.1), breaks = seq(-1.5,2.5,0.5), limits = c(-1.5,2.5), expand = c(0,0)) +
  labs(color='Site') +
  theme(axis.title = element_text(size = TEXT),
        axis.text = element_text(color = "black", size = TEXT),
        axis.ticks.length = unit(-0.2, 'cm'),
        axis.ticks = element_line(color = 'black'),
        axis.text.x = element_text(angle = 45, vjust = 1.2, hjust=1, margin=unit(c(0.5,0.5,0.5,0.5), "cm")), 
        axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")),
        axis.line = element_line(color = 'black'),
        panel.background = element_rect(fill = FALSE, color = 'black'),
        panel.grid = element_blank(),
        panel.grid.major.x = element_line('grey30', size = 0.5, linetype = "solid"),
        panel.grid.minor.x = element_line('grey95', size = 0.5, linetype = "solid"),
        panel.grid.major.y = element_line('grey30', size = 0.5, linetype = "solid"),
        panel.grid.minor.y = element_line('grey95', size = 0.5, linetype = "solid"),
        plot.margin = margin(0.5,0.5,0.5,0.5, 'cm'),
        # legend.position = c(0.1, 0.94),
        legend.text = element_text(size = TEXT),
        legend.title = element_text(size = TEXT),
        # legend.key = element_blank(),
        legend.box.background = element_rect(color = 'black'),
        plot.title = element_text(size = TEXT, face = "bold"))
plot

tiff(paste0(datadir, 'figures/tide-comps-T1-ML-2022.tiff'), unit = 'in', height = 7, width = 10, res = 150)
plot
dev.off()
