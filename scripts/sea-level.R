rm(list=ls())

library(rnoaa) ## package info https://cran.r-project.org/web/packages/rnoaa/rnoaa.pdf
library(tidyverse)

## define data directory
datadir <- '/Users/dhardy/Dropbox/r_data/sapelo'

## import average seasonal cycle data
test <- read.csv("https://tidesandcurrents.noaa.gov/sltrends/data/USAverageSeasonalCycleData.csv",
                 stringsAsFactors = FALSE)

## define stations of interest
STATIONS <- c(8670870, 8720030) ## Fort Pulaski and Fernandina
DATUM <- 'MSL'
# TIME <- c(3653, 3653*2)

## grab most recent decade of data
T1 <- 
  lapply(STATIONS, function(z) {
    coops_search(begin_date = as.character(as.Date(Sys.Date()-3653, format = '%Y%m%d')) %>% 
                   gsub('-', '', .) %>%
                   as.numeric(),
                 end_date = as.character(as.Date(Sys.Date(), format = '%Y%m%d')) %>% 
                   gsub('-', '', .) %>%
                   as.numeric(),
                 station_name = z,
                 product = 'monthly_mean', 
                 datum = DATUM, 
                 units = 'metric', 
                 time_zone = 'GMT')
}$data)

## grab second most recent decade of data
T2 <- 
  lapply(STATIONS, function(z) {
    coops_search(begin_date = as.character(as.Date(Sys.Date()-3653*2, format = '%Y%m%d')) %>% 
                   gsub('-', '', .) %>%
                   as.numeric(),
                 end_date = as.character(as.Date(Sys.Date()-3653, format = '%Y%m%d')) %>% 
                   gsub('-', '', .) %>%
                   as.numeric(),
                 station_name = z,
                 product = 'monthly_mean', 
                 datum = DATUM, 
                 units = 'metric', 
                 time_zone = 'GMT')
  }$data)

T1_1 <- T1[[1]] %>%
  mutate(station = as.character(STATIONS[1]))
T1_2 <- T1[[2]] %>%
  mutate(station = as.character(STATIONS[2]))
T2_1 <- T2[[1]] %>%
  mutate(station = as.character(STATIONS[1]))
T2_2 <- T2[[2]] %>%
  mutate(station = as.character(STATIONS[2]))

df1 <- rbind(T1_1, T1_2)
df2 <- rbind(T2_1, T2_2)
df <- rbind(df1, df2)

dat <- df %>%
  mutate(yrmo = paste(year, month, sep = '-')) %>%
  mutate(date = as.Date(paste(yrmo, '-01', sep = '')))

fig <- ggplot(filter(dat, station == STATIONS[1]), aes(x = date, y = MSL, color = station)) +
  #geom_line() +
  geom_smooth(method = 'loess') + 
  geom_point(size = 0.5) + 
  scale_y_continuous(name = paste('Datum', DATUM, '(m)'),
                     breaks = round(seq(-0.2, 0.4, by = 0.1), 2),
                     minor_breaks = seq(-0.2, 0.4, by = 0.05)) + 
  scale_x_date(name = 'Date', 
               date_breaks = '6 months', 
               date_minor_breaks = '1 month',
               date_labels = '%b-%y') + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        legend.position = 'none') + 
  ggtitle(paste("Monthly Mean Sea Level, NOAA Tide Station ID:", STATIONS[1]))
fig

tiff(file.path(datadir, '/figures/sea-level.tif'), units = 'in', 
     height = 4, width = 6, compression = 'lzw',
     res = 300)
fig
dev.off()
