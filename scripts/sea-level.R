rm(list=ls())

library(rnoaa)
library(tidyverse)

## define data directory
datadir <- '/Users/dhardy/Dropbox/r_data/sapelo'

## import average seasonal cycle data
test <- read.csv("https://tidesandcurrents.noaa.gov/sltrends/data/USAverageSeasonalCycleData.csv",
                 stringsAsFactors = FALSE)

## define stations of interest
STATIONS <- c(8670870, 8720030)
DATUM <- 'MSL'

l <- 
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

df <- l[[1]] %>%
  mutate(station = as.character(STATIONS[1]))
df2 <- l[[2]] %>%
  mutate(station = as.character(STATIONS[2]))

df3 <- rbind(df, df2)

df4 <- df3 %>%
  mutate(yrmo = paste(year, month, sep = '-')) %>%
  mutate(date = as.Date(paste(yrmo, '-01', sep = '')))

fig <- ggplot(filter(df4, station == STATIONS[1]), aes(x = date, y = MSL, color = station)) +
  geom_line() +
  geom_point() + 
  #geom_smooth() + 
  scale_y_continuous(name = paste('Datum', DATUM, '(m)'),
                     breaks = round(seq(-0.2, 0.4, by = 0.1), 2),
                     minor_breaks = seq(-0.2, 0.4, by = 0.05)) + 
  scale_x_date(name = 'Date', 
               date_breaks = '6 months', 
               date_minor_breaks = '1 month',
               date_labels = '%b-%y') + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = 'none')
fig

tiff(file.path(datadir, '/figures/sea-level.tif'), units = 'in', 
     height = 4, width = 6, compression = 'lzw',
     res = 300)
fig
dev.off()
