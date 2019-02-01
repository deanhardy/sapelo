rm(list=ls())

library(tidyverse)
library(lubridate)
Sys.setenv(TZ='GMT')

## define data directory
datadir <- 'C:/Users/dhardy/Dropbox/r_data/sapelo'

## import data--- site 02 (aka "snagtree") MLLW elevation in meters is 1.9678506744 & in feet is 5.456203
df <- read.csv(file.path(datadir, 'water-level/hobo-data/site02-181013-181109.csv'), 
               header = TRUE, skip = 1,
               stringsAsFactors = FALSE)[-1]
colnames(df) <- c('date_time_gmt', 'abs_pres_psi', 'water_temp_f', 'water_height_m')

df <- df %>%
  mutate(date_time_gmt = mdy_hms(date_time_gmt)) %>%
  slice(., 2:(n()-1)) ## removes first and last readings

df2 <- read.csv(file.path(datadir, 'water-level/hobo-data/site02-181109-190118.csv'), 
                header = TRUE, skip = 1,
                stringsAsFactors = FALSE)[-1]
colnames(df2) <- c('date_time_gmt', 'abs_pres_psi', 'water_temp_f', 'water_height_m')

df2 <- df2 %>%
  mutate(date_time_gmt = mdy_hms(date_time_gmt)) %>%
  slice(., 2:(n()-1)) ## removes first and last readings

df3 <- rbind(df, df2)

nerr <- read.csv(file.path(datadir, 'water-level/nerr-data/lowerduplin-realtime-jan18-nov18.csv'),
                 header = TRUE, stringsAsFactors = FALSE, skip = 2) %>%
  slice(., 3:n()) %>%
  mutate(date_time_gmt = ymd_hms(Date),
         Depth = as.numeric(Depth)) %>%
  filter(date_time_gmt >= first(df$date_time_gmt) & date_time_gmt <= last(df$date_time_gmt))

ot <- read.delim(file.path(datadir, 'water-level/nerr-data/OldTower-tide-predictions.txt'),
                 sep = '\t', header = TRUE, skip = 19,
                 stringsAsFactors = FALSE)
colnames(ot) <- c('Date', 'Day', 'Time', 'Pred', 'High.Low', 'SKIP1', 'SKIP2')  

ot2 <- ot %>% 
  mutate(date_time_gmt = with(., as.POSIXct(paste(Date, Time), 
                              format = '%Y/%m/%d %I:%M %p'))) %>%
  select(date_time_gmt, Pred, High.Low) %>%
  filter(High.Low == 'H')

fig <- ggplot(filter(df3, date_time_gmt >= first(ot2$date_time_gmt) & date_time_gmt <= last(ot2$date_time_gmt))) + 
  geom_line(aes(date_time_gmt, water_height_m * 3.28084 + 5.456203)) + ## convert to feet then add MLLW base elevation
  geom_line(aes(date_time_gmt, water_temp_f/10), lty = 'dotted', color = 'black') + 
  # geom_line(aes(date_time_gmt, Depth * 3.28084), data = nerr) + 
  geom_point(aes(date_time_gmt, Pred), data = ot2) +
  scale_x_datetime(name = 'Date (Year 2018)', date_breaks = '4 day', date_labels = '%m/%d') + 
  scale_y_continuous(name = 'Height in feet (MLLW)',
                     sec.axis = sec_axis(~. * 10, 
                                         name = expression(paste('Water Temperature (',degree,'F)')))) +
  theme(axis.title = element_text(size = 8),
        axis.text = element_text(color = "black", size = 8),
        axis.ticks.length = unit(-0.2, 'cm'),
        axis.ticks = element_line(color = 'black'),
        axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), 
        axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")),
        axis.text.y.right = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")),
        axis.line = element_line(color = 'black'),
        panel.background = element_rect(fill = FALSE, color = 'black'),
        panel.grid = element_blank(),
        panel.grid.major.x = element_line('grey', size = 0.5, linetype = "dotted"),
        plot.margin = margin(0.5,0.5,0.5,0.5, 'cm'),
        legend.position = c(10/15,3),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.key = element_blank(),
        legend.box.background = element_rect(color = 'black'))  
#  labs(caption = "Temperature is dotted line.")
fig

tiff(file.path(datadir, 'figures/wl_site02.tif'), res = 300, compression = 'lzw', units = 'in', 
     height = 8.5, width = 11)
fig
dev.off()
