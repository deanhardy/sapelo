rm(list=ls())

library(tidyverse)
library(lubridate)

## define data directory
datadir <- '/Users/dhardy/Dropbox/r_data/sapelo'

## import data
pop <- read.csv(file.path(datadir, "population/population_sapelo.csv"), stringsAsFactors = F, skip = 2) %>%
  mutate(date = as.Date(date, "%m/%d/%Y"), unknown = as.numeric(total), black = as.numeric(black), 
         white = as.numeric(white)) %>%
  select(date, unknown, white, black) %>%
  gather(key = 'race', value = 'population', 2:4)
  # mutate(type = factor(race, levels = c('white', 'black', 'unknown')))

evt <- read.csv(file.path(datadir, 'population/events.csv'), stringsAsFactors = F, skip = 2) %>%
  mutate(date = as.Date(date, '%m/%d/%Y'))

## from https://stackoverflow.com/questions/14759676/specification-of-first-and-last-tick-marks-with-scale-x-date
break.vec <- c(as.Date('1860-01-01'), seq(from = as.Date("1860-01-01"), to = as.Date("2020-01-01"),
                   by = "10 years"), as.Date('2020-01-01'))
break.date <- as.Date('1855-01-01')

fig <- ggplot() +
  geom_col(aes(y = population, x = date, fill = race), 
           filter(pop, race == 'unknown', date >= break.date), width = 300) + 
  geom_col(aes(y = population, x = date, fill = factor(race, levels = c('white', 'black'))), 
           filter(pop, race != 'unknown', date >= break.date), width = 300) + 
  geom_segment(aes(y = 0, yend = seq(990, 150, -(990-150)/(nrow(filter(evt, date >= break.date))-1)), 
                   x = date, xend = date),
               filter(evt, date >= break.date), 
               linetype = 'dashed', size = 0.3) +
  geom_text(aes(x = date, y = seq(990, 150, -(990-150)/(nrow(filter(evt, date >= break.date))-1)), 
                label = paste(event, ' ', '(', source, ')', sep = '')),
            filter(evt, date >= break.date), 
            hjust = -0.01, size = 2) +
  scale_x_date(name = "Year", 
               breaks = break.vec,
               # date_breaks = "10 years",
               # limits = as.Date(c('1860-01-01', '2020-01-01')),
               expand = c(0.03,0),
               date_labels = "%Y") + 
  scale_y_continuous(name = "Population",
                     breaks = seq(0,1000, 100),
                     limits = c(0,1000), expand = c(0,0)) +
  scale_fill_manual(name = 'Race', 
                    labels = c('Black', 'Other', 'White'),
                    values = c(black = 'black', unknown = 'grey50', white = 'grey')) +
# expand_limits(x = as.Date('1850-01-01')) + 
  theme(axis.line = element_line(color = 'black'),
        panel.background = element_rect(fill = FALSE),
        panel.grid = element_blank(),
        #panel.grid.major.y = element_line('grey', size = 0.5, linetype = "dotted"),
        plot.margin = margin(1,1,0.5,0.5, 'cm'),
        legend.position = c(0.9,0.9),
        legend.text = element_text(size = 7))
fig 
 
tiff(file.path(datadir, 'figures/population_sapelo_cwbp-no1.tif'), compression = 'lzw', unit = 'in', height = 3.9,
     width = 7.5, res = 600)
fig
dev.off()

# jpeg(file.path(datadir, 'figures/population_sapelo_cwbp-no1.jpeg'), height = 3.9,
#      width = 7.5, units = 'in', res = 300, pointsize = 10, type = c("cairo", "Xlib", "quartz"))
# fig
# dev.off()

# pdf(file.path(datadir, 'figures/population_sapelo_cwbp-no1.pdf'), height = 3.9,
#      width = 7.5)
# fig
# dev.off()

slide <- ggplot() +
  geom_col(aes(y = population, x = date, fill = race), filter(pop, race == 'unknown'), width = 500) + 
  geom_col(aes(y = population, x = date, fill = factor(race, levels = c('white', 'black'))), 
           filter(pop, race != 'unknown'), width = 500) + 
  geom_segment(data = evt, aes(y = 0, yend = seq(950, 150, -40), x = date, xend = date), 
               linetype = 'dashed', size = 0.3) +
  geom_text(data = evt, aes(x = date, y = seq(950, 150, -40), label = event), 
            hjust = -0.01, size = 5) + 
  scale_x_date(name = "Year", date_breaks = "20 year", 
               limits = as.Date(c('1860-01-01', '2020-01-01')),
               date_labels = "%Y",
               expand = c(0,0)) + 
  scale_y_continuous(name = "Population",
                     breaks = seq(0,1000, 100),
                     limits = c(0,1000), expand = c(0,0)) +
  scale_fill_manual(name = 'Race', 
                    labels = c('Black', 'Unknown', 'White'),
                    values = c(black = 'black', unknown = 'grey50', white = 'grey')) +
  theme(axis.line = element_line(color = 'black'),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        panel.background = element_rect(fill = FALSE),
        panel.grid = element_blank(),
        #panel.grid.major.y = element_line('grey', size = 0.5, linetype = "dotted"),
        plot.margin = margin(1,1,0.5,0.5, 'cm'),
        legend.position = c(0.9,0.9),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 16))
# expand_limits(x = as.Date('1850-01-01'))
slide

png(file.path(datadir, 'figures/population_sapelo_slide.png'), unit = 'in', height = 7,
     width = 13.33, res = 150)
slide
dev.off()

