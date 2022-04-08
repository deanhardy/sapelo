rm(list=ls())

library(tidyverse)
library(lubridate)
library(data.table)
Sys.setenv(TZ='GMT')

## define data directory
datadir <- '/Users/dhardy/Dropbox/r_data/sapelo/water-level/'
# datadir <- '/Users/Rebecca/Dropbox/r_data/sapelo/water-level/'

# set dates for graphs
date1 <- as.Date('2019-01-01') 
date2 <- as.Date('2019-03-31')

## import water level data files
filz <- list.files(path = file.path(datadir, 'new-logger-data'),
                   pattern= '*.csv',
                   full.names = TRUE,
                   recursive = TRUE) 
tidal <- NULL

## import mllw elevation including lidar and RTK adjusted elevations 
elev <- read.csv(file.path(datadir, 'site-elevations.csv'))

## import daily precipitation totals
TP <- read.csv(file.path(datadir, 'nerr-data/SAPMLMET_TP.csv')) %>%
  select(date_time_gmt, TP_mm) %>%
  filter(date_time_gmt >= date1 & date_time_gmt <= date2,
         TP_mm > 0) %>%
  mutate(date_time_gmt = as.POSIXct(date_time_gmt, format = '%Y-%m-%d %H:%M:%S'))

## import lunar data
lnr <- read.csv(file.path(datadir, 'lunar.csv')) %>%
  filter(date_time_gmt >= date1 & date_time_gmt <= date2 & phase %in% c('New Moon', 'Full Moon')) %>%
  mutate(date_time_gmt = as.POSIXct(date_time_gmt, format = '%Y-%m-%d %H:%M:%S'),
         phase = ifelse(phase == 'New Moon', 'New', 'Full'))

## import & tidy hobo water level data
## note water level C is in meters and indicates water level in reference to wellcap 
for(i in 1:length(filz)) {
  OUT <- fread(filz[i],
               select = c(2:5),
               col.names = c('date_time_gmt', 'abs_pres_psi', 'water_temp_c', 'water_level_C'),
               stringsAsFactors = FALSE) %>%
    slice(., 5:(n()-7)) %>% ## removes first and last ## readings
    mutate(date_time_gmt = mdy_hms(date_time_gmt),
           date = as.Date(date_time_gmt, '%m/%d/%y', tz = 'GMT'),
           site = str_sub(filz[i], -25,-24),
           water_level_C = as.numeric(water_level_C)) %>%
    mutate(site = paste('Site', site, sep = '-')) %>%
    mutate(name = if_else(site == 'Site-02', 'Snagtree',
                          if_else(site == 'Site-03', 'St. Lukes',
                                  if_else(site == 'Site-05', 'Graball',
                                          if_else(site == 'Site-06', 'Dani Trap',
                                                  if_else(site == 'Site-07', 'Cactus Patch',
                                                          if_else(site == 'Site-09', 'Mr. Tracys',
                                                                  if_else(site == 'Site-11', 'Library',
                                                                          if_else(site == 'Site-12', 'Mr. Smith',
                                                                                  if_else(site == 'Site-13', 'Purple Ribbon',
                                                                                          if_else(site == 'Site-14', 'Tidal Gate', site))))))))))) %>%
    mutate(sitename = paste(site, name))
  tidal <- rbind(OUT, tidal)
}

ht <- tidal %>%
  group_by(date) %>%
  arrange(desc(water_level_C)) %>%
  slice(1) %>%
  ungroup()

SN <- elev$name

tidal2 <- NULL
## adding references for water level from well cap to substrate (depth) and NAVD88
for (i in 1:length(SN)) {
  
  el2 <- elev %>%
  filter(name == SN[[i]]) 
  
  OUT2 <- tidal %>%
    filter(name == SN[[i]]) %>%
    mutate(water_depth_m = water_level_C + el2$well_ht_m,
           water_level_navd88 = el2$wellcap_navd88_m + water_level_C,
           well_ht = el2$well_ht)
  
  tidal2 <- rbind(OUT2, tidal2)
}

## import SINERR water level data from Lower Duplin
# nerr <- read.csv(file.path(datadir, 'nerr-data/lowerduplin-realtime-jan18-nov18.csv'),
#                  header = TRUE, stringsAsFactors = FALSE, skip = 2) %>%
#   slice(., 3:n()) %>%
#   mutate(date_time_gmt = ymd_hms(Date),
#          Depth = as.numeric(Depth)) %>%
#   filter(date_time_gmt >= first(df$date_time_gmt) & date_time_gmt <= last(df$date_time_gmt))

## import NOAA tide predictions for Sapelo Old Tower
ot <- read.delim(file.path(datadir, 'nerr-data/OldTower-tide-predictions.txt'),
                 sep = '\t', header = TRUE, skip = 19,
                 stringsAsFactors = FALSE)
colnames(ot) <- c('Date', 'Day', 'Time', 'Pred', 'High.Low', 'SKIP1', 'SKIP2')  

## further tidy and filter data to just high tide
ot2 <- ot %>% 
  mutate(date_time_gmt = with(., as.POSIXct(paste(Date, Time), 
                              format = '%Y/%m/%d %I:%M %p'))) %>%
  select(date_time_gmt, Pred, High.Low) %>%
  filter(High.Low == 'H')

## plot tidal data
# ggplot(filter(tidal, site == 'site13' & date_time_gmt >= as.Date('2019-05-01') & date_time_gmt <= as.Date('2019-06-30')))  + 
#   geom_line(aes(date_time_gmt, water_depth_m)) +  ## convert to feet then add MLLW base elevation
#   geom_line(aes(date_time_gmt, water_temp_c/15), lty = 'dotted', color = 'black') + 
#   # geom_line(aes(date_time_gmt, Depth * 3.28084), data = nerr) + 
#   # geom_point(aes(date_time_gmt, Pred), data = ot2) +
#   scale_x_datetime(name = 'Date (Year 2019)', date_breaks = '7 days', date_labels = '%m/%d') + 
#   scale_y_continuous(name = 'Water Depth (m)',
#                      sec.axis = sec_axis(~. * 15, 
#                                          name = expression(paste('Water Temperature (',degree,'C)')))) +
#   theme(axis.title = element_text(size = 18),
#         axis.text = element_text(color = "black", size = 18),
#         axis.ticks.length = unit(-0.2, 'cm'),
#         axis.ticks = element_line(color = 'black'),
#         axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), 
#         axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")),
#         axis.text.y.right = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")),
#         axis.line = element_line(color = 'black'),
#         panel.background = element_rect(fill = FALSE, color = 'black'),
#         panel.grid = element_blank(),
#         panel.grid.major.x = element_line('grey', size = 0.5, linetype = "dotted"),
#         plot.margin = margin(0.5,0.5,0.5,0.5, 'cm'),
#         legend.position = c(10/15,3),
#         legend.text = element_text(size = 18),
#         legend.title = element_text(size = 18),
#         legend.key = element_blank(),
#         legend.box.background = element_rect(color = 'black'))  

########################################################
# create graphing function
# https://www.reed.edu/data-at-reed/resources/R/loops_with_ggplot2.html
########################################################
TEXT = 15 ## set font size for figures
sites.graph <- function(df, na.rm = TRUE, ...){
  
  # create list of logger sites in data to loop over 
  sites_list <- unique(df$sitename)
  
  # create for loop to produce ggplot2 graphs 
  for (i in seq_along(sites_list)) {
  
    df2 <- filter(df, sitename == sites_list[i] & date_time_gmt >= date1 & date_time_gmt <= date2)
    
    # create plot for each site in df 
    plot <- 
      ggplot(df2)  + 
      geom_line(aes(date_time_gmt, water_level_navd88)) +  ## convert to feet then add MLLW base elevation
      geom_hline(aes(yintercept = mean(water_level_navd88)), linetype = 'dashed', df2) +
      geom_vline(aes(xintercept = as.POSIXct('2019-01-18 00:12:00'))) + 
                 # filter(df, sitename == sites_list[i] & date_time_gmt >= date1 & date_time_gmt <= date2)) + 
      geom_point(aes(date_time_gmt, TP_mm/100), data = TP, color = 'blue', size = 3) +
      geom_point(aes(date_time_gmt, 1.5, fill = phase), data = lnr, shape = 21, size = 5) +
      geom_text(aes(date_time_gmt, 1.5, label = dist_rad), data = lnr, vjust = -1) + 
      scale_fill_manual(values = c('white', 'black')) + 
      # geom_point(aes(date_time_gmt, 1.5), data = filter(lnr, phase == 'Full Moon'), shape = 1, size = 5) +
      # geom_point(aes(date_time_gmt, 1.5), data = filter(lnr, phase == 'New Moon'), shape = 16, size = 5) +
      # geom_line(aes(date_time_gmt, water_temp_c/15), lty = 'dotted', color = 'black') + 
      # geom_line(aes(date_time_gmt, Depth * 3.28084), data = nerr) + 
      # geom_point(aes(date_time_gmt, Pred), data = ot2) +
      scale_x_datetime(name = 'Month', date_breaks = '1 month', date_labels = '%m') + 
      scale_y_continuous(name = 'Water Level (NAVD88)', breaks = seq(0,1.8,0.1), limits = c(0,1.8), expand = c(0,0),
                         sec.axis = sec_axis(~. * 100, breaks = seq(0,180, 10),
                                           name = expression(paste('Total Daily Precipitation (mm)')))
                         ) +
      annotate("rect",
               xmin = as.POSIXct(paste(date1, '00:48:00')),
               xmax = as.POSIXct(paste(date1, '12:48:00')),
               ymin = 0,
               ymax = df2$well_ht,
               alpha = 0.1) +
      annotate("text",
               x = as.POSIXct(paste(date1, '06:48:00')),
               y = df2$well_ht+0.1,
               label = 'Well Height',
               angle = 90) +
      # geom_text(aes(as.POSIXct('2018-10-13 00:48:00'), df2$well_ht, label = 'well height')) +
      labs(fill = 'Moon Phase', caption = "Dashed line indicates mean water level.") + 
      theme(axis.title = element_text(size = TEXT),
            axis.text = element_text(color = "black", size = TEXT),
            axis.ticks.length = unit(-0.2, 'cm'),
            axis.ticks = element_line(color = 'black'),
            axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), 
            axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")),
            axis.line = element_line(color = 'black'),
            axis.text.y.right = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm"), color = 'blue'),
            axis.title.y.right = element_text(color = 'blue'),
            axis.line.y.right = element_line(color = "blue"), 
            axis.ticks.y.right = element_line(color = "blue"),
            panel.background = element_rect(fill = FALSE, color = 'black'),
            panel.grid = element_blank(),
            panel.grid.major.x = element_line('grey', size = 0.5, linetype = "dotted"),
            plot.margin = margin(0.5,0.5,0.5,0.5, 'cm'),
            legend.position = c(0.1, 0.92),
            legend.text = element_text(size = TEXT),
            legend.title = element_text(size = TEXT),
            # legend.key = element_blank(),
            legend.box.background = element_rect(color = 'black'),
            plot.title = element_text(size = TEXT, face = "bold")) + 
      ggtitle(paste0(sites_list[i], " - Year ", year(date1)))
    
    # save plots as .png
    ggsave(plot, file=paste(datadir,
                           'figures/', sites_list[i], ' ', date1, ' to ', date2, ' NAVD88', ".png", sep=''), width = 6, height = 5, units = 'in', scale=2)
    
    # save plots as .pdf
    # ggsave(plot, file=paste(results, 
    #                        'projection_graphs/county_graphs/',
    #                        count_list[i], ".pdf", sep=''), scale=2)
    
    # print plots to screen
    # print(plot)
  }
}

# run graphing function on long df
sites.graph(tidal2)
