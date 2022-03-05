rm(list=ls())

library(tidyverse)
library(lubridate)
library(data.table)
Sys.setenv(TZ='GMT')

## define data directory
datadir <- '/Users/dhardy/Dropbox/r_data/sapelo/water-level/'
# datadir <- '/Users/Rebecca/Dropbox/r_data/sapelo/water-level/'

# set dates for graphs
date1 <- as.Date('2018-10-13') 
date2 <- as.Date('2019-01-18')

## import water level data files
filz <- list.files(path = file.path(datadir, 'new-logger-data'),
                   pattern= '*.csv',
                   full.names = TRUE,
                   recursive = TRUE) 
tidal <- NULL

## import mllw elevation including lidar and RTK adjusted elevations 
elev <- read.csv(file.path(datadir, 'site-elevations.csv'))

## import daily precipitation totals
TP <- read_csv(file.path(datadir, 'nerr-data/SAPMLMET_TP.csv')) %>%
  select(date_time_gmt, TP_mm) %>%
  filter(date_time_gmt >= date1 & date_time_gmt <= date2,
         TP_mm > 0)

## import & tidy hobo water level data
for(i in 1:length(filz)) {
  OUT <- fread(filz[i],
               select = c(2:5),
               col.names = c('date_time_gmt', 'abs_pres_psi', 'water_temp_c', 'water_depth_m'),
               stringsAsFactors = FALSE) %>%
    slice(., 5:(n()-7)) %>% ## removes first and last ## readings
    mutate(date_time_gmt = mdy_hms(date_time_gmt),
           date = as.Date(date_time_gmt, '%m/%d/%Y'),
           site = str_sub(filz[i], -25,-24)) %>%
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
    # filter(water_depth_m >=0 & water_depth_m <2)
  tidal <- rbind(OUT, tidal)
}

ht <- tidal %>%
  group_by(date) %>%
  arrange(desc(water_depth_m)) %>%
  slice(1) %>%
  ungroup()

SN <- elev$name

tidal2 <- NULL
## correcting water levels from well cap to substrate reference elevation
for (i in 1:length(SN)) {
  
  el2 <- elev %>%
  filter(name == SN[[i]]) 
  
  OUT2 <- tidal %>%
    filter(name == SN[[i]]) %>%
    mutate(water_depth_m = water_depth_m + el2$well_ht_m,
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
  
    # create plot for each site in df 
    plot <- 
      ggplot(filter(df, sitename == sites_list[i] & date_time_gmt >= date1 & date_time_gmt <= date2))  + 
        geom_line(aes(date_time_gmt, water_depth_m)) +  ## convert to feet then add MLLW base elevation
        geom_point(aes(date_time_gmt, TP_mm/100), data = TP, color = 'blue', size = 3) + 
        # geom_line(aes(date_time_gmt, water_temp_c/15), lty = 'dotted', color = 'black') + 
        # geom_line(aes(date_time_gmt, Depth * 3.28084), data = nerr) + 
        # geom_point(aes(date_time_gmt, Pred), data = ot2) +
        scale_x_datetime(name = 'Month', date_breaks = '1 month', date_labels = '%m') + 
        scale_y_continuous(name = 'Water Depth (m)', breaks = seq(0,1.8,0.1), limits = c(0,1.8), expand = c(0,0)
                           # sec.axis = sec_axis(~. * 15, 
                           #                    name = expression(paste('Water Temperature (',degree,'C)')))
                           ) +
      theme(axis.title = element_text(size = TEXT),
            axis.text = element_text(color = "black", size = TEXT),
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
            plot.title = element_text(size = TEXT, face = "bold"),
            legend.position = c(10/15,3),
            legend.text = element_text(size = TEXT),
            legend.title = element_text(size = TEXT),
            legend.key = element_blank(),
            legend.box.background = element_rect(color = 'black')) + 
  #    annotate(name) + 
      ggtitle(paste0(sites_list[i], " - Year ", year(date1)))
    
    # save plots as .png
    ggsave(plot, file=paste(datadir,
                           'figures/', sites_list[i], ".png", sep=''), width = 6, height = 5, units = 'in', scale=2)
    
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


