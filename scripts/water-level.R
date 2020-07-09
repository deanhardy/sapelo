rm(list=ls())

library(tidyverse)
library(lubridate)
library(data.table)
Sys.setenv(TZ='GMT')

## define data directory
datadir <- '/Users/dhardy/Dropbox/r_data/sapelo/water-level/'

# set dates for graphs
date1 <- as.Date('2020-01-18') 
date2 <- as.Date('2020-06-13')

## define variables
# sites <- c('s02', 's03', 's05', 's06', 's07', 's09', 's11', 's12', 's13', 's14')
filz <- list.files(path = file.path(datadir, 'hobo-data'),
                   pattern= '*.csv',
                   full.names = TRUE,
                   recursive = TRUE) 
tidal <- NULL
datums <- read.csv(file.path(datadir, 'datums.csv'))
  
## import & tidy hobo water level data
for(i in 1:length(filz)) {
  OUT <- fread(filz[i],
               select = c(2:5),
               col.names = c('date_time_gmt', 'abs_pres_psi', 'water_temp_c', 'water_depth_m'),
               stringsAsFactors = FALSE) %>%
    slice(., 2:(n()-1)) %>% ## removes first and last readings
    mutate(date_time_gmt = mdy_hms(date_time_gmt),
           site = str_sub(filz[i], 68,-19)) %>%
    mutate(site = paste('Site', site, sep = '-')) %>%
    mutate(name = if_else(site == 'Site-02', 'Snagtree',
                          if_else(site == 'Site-03', 'St. Lukes',
                                  if_else(site == 'Site-05', 'Graball',
                                          if_else(site == 'Site-06', 'Dani Trap',
                                                  if_else(site == 'Site-07', 'Cope Spot',
                                                          if_else(site == 'Site-09', 'Mr. Tracys',
                                                                  if_else(site == 'Site-11', 'Library',
                                                                          if_else(site == 'Site-12', 'Mr. Smith',
                                                                                  if_else(site == 'Site-13', 'Purple Ribbon',
                                                                                          if_else(site == 'Site-14', 'Tidal Gate', site))))))))))) %>%
    mutate(sitename = paste(site, name))
  tidal <- rbind(OUT, tidal)
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

sites.graph <- function(df, na.rm = TRUE, ...){
  
  # create list of logger sites in data to loop over 
  sites_list <- unique(df$sitename)
  
  # create for loop to produce ggplot2 graphs 
  for (i in seq_along(sites_list)) {
  
    # create plot for each county in df 
    plot <- 
      ggplot(filter(df, sitename == sites_list[i] & date_time_gmt >= date1 & date_time_gmt <= date2))  + 
        geom_line(aes(date_time_gmt, water_depth_m)) +  ## convert to feet then add MLLW base elevation
        # geom_line(aes(date_time_gmt, water_temp_c/15), lty = 'dotted', color = 'black') + 
        # geom_line(aes(date_time_gmt, Depth * 3.28084), data = nerr) + 
        # geom_point(aes(date_time_gmt, Pred), data = ot2) +
        scale_x_datetime(name = 'Date (Year 2019)', date_breaks = '1 month', date_labels = '%m') + 
        scale_y_continuous(name = 'Water Depth (m)'
                           # sec.axis = sec_axis(~. * 15, 
                           #                    name = expression(paste('Water Temperature (',degree,'C)')))
                           ) +
      theme(axis.title = element_text(size = 18),
            axis.text = element_text(color = "black", size = 18),
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
            legend.text = element_text(size = 18),
            legend.title = element_text(size = 18),
            legend.key = element_blank(),
            legend.box.background = element_rect(color = 'black')) + 
  #    annotate(name) + 
      ggtitle(sites_list[i])
    
    # save plots as .png
    ggsave(plot, file=paste(datadir,
                           'figures/',
                           sites_list[i], ".png", sep=''), width = 6, height = 5, units = 'in', scale=2)
    
    # save plots as .pdf
    # ggsave(plot, file=paste(results, 
    #                        'projection_graphs/county_graphs/',
    #                        count_list[i], ".pdf", sep=''), scale=2)
    
    # print plots to screen
    # print(plot)
  }
}

# run graphing function on long df
sites.graph(tidal)


# tiff(file.path(datadir, 'figures/wl_site02.tif'), res = 300, compression = 'lzw', units = 'in', 
#      height = 8.5, width = 11)
# fig
# dev.off()
