rm(list=ls())

library(tidyverse)
library(lubridate)
library(data.table)
library(readxl)
library("rio")
library(zoo)
Sys.setenv(TZ='GMT')

## define data directory
datadir <- '/Users/dhardy/Dropbox/r_data/sapelo/water-level/'

## define depth reference datum as NAVD88 or local substrate
# level.var <- c('water_depth_m')

# set dates for transect graphs
int.date1 <- as.Date('2021-10-19') 
int.date2 <- as.Date('2021-11-18') 

######################
## import & tidy data
#####################

## define column classes
## import cleaned water level data
df <- read_csv(paste(datadir, 'wls_data.csv'))[,-1] %>%
  mutate(date_time_gmt = as.POSIXct(date_time_gmt, format = "%Y-%m-%d %H:%M:%S", tz = 'GMT'),
         date = as.POSIXct(date))

## add moving averages
df <- df %>%
  mutate(ma1hr = rollmean(water_level_navd88, k=5, fill=NA, align = 'center'))

##############################################################################################
# TRANSECTS HYDROGRAPHS create graphing function for 12-minute intervals over specified interval
# https://www.reed.edu/data-at-reed/resources/R/loops_with_ggplot2.html
##############################################################################################
TEXT = 15 ## set font size for figures
tx.graph <- function(df, na.rm = TRUE, ...){
  
  # create list of logger sites in data to loop over 
  transect_list <- unique(df$transect)
  # transect_list <- 'T3'
  
  # create for loop to produce ggplot2 graphs 
  for (i in seq_along(transect_list)) {
    
    df2 <- filter(df, transect == transect_list[i] & date_time_gmt >= int.date1 & date_time_gmt <= int.date2)
    
    daily.mn <- df2 %>%
      mutate(date = floor_date(date_time_gmt, unit = 'day')) %>%
      group_by(transect, site, date) %>%
      summarize(mean = mean(water_level_navd88))
    
    # create plot for each site in df 
    plot <- 
      ggplot(df2)  + 
      geom_line(aes(date_time_gmt, ma1hr, color = site)) + 
      geom_line(aes(date, mean, color = site), 
                data = filter(daily.mn),
                linewidth = 1) +
      # geom_hline(aes(yintercept = mean(water_level_navd88)), linetype = 'dashed', df2) +
      # geom_point(aes(date_time_gmt, TP_mm/100), data = int.TP, size = 1, color = 'red') +
      # geom_line(aes(date_time_gmt, salinity/25), lwd = 0.5, color = 'blue') +
      # geom_point(aes(date_time_gmt, 1.5, fill = phase), data = int.lnr, shape = 21, size = 5) +
      # geom_text(aes(date_time_gmt, 1.5, label = dist_rad), data = int.lnr, vjust = -1) + 
      # scale_fill_manual(values = c('white', 'black')) + 
      scale_x_datetime(name = paste0('Month/Day/', year(int.date1)), date_breaks = '1 week', date_labels = '%m/%d') + 
      scale_y_continuous(name = 'Water Level (m NAVD88)', breaks = seq(0.2,2,0.2), limits = c(0.2,2), expand = c(0,0)) +
      # annotate("rect",
      #          xmin = as.POSIXct(paste(int.date1, '00:48:00')),
      #          xmax = as.POSIXct(paste(int.date1, '12:48:00')),
      #          ymin = 0,
      #          ymax = df2$well_ht,
      #          alpha = 0.1) +
      # annotate("text",
      #          x = as.POSIXct(paste(int.date1, '06:48:00')),
      #          y = df2$well_ht+0.1,
      #          label = 'Well Height',
      #          angle = 90) +
      labs(color = 'Site') + 
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
            panel.grid.major.y = element_line('grey', size = 0.5, linetype = "dotted"),
            panel.grid.major.x = element_line('grey', size = 0.5, linetype = "dotted"),
            plot.margin = margin(0.5,0.5,0.5,0.5, 'cm'),
            legend.position = 'bottom',
            legend.text = element_text(size = TEXT),
            legend.title = element_text(size = TEXT),
            legend.box.background = element_rect(color = 'black'),
            plot.title = element_text(size = TEXT, face = "bold"))
    # ggtitle(paste0(transect_list[i], " - 12-minute Interval From ", int.date1, ' to ', int.date2))
    
    # save plots as .png
    ggsave(plot, file=paste(datadir,
                            'figures/', 'Transect-NAVD88 ', '1-Hr-MA ', transect_list[i], ".png", sep=''), width = 6, height = 5, units = 'in', scale=2)
    
    # save plots as .pdf
    # ggsave(plot, file=paste(results, 
    #                        'projection_graphs/county_graphs/',
    #                        count_list[i], ".pdf", sep=''), scale=2) 
    
    # print plots to screen
    # print(plot)
  }
}

# run graphing function on long df
tx.graph(df)

#####################################################################
## explore transects 1 and 5 hydrological connections via site NW corner
#####################################################################
# df.t <- filter(df, site_new %in% c('T5-03', 'T3-03', 'T4-02') & date_time_gmt >= int.date1 & date_time_gmt <= int.date2)
# 
# daily.mn <- df.t %>%
#   mutate(date = floor_date(date_time_gmt, unit = 'day')) %>%
#   group_by(transect, site_new, date) %>%
#   summarize(mean = mean(water_level_navd88))
# 
# TEXT = 8 ## set font size for figures
# 
# plot <- ggplot(df.t)  + 
#   geom_line(aes(date_time_gmt, water_level_navd88 * 3.28084, color = site_new), linewidth = 0.2) + 
#   geom_line(aes(date, mean * 3.28084, color = site_new), 
#             data = filter(daily.mn), linewidth = 0.2) +
#   # geom_hline(aes(yintercept = mean(water_level_navd88)), linetype = 'dashed', df2) +
#   # geom_point(aes(date_time_gmt, TP_mm/100), data = int.TP, size = 1, color = 'red') +
#   # geom_line(aes(date_time_gmt, salinity/25), lwd = 0.5, color = 'blue') +
#   # geom_point(aes(date_time_gmt, 1.5, fill = phase), data = int.lnr, shape = 21, size = 5) +
#   # geom_text(aes(date_time_gmt, 1.5, label = dist_rad), data = int.lnr, vjust = -1) + 
#   # scale_fill_manual(values = c('white', 'black')) + 
#   scale_x_datetime(name = paste0('Month/Day/', year(int.date1)), date_breaks = '1 week', date_labels = '%m/%d', date_minor_breaks = '1 day') + 
#   scale_y_continuous(name = 'Water Level (ft NAVD88)', breaks = seq(-1,7,1), limits = c(-1,7), expand = c(0,0)) +
#   # annotate("rect",
#   #          xmin = as.POSIXct(paste(int.date1, '00:48:00')),
#   #          xmax = as.POSIXct(paste(int.date1, '12:48:00')),
#   #          ymin = 0,
#   #          ymax = df2$well_ht,
#   #          alpha = 0.1) +
#   # annotate("text",
#   #          x = as.POSIXct(paste(int.date1, '06:48:00')),
#   #          y = df2$well_ht+0.1,
#   #          label = 'Well Height',
#   #          angle = 90) +
#   labs(color = 'Transect-Site') + 
#   theme(axis.title = element_text(size = TEXT),
#         axis.text = element_text(color = "black", size = TEXT),
#         axis.ticks.length = unit(-0.2, 'cm'),
#         axis.ticks = element_line(color = 'black'),
#         axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), 
#         axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")),
#         axis.line = element_line(color = 'black'),
#         axis.text.y.right = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm"), color = 'blue'),
#         axis.title.y.right = element_text(color = 'blue'),
#         axis.line.y.right = element_line(color = "blue"), 
#         axis.ticks.y.right = element_line(color = "blue"),
#         panel.background = element_rect(fill = FALSE, color = 'black'),
#         # panel.grid = element_blank(),
#         panel.grid.major.x = element_line('grey', size = 0.5, linetype = "dashed"),
#         panel.grid.minor.x = element_line('grey', size = 0.5, linetype = "dotted"),
#         panel.grid.major.y = element_line('grey', size = 0.5, linetype = "dashed"),
#         panel.grid.minor.y = element_line('grey', size = 0.5, linetype = "dotted"),
#         # plot.margin = margin(0.5,0.5,0.5,0.5, 'cm'),
#         # legend.position = c(0.15, 0.85),
#         legend.position = 'bottom',
#         legend.text = element_text(size = TEXT),
#         legend.title = element_text(size = TEXT),
#         # legend.box.background = element_rect(color = 'black'),
#         plot.title = element_text(size = TEXT, face = "bold"))
# # ggtitle(paste0(transect_list[i], " - 12-minute Interval From ", int.date1, ' to ', int.date2))
# plot
# 
# tiff(paste0(datadir, 'figures/comparison-ditch-inland.tiff'), unit = 'in', height = 5, width = 6.5, res = 300)
# plot
# dev.off()
# 
# t501 <- filter(df, site_new == 'T5-01' 
#                & date == '2022-11-20'
# )
# ggplot(filter(t501), aes(date_time_gmt, water_level_navd88)) + 
#   geom_line()