rm(list=ls())

library(tidyverse)
library(lubridate)
library(data.table)
library("rio")
Sys.setenv(TZ='GMT')

## define data directory
datadir <- '/Users/dhardy/Dropbox/r_data/sapelo/water-level/'

## define depth reference datum as NAVD88 or local substrate
# level.var <- c('water_depth_m')

# set dates for interval graphs
int.date1 <- as.Date('2021-10-01') 
int.date2 <- as.Date('2021-11-30') 

# set dates for daily high tide graphs
ht.date1 <- as.Date('2018-10-01') 
ht.date2 <- as.Date('2023-04-25')

# # set dates for esda graphs
# date1 <- as.Date('2018-10-01') 
# date2 <- as.Date('2023-04-25') 

######################
## import & tidy data
#####################

## import cleaned water level data
df <- read.csv(paste(datadir, 'wls_data.csv'))[,-1] %>%
  mutate(date_time_gmt = as.POSIXct(date_time_gmt),
         date = as.POSIXct(date))

## filter to interval dates
df.int <- df %>%
  filter(date_time_gmt >= int.date1 & date_time_gmt <= int.date2)
# select(site_new, site, type, transect, date_time_gmt, water_depth_m, water_level_navd88, water_temp_c)

## import daily precipitation totals
TP <- read.csv(file.path(datadir, 'nerr-data/SAPMLMET_TP.csv')) %>%
  select(date_time_gmt, TP_mm) %>%
  filter(date_time_gmt >= ht.date1 & date_time_gmt <= ht.date2,
         TP_mm > 0) %>%
  mutate(date_time_gmt = as.POSIXct(date_time_gmt, format = '%Y-%m-%d %H:%M:%S'))

## filter TP data to match water data
int.TP <- filter(TP, date_time_gmt >= int.date1 & date_time_gmt <= int.date2)

## view TP data
ggplot(TP, aes(TP_mm)) +
  geom_histogram()

## import lunar data
lnr <- read.csv(file.path(datadir, 'lunar.csv')) %>%
  filter(date_time_gmt >= ht.date1 & date_time_gmt <= ht.date2 & phase %in% c('New Moon', 'Full Moon')) %>%
  mutate(date_time_gmt = as.POSIXct(date_time_gmt, format = '%Y-%m-%d %H:%M:%S'),
         phase = ifelse(phase == 'New Moon', 'New', 'Full'))

## filter lunar data to match water data
int.lnr <- filter(lnr, date_time_gmt >= int.date1 & date_time_gmt <= int.date2)

#########
## esda 
#########

## facet wrap of water levels across transects and sites
sm.plot <- ggplot(df, aes(date_time_gmt, water_level_navd88)) + 
  geom_smooth(na.rm = T, aes(color = site_new, linetype = type)) + 
  scale_y_continuous(name = 'Water Level (m NAVD88)', limits = c(-0.2, 1.2)) + 
  labs(x = 'Date') + 
  theme_bw(base_size = 20) + 
  facet_wrap(~ transect)
# sm.plot

## export facet wrap
# png(paste0(datadir, '/figures/FacetWrap_', int.date1, "-to-", 
#            int.date2, '.png'), units = 'in', width = 10, height = 6, res = 150)
# sm.plot
# dev.off()

## averages by unit time
df.avg <- df %>%
  mutate(prd = floor_date(date_time_gmt, "month")) %>%
  group_by(transect, site_new, prd) %>%
  summarize(avg = mean(water_level_navd88))

ggplot(df.avg, aes(prd, avg, group = site_new)) + 
  geom_line(aes(color = site_new)) + 
  # geom_smooth(method = lm, se = F) + 
  facet_wrap(~ transect)

## plot temperatures all time
ggplot(df, aes(water_temp_c)) + 
  geom_histogram(bins = 25) + 
  scale_x_continuous(breaks = seq(0,120, 5))

## daily high tide
df.ht <- df %>%
  group_by(site_new, date) %>%
  slice_max(water_depth_m, with_ties = FALSE) %>%
  # select(date_time_gmt, water_depth_m, salinity) %>%
  ungroup()

## summary of number of days active by site
active.time <- df %>%
  group_by(sitename_new) %>%
  summarise(days = n_distinct(date)) %>%
  mutate(weeks = days/7, years = weeks/52)
# arrange(factor(site, years))

df.active <- active.time[order(active.time$sitename_new, decreasing = TRUE),]

## export active time for all sites
jpeg(paste0(datadir, "figures/sites-active-time.jpg"), width = 7, height = 5, units = 'in', res = 150)
par(mar=c(4,10,4,4))
barplot(df.active$years, names.arg = df.active$sitename_new,
        horiz = T, 
        las = 1,
        ylab = '',
        xlim = c(0,5),
        xlab = 'Years Active',
        main = 'Water Level Survey Sites')
dev.off()


########################################################################
# create graphing function for stacking transects 
# 
########################################################################
tdate1 <- as.Date('2021-10-20') 
tdate2 <- as.Date('2021-11-20')

library(lubridate)
daily.mn <- sites %>%
  mutate(date = floor_date(date_time_gmt, unit = 'day')) %>%
  group_by(transect, site_new, date) %>%
  summarize(mean = mean(water_level_navd88))

ggplot(filter(sites, transect == 'T1' & date_time_gmt >= tdate1 & date_time_gmt <= tdate2 & site_new != 'T1-05'))  + 
  geom_line(aes(date_time_gmt, water_level_navd88, color = site_new)) +  ## convert to feet then add MLLW base elevation
  geom_line(aes(date, mean, color = site_new), 
            data = filter(daily.mn, transect == 'T1' & date >= tdate1 & date <= tdate2)) +
  # geom_hline(aes(yintercept = mean(water_level_navd88)), linetype = 'dashed', df2) +
  # geom_point(aes(date_time_gmt, TP_mm/100), data = int.TP, size = 1, color = 'red') +
  # geom_point(aes(date_time_gmt, 1.5, fill = phase), data = int.lnr, shape = 21, size = 5) +
  # geom_text(aes(date_time_gmt, 1.5, label = dist_rad), data = int.lnr, vjust = -1) + 
  scale_fill_manual(values = c('white', 'black')) + 
  scale_x_datetime(name = 'Date', date_breaks = '1 week', date_labels = '%m/%d') + 
  scale_y_continuous(name = 'Water Level (m NAVD88)', breaks = seq(0,1.9,0.1), limits = c(0,2), expand = c(0,0),
                     # sec.axis = sec_axis(~. * 100, breaks = seq(0,180, 10),
                     #                   name = expression(paste('Total Daily Precipitation (mm)')))
  ) +
  # labs(fill = 'Moon Phase', caption = "Dashed line indicates mean water level.") + 
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
        plot.title = element_text(size = TEXT, face = "bold"))
# ggtitle(paste0(sites_list[i], " - 12-minute Interval From ", int.date1, ' to ', int.date2))


########################################################################
# create graphing function for daily highest tides' water depth
# https://www.reed.edu/data-at-reed/resources/R/loops_with_ggplot2.html
########################################################################
TEXT = 10 ## set font size for figures
ht.graph <- function(df, na.rm = TRUE, ...){
  
  # create list of logger sites in data to loop over 
  sites_list <- unique(df$sitename)
  
  # create for loop to produce ggplot2 graphs 
  for (i in seq_along(sites_list)) {
    
    df2 <- filter(df, sitename == sites_list[i] & date_time_gmt >= ht.date1 & date_time_gmt <= ht.date2)
    
    # create plot for each site in df 
    plot <- 
      ggplot(df2)  + 
      geom_line(aes(date_time_gmt, water_depth_m), lwd = 0.5) + 
      geom_hline(aes(yintercept = mean(water_depth_m)), linetype = 'dashed', df2) +
      geom_point(aes(date_time_gmt, TP_mm/100), data = TP, color = 'red', size = 0.5) +
      geom_line(aes(date_time_gmt, salinity/25), lwd = 0.5, color = 'blue') +
      scale_fill_manual(values = c('white', 'black')) + 
      scale_x_datetime(name = 'Month/Year', date_breaks = '2 month', date_minor_breaks = '1 month', date_labels = '%m/%y') + 
      scale_y_continuous(name = 'Water Level (meters)', breaks = seq(0,1.8,0.1), limits = c(0,1.8), expand = c(0,0),
                         # sec.axis = sec_axis(~. * 100, breaks = seq(0,180, 10),
                         #                   name = expression(paste('Total Daily Precipitation (mm)'))),
                         sec.axis = sec_axis(~. * 25, breaks = seq(0,45, 5),
                                             name = expression(paste('Salinity (psu)'))),
      ) +
      annotate("rect",
               xmin = as.POSIXct(paste(ht.date1, '00:48:00')),
               xmax = as.POSIXct(paste(ht.date1, '23:48:00')),
               ymin = 0,
               ymax = df2$well_ht,
               alpha = 0.1) +
      annotate("text",
               x = as.POSIXct(paste(ht.date1, '06:48:00')),
               y = df2$well_ht+0.1,
               label = 'Well Height',
               angle = 90) +
      # geom_text(aes(as.POSIXct('2018-10-13 00: 48:00'), df2$well_ht, label = 'well height')) +
      # labs(fill = 'Moon Phase', caption = "Dashed line indicates mean water level.") + 
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
            panel.grid.minor.x = element_line('grey', size = 0.5, linetype = "dotted"),
            plot.margin = margin(0.5,0.5,0.5,0.5, 'cm'),
            legend.position = c(0.1, 0.92),
            legend.text = element_text(size = TEXT),
            legend.title = element_text(size = TEXT),
            # legend.key = element_blank(),
            legend.box.background = element_rect(color = 'black'),
            plot.title = element_text(size = TEXT, face = "bold")) + 
      ggtitle(paste0(sites_list[i], " - Daily High Tide Trend From ", ht.date1, ' to ', ht.date2))
    
    # save plots as .png
    ggsave(plot, file=paste(datadir,
                            'figures/', 'Daily-HT ', sites_list[i], ".png", sep=''), width = 6, height = 5, units = 'in', scale=2)
    
    # save plots as .pdf
    # ggsave(plot, file=paste(results, 
    #                        'projection_graphs/county_graphs/',
    #                        count_list[i], ".pdf", sep=''), scale=2)
    
    # print plots to screen
    # print(plot)
  }
}

# run graphing function on long df
ht.graph(df)


##############################################################################################
# create graphing function for 12-minute intervals over specified interval using water depth
# https://www.reed.edu/data-at-reed/resources/R/loops_with_ggplot2.html
##############################################################################################
TEXT = 15 ## set font size for figures
int.graph <- function(df, na.rm = TRUE, ...){
  
  # create list of logger sites in data to loop over 
  sites_list <- unique(df$sitename)
  
  # create for loop to produce ggplot2 graphs 
  for (i in seq_along(sites_list)) {
    
    df2 <- filter(df, sitename == sites_list[i] & date_time_gmt >= int.date1 & date_time_gmt <= int.date2)
    
    # create plot for each site in df 
    plot <- 
      ggplot(df2)  + 
      geom_line(aes(date_time_gmt, water_level_navd88)) +  ## convert to feet then add MLLW base elevation
      geom_hline(aes(yintercept = mean(water_level_navd88)), linetype = 'dashed', df2) +
      geom_point(aes(date_time_gmt, TP_mm/100), data = int.TP, size = 1, color = 'red') +
      geom_line(aes(date_time_gmt, salinity/25), lwd = 0.5, color = 'blue') +
      geom_point(aes(date_time_gmt, 1.5, fill = phase), data = int.lnr, shape = 21, size = 5) +
      geom_text(aes(date_time_gmt, 1.5, label = dist_rad), data = int.lnr, vjust = -1) + 
      scale_fill_manual(values = c('white', 'black')) + 
      scale_x_datetime(name = 'Month', date_breaks = '1 month', date_labels = '%m') + 
      scale_y_continuous(name = 'Water Level (NAVD88) & Total Daily Precipitation x10 (meters)', breaks = seq(-0.5,1.8,0.1), limits = c(-0.5,1.8), expand = c(0,0),
                         # sec.axis = sec_axis(~. * 100, breaks = seq(0,180, 10),
                         #                   name = expression(paste('Total Daily Precipitation (mm)'))),
                         sec.axis = sec_axis(~. * 25, breaks = seq(0,45, 5),
                                             name = expression(paste('Salinity (psu)'))),
      ) +
      annotate("rect",
               xmin = as.POSIXct(paste(int.date1, '00:48:00')),
               xmax = as.POSIXct(paste(int.date1, '12:48:00')),
               ymin = 0,
               ymax = df2$well_ht,
               alpha = 0.1) +
      annotate("text",
               x = as.POSIXct(paste(int.date1, '06:48:00')),
               y = df2$well_ht+0.1,
               label = 'Well Height',
               angle = 90) +
      # geom_text(aes(as.POSIXct('2018-10-13 00:48:00'), df2$well_ht, label = 'well height')) +
      # labs(fill = 'Moon Phase', caption = "Dashed line indicates mean water level.") + 
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
      ggtitle(paste0(sites_list[i], " - 12-minute Interval From ", int.date1, ' to ', int.date2))
    
    # save plots as .png
    ggsave(plot, file=paste(datadir,
                            'figures/', 'NAVD88 ', 'Interval-12-minute ', sites_list[i], ".png", sep=''), width = 6, height = 5, units = 'in', scale=2)
    
  }
}

# run graphing function on long df
int.graph(df)

##############################################################################################
# TRANSECTS create graphing function for 12-minute intervals over specified interval using water depth
# https://www.reed.edu/data-at-reed/resources/R/loops_with_ggplot2.html
##############################################################################################
TEXT = 15 ## set font size for figures
tx.graph <- function(df, na.rm = TRUE, ...){
  
  # create list of logger sites in data to loop over 
  transect_list <- unique(df$transect)
  
  # create for loop to produce ggplot2 graphs 
  for (i in seq_along(transect_list)) {
    
    df2 <- filter(df, transect == transect_list[i] & date_time_gmt >= int.date1 & date_time_gmt <= int.date2)
    
    # create plot for each site in df 
    plot <- 
      ggplot(df2)  + 
      geom_line(aes(date_time_gmt, water_level_navd88, color = site)) + 
      # geom_hline(aes(yintercept = mean(water_level_navd88)), linetype = 'dashed', df2) +
      # geom_point(aes(date_time_gmt, TP_mm/100), data = int.TP, size = 1, color = 'red') +
      # geom_line(aes(date_time_gmt, salinity/25), lwd = 0.5, color = 'blue') +
      # geom_point(aes(date_time_gmt, 1.5, fill = phase), data = int.lnr, shape = 21, size = 5) +
      # geom_text(aes(date_time_gmt, 1.5, label = dist_rad), data = int.lnr, vjust = -1) + 
      # scale_fill_manual(values = c('white', 'black')) + 
      scale_x_datetime(name = 'Month', date_breaks = '1 month', date_labels = '%m') + 
      scale_y_continuous(name = 'Water Level (m NAVD88)', breaks = seq(0,1.8,0.1), limits = c(0,1.8), expand = c(0,0)) +
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
          legend.box.background = element_rect(color = 'black'),
          plot.title = element_text(size = TEXT, face = "bold")) + 
      ggtitle(paste0(transect_list[i], " - 12-minute Interval From ", int.date1, ' to ', int.date2))
    
    # save plots as .png
    ggsave(plot, file=paste(datadir,
                            'figures/', 'Transect-NAVD88 ', 'Interval-12-minute ', transect_list[i], ".png", sep=''), width = 6, height = 5, units = 'in', scale=2)
    
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