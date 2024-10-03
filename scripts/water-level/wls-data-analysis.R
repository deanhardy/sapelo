rm(list=ls())

library(tidyverse)
library(lubridate)
library(data.table)
library(readxl)
library("rio")
Sys.setenv(TZ='GMT')

## define data directory
datadir <- '/Users/dhardy/Dropbox/r_data/sapelo/water-level/'

## define depth reference datum as NAVD88 or local substrate
# level.var <- c('water_depth_m')

# set dates for transect graphs
int.date1 <- as.Date('2022-10-31') 
int.date2 <- as.Date('2022-11-21') 

# set dates for daily high tide graphs
ht.date1 <- as.Date('2018-11-01') 
ht.date2 <- as.Date('2024-02-25')

# # set dates for esda graphs
# date1 <- as.Date('2018-10-01') 
# date2 <- as.Date('2023-04-25') 

######################
## import & tidy data
#####################

## define column classes
## import cleaned water level data
df <- read_csv(paste(datadir, 'wls_data.csv'))[,-1] %>%
  mutate(date_time_gmt = as.POSIXct(date_time_gmt, format = "%Y-%m-%d %H:%M:%S", tz = 'GMT'),
         date = as.POSIXct(date))

## check for NAs in datetime column and remove them, if necessary
# nas <- df %>% filter(is.na(date_time_gmt))
# nas.df2 <- df2 %>% filter(is.na(date_time_gmt))
# df3 <- df2 %>% filter(!is.na(date_time_gmt))

## filter to interval dates
# df.int <- df %>%
#   filter(date >= int.date1 & date <= int.date2)
# select(site_new, site, type, transect, date_time_gmt, water_depth_m, water_level_navd88, water_temp_c)

## import daily precipitation totals
TP <- read.csv(file.path(datadir, 'nerr-data/SAPMLMET_TP.csv')) %>%
  select(date_time_gmt, TP_mm) %>%
  mutate(date_time_gmt = as.POSIXct(date_time_gmt, format = '%Y-%m-%d %H:%M:%S', tz = 'GMT'),
         date = as.Date(date_time_gmt, format = '%m/%d/%y', tz = 'GMT'))

ht.TP <- filter(TP, date_time_gmt >= ht.date1 & date_time_gmt <= ht.date2,
                  TP_mm > 0) %>%
  mutate(date = as.Date(date_time_gmt, '%Y-%m-%d', tz = 'GMT'))

## view TP data
ggplot(TP, aes(TP_mm)) +
  geom_histogram()

## import lunar data
lnr <- read.csv(file.path(datadir, 'lunar.csv')) %>%
  filter(
    # date_time_gmt >= ht.date1 & date_time_gmt <= ht.date2 & 
      phase %in% c('New Moon', 'Full Moon')) %>%
  mutate(date_time_gmt = as.POSIXct(date_time_gmt, format = '%Y-%m-%d %H:%M:%S'),
         phase = ifelse(phase == 'New Moon', 'New', 'Full'),
         date = as.Date(date_time_gmt, format = '%m/%d/%y', tz = 'GMT'))

#########
## esda 
#########

## facet wrap of water levels across transects and sites
# sm.plot <- ggplot(df, aes(date_time_gmt, water_level_navd88)) + 
#   geom_smooth(na.rm = T, aes(color = site_new, linetype = type)) + 
#   scale_y_continuous(name = 'Water Level (m NAVD88)', limits = c(-0.2, 1.2)) + 
#   labs(x = 'Date') + 
#   theme_bw(base_size = 20) + 
#   facet_wrap(~ transect)
# sm.plot

## export facet wrap
# png(paste0(datadir, '/figures/FacetWrap_', int.date1, "-to-", 
#            int.date2, '.png'), units = 'in', width = 10, height = 6, res = 150)
# sm.plot
# dev.off()

## averages by unit time
df.avg <- df %>%
  mutate(prd = floor_date(date_time_gmt, "month")) %>%
  group_by(transect, site_new, type, prd) %>%
  summarize(avg = mean(water_level_navd88))

tt_facet <- ggplot(df.avg, aes(prd, avg*3.28084, group = site_new)) + 
  geom_point(aes(color = site_new 
                 # ,shape = type
                 ), size = 0.5) + 
  geom_smooth(method = lm, se = F, lwd=0.5, color = 'black') +
  scale_y_continuous(name = 'Monthly Mean Water Level (ft NAVD88)', 
                     breaks = seq(-1,4,1), limits = c(-1,4)) + 
  scale_x_datetime(name = 'Date') + 
  scale_color_discrete(name = 'Site') + 
  scale_shape_discrete(name = 'Type') + 
  # theme(
  #   legend.text=element_text(size=rel(0.5))) + 
  facet_wrap(~ transect)
tt_facet

tiff(paste0(datadir, 'figures/transect_trends_faceted.tiff'), unit = 'in', height = 5, width = 6.5, res = 300)
tt_facet
dev.off()

## monthly high water means
df.mhhw <- df %>%
mutate(prd = floor_date(date_time_gmt, "day")) %>%
  group_by(transect, site_new, prd) %>%
  summarise(max = max(water_level_navd88)) %>%
  mutate(prd2 = floor_date(prd, "month")) %>%
  group_by(transect, site_new, prd2) %>%
  summarize(avg = mean(max))

ggplot(df.mhhw, aes(prd2, avg, group = site_new)) + 
  geom_line(aes(color = site_new)) + 
  # geom_smooth(method = lm, se = F) + 
  facet_wrap(~ transect)

## plot temperatures all time
ggplot(df, aes(water_temp_c)) + 
  geom_histogram(bins = 25) + 
  scale_x_continuous(breaks = seq(0,120, 5))

## summary of number of days active by site
active.time <- df %>%
  group_by(sitename_new) %>%
  summarise(days = n_distinct(date)) %>%
  mutate(weeks = days/7, years = weeks/52)
# arrange(factor(site, years))

df.active <- active.time[order(active.time$sitename_new, decreasing = TRUE),]

## export active time for all sites
jpeg(paste0(datadir, "figures/sites-deployment-time.jpg"), width = 7, height = 5, units = 'in', res = 150)
par(mar=c(4,10,4,4))
barplot(df.active$years, names.arg = df.active$sitename_new,
        horiz = T, 
        las = 1,
        ylab = '',
        xlim = c(0,5),
        xlab = 'Years Active',
        main = 'Hog Hammock Water Level Survey: Active Logging Time')
dev.off()

## sites active time by date
df.date <- df %>%
  group_by(site_new, serial, logger) %>%
  mutate(date = as.Date(date, "%Y-%m-%d")) %>%
  arrange(date) %>%
  mutate(start_date = first(date),
         end_date = last(date)) %>%
  # arrange(sitename_new) %>%
  # mutate(sitename_new = factor(sitename_new, levels=sitename_new)) %>%
  summarise(start_date = first(start_date), end_date = last(end_date), type = first(type))

sites.timeline <- 
  ggplot(df.date) +
  geom_linerange(aes(x = reorder(site_new, desc(site_new)),
                     ymax = end_date,
                     ymin = start_date,
                 linetype = type,
                 color = logger),
                 show.legend = T) +
  # geom_errorbar(aes(x = reorder(sitename_new, desc(sitename_new)),
  #                   ymax = as.Date("2000-01-01"),
  #                   ymin = as.Date('2010-01-01'),
  #                   linetype = type),
  #               size = 1,
  #               show.legend = T) + ## trick for making horizontal legend bars
  scale_y_date(name = "Year", date_breaks = "1 year", date_minor_breaks = '3 months', date_labels = "%Y") + 
               # limits = c(first(df.date$start_date), last(df.date$end_date))) + 
  # scale_color_manual()
  xlab('Transect-Site') + 
  scale_color_manual(name='Logger Type',
                     breaks=c('hobo', 'van essen'),
                     values=c('hobo' = 'black',
                              'van essen' = 'red'),
                     labels = c('Hobo', 'Van Essen')) +
  scale_linetype_manual(name='Site Type',
                        breaks=c('creek', 'ditch'),
                        values=c('creek' = 'solid',
                              'ditch' = 'dashed'),
                     labels = c('Creek', 'Ditch')) +
  # ggtitle('Hog Hammock Water Level Survey\nDeployment Date Range') + 
  coord_flip() + 
  theme(legend.position = 'bottom')
sites.timeline

tiff(paste0(datadir, "figures/sites-deployment-dates.tiff"), width = 6.5, height = 5, units = 'in', res = 300)
sites.timeline
dev.off()


#################################################################################
# DAILY HIGH TIDES create graphing function for daily highest tides' water level
# https://www.reed.edu/data-at-reed/resources/R/loops_with_ggplot2.html
#################################################################################
TEXT = 10 ## set font size for figures
ht.graph <- function(df, na.rm = TRUE, ...){
  
  ## filter to daily high tide
  df <- df %>%
    group_by(sitename_new, date) %>%
    slice_max(water_level_navd88, with_ties = FALSE) %>%
    # select(date_time_gmt, water_depth_m, salinity) %>%
    ungroup() %>%
    arrange(date)
  
  # create list of logger sites in data to loop over 
  sites_list <- unique(df$sitename_new)
  
  # create for loop to produce ggplot2 graphs 
  for (i in seq_along(sites_list)) {
    
    df2 <- filter(df, sitename_new == sites_list[i] & date >= first(date) & date <= last(date))
    
    ## set parameters
    my.formula <- y ~ x # generic formula for use in equation
    # D <- P+1
    
    # create plot for each site in df 
    plot <- 
      ggplot(df2)  + 
      geom_point(aes(date, water_level_navd88, color = logger), size = 0.5) + 
      geom_smooth(aes(date, water_level_navd88), method = 'lm', formula = my.formula) +
      # geom_hline(aes(yintercept = mean(water_depth_m)), linetype = 'dashed', df2) +
      # geom_point(aes(date_time_gmt, TP_mm/100), data = ht.TP, color = 'blue', size = 0.5) +
      # geom_line(aes(date_time_gmt, salinity/25), lwd = 0.5, color = 'blue') +
      scale_fill_manual(values = c('white', 'black')) + 
      # scale_x_datetime(name = 'Month/Year', date_breaks = '3 month', date_minor_breaks = '1 month', date_labels = '%m/%y') +
      # scale_y_continuous(name = 'Water Level (m NAVD88)', breaks = seq(0,2.0,0.1), limits = c(0,2.0), expand = c(0,0),
      #                    sec.axis = sec_axis(~., breaks = seq(0,2.0,0.1))
      # ) +
      scale_x_datetime(name = 'Month/Year', date_breaks = '3 month', date_minor_breaks = '1 month', date_labels = '%m/%y') +
      scale_y_continuous(name = 'Water Level (m NAVD88)', breaks = seq(0,2.0,0.1), expand = c(0,0),
                         sec.axis = sec_axis(~., breaks = seq(0,2.0,0.1))
      ) +
      # annotate("rect",
      #          xmin = as.POSIXct(paste(ht.date1, '00:48:00')),
      #          xmax = as.POSIXct(paste(ht.date1, '23:48:00')),
      #          ymin = 0, 
      #          ymax = df2$well_ht,
      #          alpha = 0.1) +
      # annotate("text",
      #          x = as.POSIXct(paste(ht.date1, '06:48:00')),
      #          y = df2$well_ht+0.1,
      #          label = 'Well Height',
      #          angle = 90) +
      # geom_text(aes(as.POSIXct('2018-10-13 00: 48:00'), df2$well_ht, label = 'well height')) +
      # labs(fill = 'Moon Phase', caption = "Dashed line indicates mean water level.") + 
      theme(axis.title = element_text(size = TEXT),
            axis.text = element_text(color = "black", size = TEXT),
            axis.ticks.length = unit(-0.2, 'cm'),
            axis.ticks = element_line(color = 'black'),
            axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), 
            axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")),
            axis.line = element_line(color = 'black'),
            axis.text.y.right = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm"), color = 'black'),
            axis.title.y.right = element_text(color = 'black'),
            axis.line.y.right = element_line(color = "black"), 
            axis.ticks.y.right = element_line(color = "black"),
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
      ggtitle(paste0(sites_list[i], " (", df2$type, ")"," - Daily High Tide Trend From ", first(df2$date), ' to ', last(df2$date)))
    
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
# 12-MIN INTERVALS create graphing function for 12-minute intervals over specified interval 
## using water depth
# https://www.reed.edu/data-at-reed/resources/R/loops_with_ggplot2.html
##############################################################################################
TEXT = 15 ## set font size for figures
int.graph <- function(df, na.rm = TRUE, ...){
  
  # create list of logger sites in data to loop over 
  sites_list <- unique(df$site_new)
  
  # create date filter for most recent # months of date for interval graphs
  n <- 3 # set number of months
  
  # create for loop to produce ggplot2 graphs 
  for (i in seq_along(sites_list)) {
    
    df2 <- df %>%
      arrange(date) %>%
      filter(site_new == sites_list[i]) %>%
      filter(date >= seq(floor_date(max(date), 'month'),
                         length.out = n + 1, by = '-1 month'))
    
    ## filter TP data to match interval water data
    int.TP <- filter(TP, date >= first(df2$date) & date <= last(df2$date)) %>%
      filter(TP_mm > 0)
    
    ## filter lunar data to match water data
    int.lnr <- filter(lnr, date >= first(df2$date) & date <= last(df2$date))
    
    # create plot for each site in df 
    plot <- 
      ggplot(df2)  + 
      geom_line(aes(date_time_gmt, water_depth_m)) +  ## convert to feet then add MLLW base elevation
      geom_hline(aes(yintercept = mean(water_depth_m)), linetype = 'dashed', df2) +
      geom_point(aes(date_time_gmt, TP_mm/100), data = int.TP, size = 1, color = 'blue') +
      # geom_line(aes(date_time_gmt, salinity/25), lwd = 0.5, color = 'blue') +
      geom_point(aes(date_time_gmt, 1.5, fill = phase), data = int.lnr, shape = 21, size = 5) +
      geom_text(aes(date_time_gmt, 1.5, label = dist_rad), data = int.lnr, vjust = -1) + 
      scale_fill_manual(values = c('white', 'black')) + 
      scale_x_datetime(name = 'Month/Year', date_breaks = '1 month', date_labels = '%m/%y') + 
      scale_y_continuous(name = 'Water Depth (m)', breaks = seq(-0.1,1.8,0.1), limits = c(-0.1,1.8), expand = c(0,0),
                         sec.axis = sec_axis(~. * 100, breaks = seq(0,180, 10),
                                           name = expression(paste('Total Daily Precipitation (mm)'))),
                         # sec.axis = sec_axis(~. * 25, breaks = seq(0,45, 5),
                         #                     name = expression(paste('Salinity (psu)'))),
      ) +
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
            legend.position = c(0.1, 0.94),
            legend.text = element_text(size = TEXT),
            legend.title = element_text(size = TEXT),
            # legend.key = element_blank(),
            legend.box.background = element_rect(color = 'black'),
            plot.title = element_text(size = TEXT, face = "bold")) + 
      guides(fill=guide_legend(title="Moon Phase")) + 
      ggtitle(paste0(sites_list[i], " - 12-minute Interval From ", first(df2$date), ' to ', last(df2$date)))
    
    # save plots as .png
    ggsave(plot, file=paste(datadir,
                            'figures/', 'NAVD88 ', 'Interval-12-minute ', sites_list[i], ".png", sep=''), width = 6, height = 5, units = 'in', scale=2)
    
  }
}

# run graphing function on long df
int.graph(df)


##############################################################################################
# SALINITY individual graphs
# https://www.reed.edu/data-at-reed/resources/R/loops_with_ggplot2.html
##############################################################################################

TEXT = 15 ## set font size for figures
sal.graph <- function(df, na.rm = TRUE, ...){
  
  # create list of logger sites in data to loop over 
  sites_list <- unique(df$site_new)
  
  # create date filter for most recent # months of date for interval graphs
  n <- 3 # set number of months
  
  # create for loop to produce ggplot2 graphs 
  for (i in seq_along(sites_list)) {
    
    df2 <- df %>%
      arrange(date) %>%
      filter(site_new == sites_list[i]) %>%
      filter(salinity > 0)
      # filter(date >= seq(floor_date(max(date), 'month'),
      #                    length.out = n + 1, by = '-1 month'))
    
    ## filter TP data to match interval water data
    # int.TP <- filter(TP, date >= first(df2$date) & date <= last(df2$date)) %>%
    #   filter(TP_mm > 0)
    
    # create plot for each site in df 
    plot <- 
      ggplot(df2)  + 
      geom_line(aes(date_time_gmt, salinity), lwd = 0.5, color = 'blue') +
      # geom_point(aes(date_time_gmt, TP_mm/100), data = int.TP, size = 1, color = 'blue') +
      scale_x_datetime(name = 'Date', date_breaks = '2 week',
                       date_minor_breaks = '1 week',
                       date_labels = '%m/%d/%y') + 
      scale_y_continuous(name = 'Salinity (psu)', 
                         breaks = seq(0,45,5),
                         limits = c(0,45)
      ) +
    theme(axis.title = element_text(size = TEXT),
          axis.text = element_text(color = "black", size = TEXT),
          axis.ticks.length = unit(-0.2, 'cm'),
          axis.ticks = element_line(color = 'black'),
          axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm"), angle = 45, vjust = 1, hjust=1), 
          axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")),
          axis.line = element_line(color = 'black'),
          axis.text.y.right = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm"), color = 'blue'),
          axis.title.y.right = element_text(color = 'blue'),
          axis.line.y.right = element_line(color = "blue"), 
          axis.ticks.y.right = element_line(color = "blue"),
          panel.background = element_rect(fill = FALSE, color = 'black'),
          # panel.grid = element_blank(),
          panel.grid.major.x = element_line('grey', size = 0.5, linetype = "dashed"),
          panel.grid.minor.x = element_line('grey', size = 0.5, linetype = "dotted"),
          panel.grid.major.y = element_line('grey', size = 0.5, linetype = "dashed"),
          panel.grid.minor.y = element_line('grey', size = 0.5, linetype = "dotted"),
          plot.margin = margin(0.5,0.5,0.5,0.5, 'cm'),
          legend.position = c(0.1, 0.94),
          legend.text = element_text(size = TEXT),
          legend.title = element_text(size = TEXT),
          # legend.key = element_blank(),
          legend.box.background = element_rect(color = 'black'),
          plot.title = element_text(size = TEXT, face = "bold")) + 
      guides(fill=guide_legend(title="Moon Phase")) + 
      ggtitle(paste0(sites_list[i], " - Salinity From ", first(df2$date), ' to ', last(df2$date)))
    
    # save plots as .png
    ggsave(plot, file=paste(datadir,
                            'figures/', 'Salinity ', sites_list[i], ".png", sep=''), width = 6, height = 5, units = 'in', scale=2)
    
  }
}

# run graphing function on long df
sal.graph(df)


##############################################################################################
# TRANSECTS HYDROGRAPHS create graphing function for 12-minute intervals over specified interval
# https://www.reed.edu/data-at-reed/resources/R/loops_with_ggplot2.html
##############################################################################################
TEXT = 15 ## set font size for figures
tx.graph <- function(df, na.rm = TRUE, ...){
  
  # create list of logger sites in data to loop over 
  transect_list <- unique(df$transect)
  
  # create for loop to produce ggplot2 graphs 
  for (i in seq_along(transect_list)) {
    
    df2 <- filter(df, transect == transect_list[i] & date_time_gmt >= int.date1 & date_time_gmt <= int.date2)
    
    daily.mn <- df2 %>%
      mutate(date = floor_date(date_time_gmt, unit = 'day')) %>%
      group_by(transect, site_new, date) %>%
      summarize(mean = mean(water_level_navd88))
    
    # create plot for each site in df 
    plot <- 
      ggplot(df2)  + 
      geom_line(aes(date_time_gmt, water_level_navd88*3.28084, color = site_new)) + 
      geom_line(aes(date, mean*3.28084, color = site_new), 
                data = filter(daily.mn)) +
      # geom_hline(aes(yintercept = mean(water_level_navd88)), linetype = 'dashed', df2) +
      # geom_point(aes(date_time_gmt, TP_mm/100), data = int.TP, size = 1, color = 'red') +
      # geom_line(aes(date_time_gmt, salinity/25), lwd = 0.5, color = 'blue') +
      # geom_point(aes(date_time_gmt, 1.5, fill = phase), data = int.lnr, shape = 21, size = 5) +
      # geom_text(aes(date_time_gmt, 1.5, label = dist_rad), data = int.lnr, vjust = -1) + 
      # scale_fill_manual(values = c('white', 'black')) + 
      scale_x_datetime(name = paste0('Month/Day/', year(int.date1)), date_breaks = '1 week', date_labels = '%m/%d') + 
      scale_y_continuous(name = 'Water Level (ft NAVD88)', breaks = seq(0,6.5,0.5), limits = c(0,6.5), expand = c(0,0)) +
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
    labs(color = 'Site Name') + 
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
          legend.position = 'bottom',
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


#####################################################################
## explore transects 1 and 5 hydrological connections via site NW corner
#####################################################################
df.t <- filter(df, site_new %in% c('T5-03', 'T3-03', 'T4-02') & date_time_gmt >= int.date1 & date_time_gmt <= int.date2)

daily.mn <- df.t %>%
  mutate(date = floor_date(date_time_gmt, unit = 'day')) %>%
  group_by(transect, site_new, date) %>%
  summarize(mean = mean(water_level_navd88))

TEXT = 8 ## set font size for figures

plot <- ggplot(df.t)  + 
  geom_line(aes(date_time_gmt, water_level_navd88 * 3.28084, color = site_new), linewidth = 0.2) + 
  geom_line(aes(date, mean * 3.28084, color = site_new), 
            data = filter(daily.mn), linewidth = 0.2) +
  # geom_hline(aes(yintercept = mean(water_level_navd88)), linetype = 'dashed', df2) +
  # geom_point(aes(date_time_gmt, TP_mm/100), data = int.TP, size = 1, color = 'red') +
  # geom_line(aes(date_time_gmt, salinity/25), lwd = 0.5, color = 'blue') +
  # geom_point(aes(date_time_gmt, 1.5, fill = phase), data = int.lnr, shape = 21, size = 5) +
  # geom_text(aes(date_time_gmt, 1.5, label = dist_rad), data = int.lnr, vjust = -1) + 
  # scale_fill_manual(values = c('white', 'black')) + 
  scale_x_datetime(name = paste0('Month/Day/', year(int.date1)), date_breaks = '1 week', date_labels = '%m/%d', date_minor_breaks = '1 day') + 
  scale_y_continuous(name = 'Water Level (ft NAVD88)', breaks = seq(-1,7,1), limits = c(-1,7), expand = c(0,0)) +
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
labs(color = 'Transect-Site') + 
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
        # panel.grid = element_blank(),
        panel.grid.major.x = element_line('grey', size = 0.5, linetype = "dashed"),
        panel.grid.minor.x = element_line('grey', size = 0.5, linetype = "dotted"),
        panel.grid.major.y = element_line('grey', size = 0.5, linetype = "dashed"),
        panel.grid.minor.y = element_line('grey', size = 0.5, linetype = "dotted"),
        # plot.margin = margin(0.5,0.5,0.5,0.5, 'cm'),
        # legend.position = c(0.15, 0.85),
        legend.position = 'bottom',
        legend.text = element_text(size = TEXT),
        legend.title = element_text(size = TEXT),
        # legend.box.background = element_rect(color = 'black'),
        plot.title = element_text(size = TEXT, face = "bold"))
  # ggtitle(paste0(transect_list[i], " - 12-minute Interval From ", int.date1, ' to ', int.date2))
plot

tiff(paste0(datadir, 'figures/comparison-ditch-inland.tiff'), unit = 'in', height = 5, width = 6.5, res = 300)
plot
dev.off()

t501 <- filter(df, site_new == 'T5-01' 
               & date == '2022-11-20'
               )
ggplot(filter(t501), aes(date_time_gmt, water_level_navd88)) + 
  geom_line()
