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
int.date1 <- as.Date('2022-09-01') 
int.date2 <- as.Date('2022-10-31') 

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
        main = 'Hog Hammock Water Level Survey: Deployment Length')
dev.off()

## sites active time by date
df.date <- df %>%
  group_by(sitename_new) %>%
  mutate(date = as.Date(date, "%Y-%m-%d")) %>%
  arrange(date) %>%
  mutate(start_date = first(date),
         end_date = last(date)) %>%
  # arrange(sitename_new) %>%
  # mutate(sitename_new = factor(sitename_new, levels=sitename_new)) %>%
  summarise(start_date = first(start_date), end_date = last(end_date), type = first(type))

sites.timeline <- 
  ggplot(df.date) +
  geom_linerange(aes(x = reorder(sitename_new, desc(sitename_new)),
                     ymax = end_date,
                     ymin = start_date,
                 linetype = type),
                 show.legend = T) +
  # geom_errorbar(aes(x = reorder(sitename_new, desc(sitename_new)),
  #                   ymax = as.Date("2000-01-01"),
  #                   ymin = as.Date('2010-01-01'),
  #                   linetype = type),
  #               size = 1,
  #               show.legend = T) + ## trick for making horizontal legend bars
  scale_y_date(name = "Year", date_breaks = "1 year", date_minor_breaks = '3 months', date_labels = "%Y") + 
               # limits = c(first(df.date$start_date), last(df.date$end_date))) + 
  xlab('Site') + 
  ggtitle('Hog Hammock Water Level Survey: Deployment Date Range') + 
  coord_flip() + 
  theme_bw()
sites.timeline

jpeg(paste0(datadir, "figures/sites-deployment-dates.jpg"), width = 7, height = 5, units = 'in', res = 150)
sites.timeline
dev.off()


########################################################################
# create graphing function for daily highest tides' water depth
# https://www.reed.edu/data-at-reed/resources/R/loops_with_ggplot2.html
########################################################################
TEXT = 10 ## set font size for figures
ht.graph <- function(df.ht, na.rm = TRUE, ...){
  
  ## filter to daily high tide
  df <- df %>%
    group_by(sitename_new, date) %>%
    slice_max(water_level_navd88, with_ties = FALSE) %>%
    # select(date_time_gmt, water_depth_m, salinity) %>%
    ungroup()
  
  # create list of logger sites in data to loop over 
  sites_list <- unique(df$sitename_new)
  
  # create for loop to produce ggplot2 graphs 
  for (i in seq_along(sites_list)) {
    
    df2 <- filter(df, sitename_new == sites_list[i] & date_time_gmt >= ht.date1 & date_time_gmt <= ht.date2)
    
    ## set parameters
    my.formula <- y ~ x # generic formula for use in equation
    # D <- P+1
    
    # create plot for each site in df 
    plot <- 
      ggplot(df2)  + 
      geom_point(aes(date_time_gmt, water_level_navd88, color = logger), size = 0.5) + 
      geom_smooth(aes(date_time_gmt, water_level_navd88), method = 'lm', formula = my.formula) +  
      # geom_hline(aes(yintercept = mean(water_depth_m)), linetype = 'dashed', df2) +
      # geom_point(aes(date_time_gmt, TP_mm/100), data = TP, color = 'red', size = 0.5) +
      # geom_line(aes(date_time_gmt, salinity/25), lwd = 0.5, color = 'blue') +
      scale_fill_manual(values = c('white', 'black')) + 
      scale_x_datetime(name = 'Month/Year', date_breaks = '3 month', date_minor_breaks = '1 month', date_labels = '%m/%y') + 
      scale_y_continuous(name = 'Water Level (m NAVD88)', breaks = seq(0,1.8,0.1), limits = c(0,1.8), expand = c(0,0),
                         sec.axis = sec_axis(~., breaks = seq(0,1.8,0.1))
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
      ggtitle(paste0(sites_list[i], " (", df2$type, ")"," - Daily High Tide Trend From ", ht.date1, ' to ', ht.date2))
    
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
# TRANSECTS STACKED create graphing function for 12-minute intervals over specified interval
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
      group_by(transect, sitename_new, date) %>%
      summarize(mean = mean(water_level_navd88))
    
    # create plot for each site in df 
    plot <- 
      ggplot(df2)  + 
      geom_line(aes(date_time_gmt, water_level_navd88, color = sitename_new)) + 
      geom_line(aes(date, mean, color = sitename_new), 
                data = filter(daily.mn)) +
      # geom_hline(aes(yintercept = mean(water_level_navd88)), linetype = 'dashed', df2) +
      # geom_point(aes(date_time_gmt, TP_mm/100), data = int.TP, size = 1, color = 'red') +
      # geom_line(aes(date_time_gmt, salinity/25), lwd = 0.5, color = 'blue') +
      # geom_point(aes(date_time_gmt, 1.5, fill = phase), data = int.lnr, shape = 21, size = 5) +
      # geom_text(aes(date_time_gmt, 1.5, label = dist_rad), data = int.lnr, vjust = -1) + 
      # scale_fill_manual(values = c('white', 'black')) + 
      scale_x_datetime(name = paste0('Month/Day/', year(int.date1)), date_breaks = '1 week', date_labels = '%m/%d') + 
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
          legend.position = c(0.2, 0.9),
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
 