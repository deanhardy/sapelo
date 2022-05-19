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
int.date1 <- as.Date('2022-03-01') 
int.date2 <- as.Date('2022-05-12')

# set dates for daily high tide graphs
ht.date1 <- as.Date('2018-10-01') 
ht.date2 <- as.Date('2022-04-30')

## import water level data files
filz <- list.files(path = file.path(datadir, 'new-logger-data/hobo'),
                   pattern= '*.csv',
                   full.names = TRUE,
                   recursive = TRUE) 
tidal <- NULL

filz.ve <- list.files(path = file.path(datadir, 'new-logger-data/vanessen'),
                   pattern= '*.CSV',
                   full.names = TRUE,
                   recursive = TRUE) 
tidal.ve <- NULL

## convert salinity Excel files to csv
sal <- list.files(path = file.path(datadir, 'new-logger-data/salinity'),
                       pattern= '*.xlsx',
                       full.names = TRUE,
                       recursive = TRUE) 
created <- mapply(convert, sal, gsub("xlsx", "csv", sal))
unlink(sal) # delete xlsx files

filz.psu <- list.files(path = file.path(datadir, 'new-logger-data/salinity'),
                      pattern= '*.csv',
                      full.names = TRUE,
                      recursive = TRUE) 
tidal.psu <- NULL

## import mllw elevation including lidar and RTK adjusted elevations 
elev <- read.csv(file.path(datadir, 'site-elevations.csv'))

## import daily precipitation totals
TP <- read.csv(file.path(datadir, 'nerr-data/SAPMLMET_TP.csv')) %>%
  select(date_time_gmt, TP_mm) %>%
  filter(date_time_gmt >= ht.date1 & date_time_gmt <= ht.date2,
         TP_mm > 0) %>%
  mutate(date_time_gmt = as.POSIXct(date_time_gmt, format = '%Y-%m-%d %H:%M:%S'))

int.TP <- filter(TP, date_time_gmt >= int.date1 & date_time_gmt <= int.date2)

## import lunar data
lnr <- read.csv(file.path(datadir, 'lunar.csv')) %>%
  filter(date_time_gmt >= ht.date1 & date_time_gmt <= ht.date2 & phase %in% c('New Moon', 'Full Moon')) %>%
  mutate(date_time_gmt = as.POSIXct(date_time_gmt, format = '%Y-%m-%d %H:%M:%S'),
         phase = ifelse(phase == 'New Moon', 'New', 'Full'))

int.lnr <- filter(lnr, date_time_gmt >= int.date1 & date_time_gmt <= int.date2)

## import & tidy hobo water level data
## note water level C is in meters and indicates water level in reference to top of wellcap (negative numbers indicate below for Hobo)
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
                                                          if_else(site == 'Site-09', 'Mr. Tracy',
                                                                  if_else(site == 'Site-11', 'Library',
                                                                          if_else(site == 'Site-12', 'Mr. Smith',
                                                                                  if_else(site == 'Site-13', 'Purple Ribbon',
                                                                                          if_else(site == 'Site-14', 'Tidal Gate', site))))))))))) %>%
    mutate(sitename = paste(site, name)) %>%
    select(!(abs_pres_psi))
  tidal <- rbind(OUT, tidal)
}

## import & tidy van essen water level data
## note water level C is in meters and indicates water level in reference to top of wellcap (negative numbers indicate below for VE data)
for(i in 1:length(filz.ve)) {
  OUT <- fread(filz.ve[i],
               select = c(1:3),
               col.names = c('date_time_est', 'water_level_C', 'water_temp_c'),
               stringsAsFactors = FALSE) %>%
    # slice(., 5:(n()-7)) %>% ## removes first and last ## readings
    mutate(date_time_est = ymd_hms(date_time_est),
           date = as.Date(date_time_est, '%y/%m/%d', tz = 'EST'),
           site = str_sub(filz.ve[i], -26,-25),
           water_level_C = as.numeric(water_level_C)/1000 * -1) %>%
    mutate(site = paste('Site', site, sep = '-')) %>%
    mutate(name = if_else(site == 'Site-15', 'Oakdale',
                          if_else(site == 'Site-07', 'Cactus Patch', 
                                  if_else(site == 'Site-09', 'Mr. Tracy',
                                          if_else(site == 'Site-11', 'Library', 
                                                  if_else(site == 'Site-13', 'Purple Ribbon',
                                                          if_else(site == 'Site-19', 'The Trunk', 
                                                                  if_else(site == 'Site-14', 'Tidal Gate', site)))))))) %>%
    mutate(sitename = paste(site, name))
  tidal.ve <- rbind(OUT, tidal.ve)
}

tidal.ve2 <- tidal.ve %>%
  mutate(date_time_gmt = as.POSIXct(date_time_est + hours(5))) %>%
  select(date_time_gmt, water_temp_c, water_level_C, date, site, name, sitename)

tidal1 <- rbind(tidal, tidal.ve2)

## import & tidy van essen specific conductivity/salinity data
## note water level C is in meters and indicates water level in reference to top of wellcap (negative numbers indicate below for VE data)
for(i in 1:length(filz.psu)) {
  OUT <- fread(filz.psu[i],
               select = c(2:8),
               col.names = c('date_time_est', 'pressure', 'water_temp_c', 'conductivity', 'water_level_C', 'datum_reference', 'salinity'),
               stringsAsFactors = FALSE) %>%
    # slice(., 5:(n()-7)) %>% ## removes first and last ## readings
    mutate(date_time_est = ymd_hms(date_time_est),
           date = as.Date(date_time_est, '%y/%m/%d', tz = 'EST'),
           site = str_sub(filz.psu[i], -26,-25),
           water_level_C = as.numeric(water_level_C)/1000 * -1) %>%
    mutate(site = paste('Site', site, sep = '-')) %>%
    mutate(name = if_else(site == 'Site-02', 'Snagtree',
                          if_else(site == 'Site-03', 'St. Lukes',
                                  if_else(site == 'Site-05', 'Graball',
                                          if_else(site == 'Site-06', 'Dani Trap',
                                                  if_else(site == 'Site-07', 'Cactus Patch',
                                                          if_else(site == 'Site-09', 'Mr. Tracy',
                                                                  if_else(site == 'Site-11', 'Library',
                                                                          if_else(site == 'Site-12', 'Mr. Smith',
                                                                                  if_else(site == 'Site-13', 'Purple Ribbon',
                                                                                          if_else(site == 'Site-14', 'Tidal Gate', 
                                                                                                  if_else(site == 'Site-15', 'Oakdale', 
                                                                                                          if_else(site == 'Site-19', 'The Trunk', site))))))))))))) %>%   
             mutate(sitename = paste(site, name))
  tidal.psu <- rbind(OUT, tidal.psu)
}

options(scipen=999)

tidal.psu2 <- tidal.psu %>%
  mutate(date_time_gmt = as.POSIXct(date_time_est + hours(5))) %>%
  select(date_time_gmt, site, sitename, salinity) %>%
  filter(salinity < 50 | is.na(salinity))

## plot salinity at all sites
## this plot still needs lots of work as does the data collection process
p <- ggplot(tidal.psu2, aes(date_time_gmt, salinity)) + geom_line(lwd = 0.1)
q <- p + facet_grid(rows = vars(site))
q

# png(q, filename = paste(datadir, 'figures/', 'Salinity', '.png', sep = ''), width = 9, height = 6.5, units = 'in', res = 300)
# q
# dev.off()

tidal1.1 <- full_join(tidal1, tidal.psu2, by = c('sitename', 'date_time_gmt')) %>%
  select(!(site.y))

SN <- elev$name

tidal2 <- NULL
## adding references for water level from well cap to substrate (depth) and NAVD88
for (i in 1:length(SN)) {
  
  el2 <- elev %>%
  filter(name == SN[[i]]) 
  
  OUT2 <- tidal1.1 %>%
    filter(name == SN[[i]]) %>%
    mutate(water_depth_m = water_level_C + el2$well_ht_m,
           water_level_navd88 = el2$wellcap_navd88_m + water_level_C,
           well_ht = el2$well_ht)
  
  tidal2 <- rbind(OUT2, tidal2)
}

## filter extreme values
tidal3 <- tidal2 %>% filter(!(water_level_C < -4 | water_level_C >2.5))
tidal2 <- tidal3

## daily high tide
ht <- tidal2 %>%
  group_by(site, date) %>%
  slice_max(water_depth_m, with_ties = FALSE) %>%
  # select(date_time_gmt, water_depth_m, salinity) %>%
  ungroup()

## summary of number of days active by site
active.time <- tidal2 %>%
  group_by(site, sitename) %>%
  summarise(days = n_distinct(date)) %>%
  mutate(weeks = days/7, years = weeks/52) %>%
  arrange(factor(site, years))

# Increase margin size

df <- active.time[order(active.time$years, decreasing = TRUE),]

jpeg(paste(datadir, "sites-active-time.jpg"), width = 7, height = 5, units = 'in', res = 150)
par(mar=c(4,10,4,4))
barplot(df$years, names.arg = df$sitename,
        horiz = T, 
        las = 1,
        ylab = '',
        xlim = c(0,4),
        xlab = 'Years Active',
        main = 'Water Level Survey Sites')
dev.off()


########################################################
# create graphing function for daily high tides
# https://www.reed.edu/data-at-reed/resources/R/loops_with_ggplot2.html
########################################################
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
ht.graph(ht)


########################################################
# create graphing function for 12-minute intervals over specified interval
# https://www.reed.edu/data-at-reed/resources/R/loops_with_ggplot2.html
########################################################
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
      geom_line(aes(date_time_gmt, water_depth_m)) +  ## convert to feet then add MLLW base elevation
      geom_hline(aes(yintercept = mean(water_depth_m)), linetype = 'dashed', df2) +
      geom_point(aes(date_time_gmt, TP_mm/100), data = int.TP, size = 1, color = 'red') +
      geom_line(aes(date_time_gmt, salinity/25), lwd = 0.5, color = 'blue') +
      geom_point(aes(date_time_gmt, 1.5, fill = phase), data = int.lnr, shape = 21, size = 5) +
      geom_text(aes(date_time_gmt, 1.5, label = dist_rad), data = int.lnr, vjust = -1) + 
      scale_fill_manual(values = c('white', 'black')) + 
      scale_x_datetime(name = 'Month', date_breaks = '1 month', date_labels = '%m') + 
      scale_y_continuous(name = 'Water Level & Total Daily Precipitation x10 (meters)', breaks = seq(0,1.8,0.1), limits = c(0,1.8), expand = c(0,0),
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
                            'figures/', 'Interval-12-minute ', sites_list[i], ".png", sep=''), width = 6, height = 5, units = 'in', scale=2)
    
    # save plots as .pdf
    # ggsave(plot, file=paste(results, 
    #                        'projection_graphs/county_graphs/',
    #                        count_list[i], ".pdf", sep=''), scale=2) 
    
    # print plots to screen
    # print(plot)
  }
}

# run graphing function on long df
int.graph(tidal2)
