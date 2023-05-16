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
int.date1 <- as.Date('2022-10-01') 
int.date2 <- as.Date('2023-12-31') 

# set dates for daily high tide graphs
ht.date1 <- as.Date('2018-10-01') 
ht.date2 <- as.Date('2023-04-25')

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

ggplot(TP, aes(TP_mm)) +
  geom_histogram()

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
           serial = str_sub(filz[i], -22,-19),
           water_temp_c = as.numeric(water_temp_c),
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
                                                                                          if_else(site == 'Site-14', 'Tidal Gate', 
                                                                                                  if_else(site == 'Site-19', 'The Trunk',
                                                                                                          if_else(site == 'Site-16', 'South Oakdale', 
                                                                                                                  if_else(site == 'Site-18', 'NW Corner', 
                                                                                                                          if_else(site == 'Site-20', 'Walker', 
                                                                                                                                  if_else(site == 'Site-23', 'Hillery',
                                                                                                                                          if_else(site == 'Site-24', 'Johnson', site))))))))))))))))) %>%
    mutate(sitename = paste(site, name)) %>%
    select(!(abs_pres_psi))
  tidal <- rbind(OUT, tidal)
}

tidal.01 <- tidal %>%
  mutate(logger = 'hobo') %>%
  select(date_time_gmt, water_temp_c, water_level_C, date, site, name, sitename, logger, serial)

## import & tidy van essen water level data
## note water level C is in meters and indicates water level in reference to top of wellcap (negative numbers indicate below for VE data)
for(i in 1:length(filz.ve)) {
  OUT <- fread(filz.ve[i],
               select = c(1:3),
               col.names = c('date_time_gmt', 'water_level_C', 'water_temp_c'),
               stringsAsFactors = FALSE) %>%
    slice(., 1:(n()-2)) %>% ## removes first and last ## readings
    mutate(date_time_gmt = ymd_hms(date_time_gmt),
           date = as.Date(date_time_gmt, '%y/%m/%d', tz = 'GMT'),
           site = str_sub(filz.ve[i], -26,-25),
           serial = str_sub(filz.ve[i], -23,-19),
           water_level_C = as.numeric(water_level_C)/1000 * -1) %>%
    mutate(site = paste('Site', site, sep = '-')) %>%
    mutate(name = if_else(site == 'Site-07', 'Cactus Patch', 
                          if_else(site == 'Site-09', 'Mr. Tracy',
                                  if_else(site == 'Site-11', 'Library',
                                          if_else(site == 'Site-13', 'Purple Ribbon',
                                                  if_else(site == 'Site-14', 'Tidal Gate',
                                                          if_else(site == 'Site-15', 'Oakdale',
                                                                  if_else(site == 'Site-19', 'The Trunk', site)))))))) %>%
    mutate(sitename = paste(site, name))
  tidal.ve <- rbind(OUT, tidal.ve)
}

tidal.ve2 <- tidal.ve %>%
  mutate(date_time_gmt = as.POSIXct(date_time_gmt),
         logger = 'van essen') %>%
  select(date_time_gmt, water_temp_c, water_level_C, date, site, name, sitename, logger, serial)

tidal1 <- rbind(tidal.01, tidal.ve2)

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
           water_temp_c = as.numeric(water_temp_c),
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
# p <- ggplot(tidal.psu2, aes(date_time_gmt, salinity)) + geom_line(lwd = 0.1)
# q <- p + facet_grid(rows = vars(site))


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

## filter extreme values & convert F temps to C temps
tidal3 <- tidal2 %>% 
  filter(!(water_level_C < -4 | water_level_C >2.5)) %>%
  mutate(water_temp_c = as.numeric(water_temp_c)) %>%
  mutate(water_temp_c = if_else(water_temp_c >= 40, (water_temp_c -32) * 5/9, water_temp_c)) %>%
  rename(site = site.x)


## export combined data
# write.csv(tidal3, paste(datadir, 'wls_data.csv'))


####################################################
## esda of smoothed water levels across sites
####################################################
date1 <- as.Date('2018-10-01') 
date2 <- as.Date('2023-04-25') 
  
tidal4 <- tidal3 %>%
  mutate(type = ifelse(site %in% c('Site-13', 'Site-11', 'Site-09', 'Site-05', 'Site-19', 'Site-14', 'Site-15', 'Site-18', 'Site-20', 'Site-23', 'Site-24'), 'ditch', 
                       ifelse(site %in% c('Site-02', 'Site-03', 'Site-07', 'Site-06', 'Site-12', 'Site-16'), 'creek', site))) %>% ## ditches sites
  # filter(site %in% c('Site-02', 'Site-03', 'Site-07', 'Site-06', 'Site-12')) %>% ## marsh sites
  mutate(transect = ifelse(site %in% c('Site-06', 'Site-12', 'Site-19', 'Site-13', 'Site-18'), 'T1', 
                           ifelse(site %in% c('Site-02', 'Site-03', 'Site-05','Site-11', 'Site-23'), 'T2',
                                  ifelse(site %in% c('Site-07', 'Site-09'), 'T3N', 
                                         ifelse(site %in% c('Site-16', 'Site-15'), "T5",
                                                ifelse(site %in% c('Site-20'), 'T4',
                                                       ifelse(site %in% c('Site-07', 'Site-24'), 'T3S', site))))))) 

## working on renaming sites to be more logical related to transects; site-05 is a branch of T2
# tidal5 <- tidal4 %>%
#   mutate(site_new = if_else(site = 'Site-02', 'T2-01',
#                             if_else()))
  
sites <- tidal4 %>%
  filter(date_time_gmt >= date1 & date_time_gmt <= date2) %>%
  select(site, type, transect, date_time_gmt, water_depth_m, water_level_navd88, water_temp_c)

# write.csv(sites, paste0(datadir, 'sapelo-ditch-water-levels-2019.csv'), row.names = FALSE) ## for A.W.

sm.plot <- ggplot(sites, aes(date_time_gmt, water_level_navd88)) + 
  geom_smooth(na.rm = T, aes(linetype = site, color = type)) + 
  scale_y_continuous(name = 'Water Level (m NAVD88)', limits = c(-0.2, 1.2)) + 
  labs(x = 'Date') + 
  theme_bw(base_size = 20)
sm.plot

png(paste0(datadir, '/figures/Smoothed_', date1, "-to-", 
           date2, '.png'), units = 'in', width = 10, height = 6, res = 150)
sm.plot
dev.off()


##########################
## averages by unit time
##########################
df <- sites %>%
  mutate(prd = floor_date(date_time_gmt, "week")) %>%
  group_by(site, prd) %>%
  summarize(avg = mean(water_level_navd88)) %>%
  filter(site != 'Site-19')

ggplot(df, aes(prd, avg, group = site)) + 
  geom_point(aes(color = site)) + 
  geom_smooth(method = lm, se = F)

## plot temperatures
ggplot(tidal3, aes(water_temp_c)) + 
  geom_histogram(bins = 25) + 
  scale_x_continuous(breaks = seq(0,120, 5))
  
## daily high tide
ht <- tidal3 %>%
  group_by(site, date) %>%
  slice_max(water_depth_m, with_ties = FALSE) %>%
  # select(date_time_gmt, water_depth_m, salinity) %>%
  ungroup()

## summary of number of days active by site
active.time <- tidal3 %>%
  group_by(site, sitename) %>%
  summarise(days = n_distinct(date)) %>%
  mutate(weeks = days/7, years = weeks/52)
  # arrange(factor(site, years))

# Increase margin size

df <- active.time[order(active.time$years, decreasing = TRUE),]

jpeg(paste(datadir, "sites-active-time.jpg"), width = 7, height = 5, units = 'in', res = 150)
par(mar=c(4,10,4,4))
barplot(df$years, names.arg = df$sitename,
        horiz = T, 
        las = 1,
        ylab = '',
        xlim = c(0,5),
        xlab = 'Years Active',
        main = 'Water Level Survey Sites')
dev.off()


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
ht.graph(ht)


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
    
    # save plots as .pdf
    # ggsave(plot, file=paste(results, 
    #                        'projection_graphs/county_graphs/',
    #                        count_list[i], ".pdf", sep=''), scale=2) 
    
    # print plots to screen
    # print(plot)
  }
}

# run graphing function on long df
int.graph(tidal3)

## adding transect IDs
tidal4 <- tidal3 %>%
  mutate(transect = if_else(site %in% c('Site-06', 'Site-12', 'Site-19', 'Site-13', 'Site 18'), 'lot1-transect',
                            if_else(site %in% c('Site-02', 'Site-03','Site-05', 'Site-11', 'Site-23'), 'stlukes-transect', 
                                    if_else(site %in% c('Site-07', 'Site-09', 'Site-26'), 'tracys-transect',
                                            if_else(site %in% c('Site-15'), 'oakdale-transect', 
                                                    if_else(site %in% c('Site-20'), 'walker-transect','NA'))))))


##############################################################################################
# TRANSECTS create graphing function for 12-minute intervals over specified interval using water depth
# https://www.reed.edu/data-at-reed/resources/R/loops_with_ggplot2.html
##############################################################################################
TEXT = 15 ## set font size for figures
int.graph <- function(df, na.rm = TRUE, ...){
  
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
int.graph(tidal4)
