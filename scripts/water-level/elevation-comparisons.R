################################################################
## comparing elevation of sites against other projects
################################################################
rm(list=ls())

library(tidyverse)
library(lubridate)
library(readxl)
library(dataRetrieval) ## https://cran.r-project.org/web/packages/dataRetrieval/vignettes/dataRetrieval.html
Sys.setenv(TZ='GMT')

## define data directory
datadir <- '/Users/dhardy/Dropbox/r_data/sapelo/water-level/'

######################
## import & tidy data
#####################

## define column classes
## import cleaned water level data
df <- read_csv(paste(datadir, 'wls_data.csv'))[,-1] %>%
  mutate(date_time_gmt = as.POSIXct(date_time_gmt, format = "%Y-%m-%d %H:%M:%S", tz = 'GMT'),
         date = as.POSIXct(date)) %>%
  arrange(date_time_gmt)

# Hudson River, Meridian, GA
siteNo <- "022035975"
pCode <- "00065" ## gage height data
statCode <- "00021" ## tidal high-high values
start.date <- first(df$date) ## earliest available date
end.date <- last(df$date)

ml <- readNWISdv(siteNumbers = siteNo,
                 parameterCd = pCode,
                 startDate = start.date,
                 endDate = end.date,
                 statCd = statCode) %>%
  rename(water_level_navd88 = X_00065_00021,
         quality = X_00065_00021_cd,
         date = Date) %>%
  mutate(type = 'high', water_level_navd88 = water_level_navd88 * 0.3048)

ml$date <- as.Date(ml$date) ## convert datetime column to correct format

## monthly high water means at sites
df.mhhw <- df %>%
  mutate(prd = floor_date(date_time_gmt, "day")) %>%
  group_by(transect, site_new, prd) %>%
  summarise(max = max(water_level_navd88)) %>%
  mutate(month = floor_date(prd, "month")) %>%
  group_by(transect, site_new, month) %>%
  summarize(avg = mean(max)) %>%
  mutate(source = 'CWBP')

## monthly high water means at ML
ml.mhhw <- ml %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month) %>%
  summarize(avg = mean(water_level_navd88)) %>%
  mutate(transect = 'Hudson Creek', site_new = 'ML', source = 'USGS') %>%
  select(transect, site_new, month, avg, source)

mhhw <- rbind(df.mhhw, ml.mhhw) %>%
  mutate(date = as.Date(month))
  
comps <- mhhw %>%
  # filter(transect %in% c('Hudson Creek', 'T1')) %>%
  ggplot(aes(date, avg, group = site_new)) + 
  geom_point(aes(color = source)) + 
  scale_y_continuous(name = "Elevation (m NAVD88)", breaks = seq(0.6, 1.6, 0.2), limits = c(0.6,1.6)) + 
  scale_x_date(name = 'Mo/Yr', date_breaks = '4 month', date_labels = '%m/%y') +
  # geom_smooth(method = lm, se = F) + 
  # facet_wrap(~site_new) + 
  ggtitle('Monthly Mean Higher High Water') + 
  scale_color_manual(name = 'Source', values = c('red', 'black'), labels = c('CWBP Sites', 'Meridian Landing')) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.position = 'bottom')
comps 

png(paste0(datadir, 'figures/mhhw-comparisons.png'), unit = 'in', height = 5, width = 6.5, res = 150)
comps
dev.off()

## import site characteristics/info 
wls.info <- read.csv(file.path(datadir, 'wls-info.csv'))

## prep for plotting
wls2 <- wls.info %>%
  gather('source', 'meters', 8:20) %>%
  mutate(datum = if_else(str_detect(source, 'mhhw'), 'mhhw', 
                         if_else(str_detect(source, 'mllw'), 'mllw', 
                                 if_else(str_detect(source, 'cgep|rtk|usgs'), 'navd88', 'na')))) %>%
  mutate(project = if_else(str_detect(source, '2019'), 'USGS', 
                           if_else(str_detect(source, '2010'), 'CGEP', 
                                   if_else(str_detect(source, 'rtk'), 'CWBP', 'na'))))

# New facet label names for datum variables
dat.labs <- c("MHHW", "MLLW", "NAVD88")
names(dat.labs) <- c("mhhw", "mllw", "navd88")

## plot site elevations using different sources and datums
elvs <- wls2 %>%
  filter(!source %in% c('well_ht', 'rtkcap_navd88', 'noaa2019', 'rtk_cgep', 'rtk_usgs', 'usgs_cgep') &
           project != 'na') %>%
  # filter(source %in% c('usgs2019', 'cgep2010', 'rtk_site', 'mhhw2019', 'mhhw2010')) %>%
  ggplot(aes(transect_site, meters, color = project, shape = type)) + 
  geom_point() + 
  scale_y_continuous(name = "Elevation (m NAVD88)", breaks = seq(-2.6, 2.6, 0.2)) + 
  scale_x_discrete(name = 'Transect-Site') + 
  scale_color_manual(name='Project',
                     breaks=c('CGEP', 'USGS', 'CWBP'),
                     values=c('CGEP'='green3', 'USGS'='black', 'CWBP'='red')) + 
  scale_shape_manual(name='Type',
                     breaks=c('creek', 'ditch'),
                     values=c('creek'= 16, 'ditch'= 17)) + 
  facet_wrap(~datum, labeller = labeller(datum = dat.labs)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = 'bottom')
elvs

png(paste0(datadir, 'figures/site-elevations.png'), unit = 'in', height = 6, width = 10, res = 150)
elvs
dev.off()

elvd <- wls2 %>%
  filter(source %in% c('rtk_cgep', 'rtk_usgs', 'usgs_cgep')) %>%
  # filter(source %in% c('usgs2019', 'cgep2010', 'rtk_site', 'mhhw2019', 'mhhw2010')) %>%
  ggplot(aes(transect_site, meters, color = source, shape = type)) + 
  geom_point() + 
  scale_y_continuous(name = "Elevation (m NAVD88)", breaks = seq(-1.6, 0.4, 0.2)) + 
  scale_x_discrete(name = 'Transect-Site') + 
  # scale_color_manual(name='Project',
  #                    breaks=c('CGEP', 'USGS', 'CWBP'),
  #                    values=c('CGEP'='red', 'USGS'='blue', 'CWBP'='green3')) + 
  scale_shape_manual(name='Type',
                     breaks=c('creek', 'ditch'),
                     values=c('creek'= 16, 'ditch'= 17)) + 
  facet_wrap(~datum) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
elvd

png(paste0(datadir, 'figures/site-elev_differences.png'), unit = 'in', height = 6, width = 10, res = 150)
elvd
dev.off()

##############################################################################################
# TRENDS in water level over study period
# https://www.reed.edu/data-at-reed/resources/R/loops_with_ggplot2.html
##############################################################################################

trends.graph <- function(df.mhhw, na.rm = TRUE, ...){
  
  # create list of logger sites in data to loop over 
  trans_list <- unique(df.mhhw$transect)
  
  ## set parameters
  TEXT = 15 ## set font size for figures
  my.formula <- y ~ x # generic formula for use in equation

  # create for loop to produce ggplot2 graphs 
  for (i in seq_along(trans_list)) {
    
  t.mhhw <- df.mhhw %>%
    filter(transect == trans_list[i])

plot <-  ggplot(t.mhhw) + 
  geom_point(aes(month, avg, color = site_new), size = 0.5) +
  geom_smooth(aes(month, avg, color = site_new), method = 'lm', formula = my.formula, se = F) +
  # geom_hline(aes(yintercept = mean(water_depth_m)), linetype = 'dashed', df2) +
  # geom_point(aes(date_time_gmt, TP_mm/100), data = ht.TP, color = 'blue', size = 0.5) +
  # geom_line(aes(date_time_gmt, salinity/25), lwd = 0.5, color = 'blue') +
  scale_fill_manual(values = c('white', 'black')) + 
  # scale_x_datetime(name = 'Month/Year', date_breaks = '3 month', date_minor_breaks = '1 month', date_labels = '%m/%y') +
  # scale_y_continuous(name = 'Water Level (m NAVD88)', breaks = seq(0,2.0,0.1), limits = c(0,2.0), expand = c(0,0),
  #                    sec.axis = sec_axis(~., breaks = seq(0,2.0,0.1))
  # ) +
  scale_x_datetime(name = 'Month/Year', date_breaks = '6 month', date_minor_breaks = '3 month', date_labels = '%m/%y') +
  scale_y_continuous(name = 'Monhtly MHHW (m NAVD88)', breaks = seq(0,1.5,0.1), limits = c(0,1.5), expand = c(0,0)
                     # sec.axis = sec_axis(~., breaks = seq(0,1.5,0.1))
  ) +
scale_color_viridis_d(name = 'Site', option = 'turbo') +
theme(axis.title = element_text(size = TEXT),
      axis.text = element_text(color = "black", size = TEXT),
      axis.ticks.length = unit(-0.2, 'cm'),
      axis.ticks = element_line(color = 'black'),
      axis.text.x = element_text(angle = 90, vjust = 0, hjust=0, margin=unit(c(0.5,0.5,0.5,0.5), "cm")), 
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
      panel.grid.major.y = element_line('grey', size = 0.5, linetype = "dotted"),
      plot.margin = margin(0.5,0.5,0.5,0.5, 'cm'),
      # legend.position = c(0.1, 0.92),
      legend.text = element_text(size = TEXT),
      legend.title = element_text(size = TEXT),
      # legend.key = element_blank(),
      legend.box.background = element_rect(color = 'black'),
      plot.title = element_text(size = TEXT, face = "bold")) + 
  facet_wrap(~site_new) + 
  ggtitle(paste0("Trend in Monthly MHHW for Transect ", trans_list[i]))
  
  # save plots as .png
  ggsave(plot, file=paste(datadir,
                          'figures/', 'Transect-Trends ', trans_list[i], ".png", sep=''), width = 6.5, height = 6.5, units = 'in', scale=2)
  
  }
}

trends.graph(df.mhhw)
