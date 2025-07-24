################################################################
## comparing elevation of sites against other projects
## take ensemble averages of project sites
################################################################
rm(list=ls())

library(tidyverse)
library(lubridate)
library(readxl)
library(colorspace)
library(sf)
library(dataRetrieval) ## https://cran.r-project.org/web/packages/dataRetrieval/vignettes/dataRetrieval.html
Sys.setenv(TZ='GMT')
# options(scipen=999)

## define data directory
datadir <- '/Users/dhardy/Dropbox/r_data/sapelo/water-level/'

############################
## import & tidy field data
############################

## define column classes
## import cleaned water level data
df <- read_csv(paste(datadir, 'wls_data.csv'))[,-1] %>%
  mutate(date = as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S", tz = 'UTC'),
         date = as.POSIXct(date)) %>%
  arrange(date_time_gmt)

# Hudson River, Meridian, GA
# some answers about pcode and statcd here: https://github.com/DOI-USGS/dataRetrieval/issues/438
siteNumbers <- "USGS-022035975"
parameterCode <- "00065" ## gage height data
statCode <- "00021" ## tidal high-high values; https://help.waterdata.usgs.gov/stat_code
statCode <- "00021" ##; 
start.date <- as.character(first(df$date)) ## earliest available date in dataset
end.date <- as.character(last(df$date)) # last available date in dataset
# + duration(0.2, units = "year")))) ## dl X number years after first date

## https://water.code-pages.usgs.gov/dataRetrieval/reference/read_waterdata_daily.html
## read in tidal high-high values
ml.mhhw <- read_waterdata_daily(
  monitoring_location_id = siteNumbers,
  parameter_code = parameterCode,
  statistic_id = statCode,
  time = c(start.date, end.date),
  skipGeometry = T
) %>%
  st_drop_geometry() %>%
  rename(water_level_navd88 = value,
         date = time) %>%
  mutate(water_level_navd88 = (water_level_navd88) * 0.3048) %>%
  select(date, water_level_navd88)
ml.mhhw$date <- as.Date(ml.mhhw$date) ## convert datetime column to correct format

########################################
## calculate ensemble averages of mhhw
########################################

## ensemble average of monthly mhhw at sites over study period
## different lengths of time for each site, need sd included
ea.mhhw.cwbp <- df %>%
  mutate(prd = floor_date(date_time_gmt, "day")) %>%
  group_by(transect, site, prd) %>%
  summarise(max = max(water_level_navd88)) %>%
  # mutate(month = floor_date(prd, "month")) %>%
  mutate(month = month(prd)) %>%
  mutate(year = year(prd)) %>%
  group_by(transect, site, month) %>%
  summarize(avg = mean(max), count = n(), sd = sd(max)) %>%
  mutate(se = sd/sqrt(count)) %>% 
  mutate(source = 'CWBP')
  
## ensemble average of monthly mhhw at ML over study period
ea.mhhw.ml <- ml.mhhw %>%
  mutate(month = month(date)) %>%
  group_by(month) %>%
  summarize(avg = mean(water_level_navd88), sd = sd(water_level_navd88), count = n()) %>%
  mutate(se = sd/sqrt(count)) %>% 
  mutate(
    transect = 'Hudson Creek',
    site = 'ML', source = 'USGS') %>%
  select(transect, site, month, avg, count, sd, se, source)

## merge ML and CWBP ensemble data
ea.mhhw <- rbind(ea.mhhw.cwbp, ea.mhhw.ml)

## monthly mhhw ensemble average comparisons
t.var <- 'T1'
TEXT = 10
ea_comps.mhhw <- ea.mhhw %>%
  # filter(!site_new %in% c('T5-01', 'T5-02')) %>%
  filter(avg > 0 & transect %in% c(t.var, 'Hudson Creek')
        #  & site != 'T3-BR-01'
         ) %>%
  ggplot(aes(month, avg)) + 
  # geom_point(aes(color = source)) + 
  geom_pointrange(aes(ymin = avg - (se), ymax = avg + (se), color = site), size = 0.25, position = position_dodge(width = 0.25)) + 
  # geom_hline(aes(yintercept = mean(avg)), linetype = 'dashed', color = 'red', data = filter(ea.mhhw, site == t.var)) + ## measured MHHW at CWBP sites
  # geom_hline(aes(yintercept = 3.1791339 * 0.3048), linetype = 'dotted', color = 'red') +  ## NOAA MHHW for Community
  # geom_hline(aes(yintercept = mean(avg)), linetype = 'dashed', color = 'black', data = filter(ea.mhhw, source == 'USGS')) + ## measured MHHW at CWBP sites
  # geom_hline(aes(yintercept = 3.3038058* 0.3048), linetype = 'dotted', color = 'black') + ## NOAA MHHW for ML
  scale_y_continuous(name = "Monthly MHHW Level (m NAVD88)", breaks = seq(0.9, 1.4, 0.1), minor_breaks = seq(0.9,1.4,0.01), limits = c(0.8, 1.4)) + 
  scale_x_continuous(name = "Month", breaks = seq(1,12,1), limits = c(0.5,12.5)) +
  # scale_x_date(name = 'Year', 
  #              date_breaks = '1 year', date_labels = '%Y', 
  #              date_minor_breaks = '3 months',
  #              limits = c(ymd("2018-07-01"), ymd("2024-03-31")),
  #              expand = c(0,0)) +
  scale_color_manual(name = 'Site: ', values = qualitative_hcl(6)) +
  # scale_linetype_manual(name = "MHHW: ", values = c(2,3,3,2),
  #                       guide = guide_legend(override.aes = list(color = c("red", "black", 'black', 'red')))) +
  # geom_smooth(method = lm, se = F) + 
  # facet_wrap(~site_new) + 
  # ggtitle('Monthly Mean Higher High Water') + 
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
        # panel.grid.minor.x = element_line('grey', size = 0.5, linetype = "dotted"),
        # panel.grid.minor.y = element_line('grey', size = 0.5, linetype = "dotted"),
        plot.margin = margin(0.5,0.5,0.5,0.5, 'cm'),
        legend.position = 'bottom',
        legend.text = element_text(size = TEXT),
        legend.title = element_text(size = TEXT),
        legend.box.background = element_rect(color = 'black'),
        plot.title = element_text(size = TEXT, face = "bold"))
# guides(fill=guide_legend(nrow=2,byrow=TRUE))
ea_comps.mhhw

tiff(paste0(datadir, 'figures/mhhw-ensemble-t1-comparisons.tiff'), unit = 'in', height = 5, width = 6.5, res = 300)
ea_comps.mhhw
dev.off()

png(paste0(datadir, 'figures/mhhw-ensemble-t1-comparisons_slide.png'), unit = 'in', height = 5, width = 6.5, res = 150)
ea_comps.mhhw
dev.off()

#####################################################
## calculate ensemble averages of mean water levels
#####################################################

## read in 15-min interval data aka "unit value" data
siteNo = "022035975"
ml.uv <- readNWISuv(siteNumbers = siteNo,
                    parameterCd = parameterCode,
                    startDate = start.date,
                    endDate = end.date) %>%
  rename(water_level_navd88 = X_00065_00000,
         quality = X_00065_00000_cd,
         date_time_gmt = dateTime) %>%
  mutate(water_level_navd88 = water_level_navd88 * 0.3048,
         site_new = 'ML') %>%
  select(date_time_gmt, site_new, water_level_navd88)

## ensemble average of monthly mean water levels sites across study period years
## different lengths of time for each site, need sd included
ea.mn.cwbp <- df %>%
  mutate(prd = floor_date(date_time_gmt, "day")) %>%
  group_by(transect, site, prd) %>%
  summarise(mwl = mean(water_level_navd88)) %>%
  # mutate(month = floor_date(prd, "month")) %>%
  mutate(month = month(prd)) %>%
  mutate(year = year(prd)) %>%
  group_by(transect, site, month) %>%
  summarize(avg = mean(mwl), count = n(), sd = sd(mwl)) %>%
  mutate(se = sd/sqrt(count)) %>% 
  mutate(source = 'CWBP')

## ensemble average of monthly mean water levels sites across study period years
## different lengths of time for each site, need sd included
ea.mn.ml <- ml.uv %>%
  mutate(prd = floor_date(date_time_gmt, "day")) %>%
  group_by(prd) %>%
  summarise(mwl = mean(water_level_navd88)) %>%
  # mutate(month = floor_date(prd, "month")) %>%
  mutate(month = month(prd)) %>%
  mutate(year = year(prd)) %>%
  group_by(month) %>%
  summarize(avg = mean(mwl), count = n(), sd = sd(mwl)) %>%
  mutate(se = sd/sqrt(count)) %>% 
  mutate(
    transect = 'Hudson Creek',
    site = 'ML', source = 'USGS') %>%
  mutate(source = 'ML')

## merge ML and CWBP ensemble data
ea.mn <- rbind(ea.mn.cwbp, ea.mn.ml)

## monthly mean water level ensemble average comparisons
t.var <- 'T1'
TEXT = 10
ea_comps.mn <- ea.mn %>%
  # filter(!site_new %in% c('T5-01', 'T5-02')) %>%
  filter(transect %in% c(t.var, 'Hudson Creek')
         #  & site != 'T3-BR-01'
  ) %>%
  ggplot(aes(month, avg)) + 
  # geom_point(aes(color = source)) + 
  geom_pointrange(aes(ymin = avg - (se), ymax = avg + (se), color = site), size = 0.25, position = position_dodge(width = 0.25)) + 
  # geom_hline(aes(yintercept = mean(avg)), linetype = 'dashed', color = 'red', data = filter(ea.mhhw, site == t.var)) + ## measured MHHW at CWBP sites
  # geom_hline(aes(yintercept = 3.1791339 * 0.3048), linetype = 'dotted', color = 'red') +  ## NOAA MHHW for Community
  # geom_hline(aes(yintercept = mean(avg)), linetype = 'dashed', color = 'black', data = filter(ea.mhhw, source == 'USGS')) + ## measured MHHW at CWBP sites
  # geom_hline(aes(yintercept = 3.3038058* 0.3048), linetype = 'dotted', color = 'black') + ## NOAA MHHW for ML
  scale_y_continuous(name = "Monthly Mean Water Level (m NAVD88)", breaks = seq(-0.2, 1.2, 0.1), minor_breaks = seq(-0.2,1.2,0.1), limits = c(-0.2, 1.2)) + 
  scale_x_continuous(name = "Month", breaks = seq(1,12,1), limits = c(0.5,12.5)) +
  # scale_x_date(name = 'Year', 
  #              date_breaks = '1 year', date_labels = '%Y', 
  #              date_minor_breaks = '3 months',
  #              limits = c(ymd("2018-07-01"), ymd("2024-03-31")),
  #              expand = c(0,0)) +
  scale_color_manual(name = 'Site: ', values = qualitative_hcl(6)) +
  # scale_linetype_manual(name = "MHHW: ", values = c(2,3,3,2),
  #                       guide = guide_legend(override.aes = list(color = c("red", "black", 'black', 'red')))) +
  # geom_smooth(method = lm, se = F) + 
  # facet_wrap(~site_new) + 
  # ggtitle('Monthly Mean Higher High Water') + 
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
        # panel.grid.minor.x = element_line('grey', size = 0.5, linetype = "dotted"),
        # panel.grid.minor.y = element_line('grey', size = 0.5, linetype = "dotted"),
        plot.margin = margin(0.5,0.5,0.5,0.5, 'cm'),
        legend.position = 'bottom',
        legend.text = element_text(size = TEXT),
        legend.title = element_text(size = TEXT),
        legend.box.background = element_rect(color = 'black'),
        plot.title = element_text(size = TEXT, face = "bold"))
# guides(fill=guide_legend(nrow=2,byrow=TRUE))
ea_comps.mn

tiff(paste0(datadir, 'figures/mean-ensemble-t1-comparisons.tiff'), unit = 'in', height = 5, width = 6.5, res = 300)
ea_comps.mn
dev.off()

png(paste0(datadir, 'figures/mean-ensemble-t1-comparisons_slide.png'), unit = 'in', height = 5, width = 6.5, res = 150)
ea_comps.mn
dev.off()
