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

######################
## import & tidy data
#####################

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
# pCode <- "00003" ## sampling depth feet
# statCode <- "00003" ## mean values
start.date <- as.character(first(df$date)) ## earliest available date in dataset
end.date <- as.character(last(df$date)) # last available date in dataset
                                   # + duration(0.2, units = "year")))) ## dl X number years after first date

## https://water.code-pages.usgs.gov/dataRetrieval/reference/read_waterdata_daily.html
ml <- read_waterdata_daily(
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


ml$date <- as.Date(ml$date) ## convert datetime column to correct format

#####################################
## compare ML MHHW to site MHHW
#####################################

## CWBP monthly MHHW at sites
mo.mhhw.cwbp <- df %>%
  mutate(prd = floor_date(date_time_gmt, "day")) %>%
  group_by(transect, site, prd) %>%
  summarise(max = max(water_level_navd88)) %>%
  mutate(month = floor_date(prd, "month")) %>%
  group_by(transect, site, month) %>%
  summarize(avg = mean(max), count = n(), sd = sd(max)) %>%
  mutate(se = sd/sqrt(count)) %>% 
  mutate(source = 'CWBP')

## ensemble average of monthly mhhw at sites across study period years
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

## monthly MHHW at ML
mo.mhhw.ml <- ml %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month) %>%
  summarize(avg = mean(water_level_navd88), sd = sd(water_level_navd88), count = n()) %>%
  mutate(se = sd/sqrt(count)) %>% 
  mutate(
    transect = 'Hudson Creek',
         site = 'ML', source = 'USGS') %>%
  select(transect, site, month, avg, count, sd, se, source)

## monthly MHHW at ML
ea.mhhw.ml <- ml %>%
  mutate(month = month(date)) %>%
  group_by(month) %>%
  summarize(avg = mean(water_level_navd88), sd = sd(water_level_navd88), count = n()) %>%
  mutate(se = sd/sqrt(count)) %>% 
  mutate(
    transect = 'Hudson Creek',
    site = 'ML', source = 'USGS') %>%
  select(transect, site, month, avg, count, sd, se, source)

mo.mhhw.cwbp2 <- mo.mhhw.cwbp %>% ungroup() %>% select(!transect)
## %>% select(!transect) ## drop transect for joining
  
## merge ML and CWBP data
mo.mhhw <- rbind(mo.mhhw.cwbp, mo.mhhw.ml) %>%
  mutate(date = as.Date(month))

## merge ML and CWBP ensemble data
ea.mhhw <- rbind(ea.mhhw.cwbp, ea.mhhw.ml)

## calc monthly MHHW avg for cwbp sites
# cwbp.avg <- mo.mhhw %>%
#   filter(avg > 2) %>%
#   group_by(month) %>%
#   summarise(avg = mean(avg), se = se(avg))

## plot monthly mhhw trends by transect
tt_facet <- ggplot(filter(mo.mhhw.cwbp, site %in% c('T1-02','T2-01','T3-03','T4-01')), aes(month, avg*3.28084, group = site)) + 
  geom_point(aes(color = site 
                 # ,shape = type
  ), size = 1) + 
  geom_smooth(method = lm, se = F, lwd=0.5, color = 'black') +
  scale_y_continuous(name = 'Monthly MHHW (ft NAVD88)', 
                     breaks = seq(2,4,0.5), limits = c(2,4)) + 
  scale_x_datetime(name = 'Date') + 
  scale_color_discrete(name = 'Site') + 
  scale_shape_discrete(name = 'Type') + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.position = 'none',
        text = element_text(size = 24),
        panel.background = element_rect(color = 'grey10', fill = 'white', linewidth = 0.5),
        panel.grid = element_line(color = 'grey90')) +
  facet_wrap(~ site)
tt_facet

png(paste0(datadir, 'figures/transect_mhhw_trends_faceted_slide.png'), unit = 'in', height = 6.5, width = 13.33, res = 150)
tt_facet
dev.off()

## monthly mhhw comparisons
t.var <- 'T1-02'
TEXT = 24
comps <- mo.mhhw %>%
  # filter(!site_new %in% c('T5-01', 'T5-02')) %>%
  filter(avg > 0 & site %in% c(t.var, 'ML')) %>%
  ggplot(aes(date, avg)) + 
  # geom_point(aes(color = source)) + 
  geom_pointrange(aes(ymin = avg - (se), ymax = avg + (se), color = source)) + 
  geom_hline(aes(yintercept = mean(avg)), linetype = 'dashed', color = 'red', data = filter(mo.mhhw, site == t.var)) + ## measured MHHW at CWBP sites
  geom_hline(aes(yintercept = 3.1791339 * 0.3048), linetype = 'dotted', color = 'red') +  ## NOAA MHHW for Community
  geom_hline(aes(yintercept = mean(avg)), linetype = 'dashed', color = 'black', data = filter(mo.mhhw, source == 'USGS')) + ## measured MHHW at CWBP sites
  geom_hline(aes(yintercept = 3.3038058* 0.3048), linetype = 'dotted', color = 'black') + ## NOAA MHHW for ML
  scale_y_continuous(name = "Water Level (m NAVD88)", breaks = seq(0.5, 1.5, 0.1), limits = c(0.5, 1.5)) + 
  scale_x_date(name = 'Year', 
               date_breaks = '1 year', date_labels = '%Y', 
               date_minor_breaks = '3 months',
               limits = c(ymd("2018-07-01"), ymd("2024-03-31")),
               expand = c(0,0)) +
  scale_color_manual(name = 'Monthly MHHW: ', values = c('red', 'black'), labels = c(t.var, 'ML USGS')) +
  scale_linetype_manual(name = "MHHW: ", values = c(2,3,3,2),
                        guide = guide_legend(override.aes = list(color = c("red", "black", 'black', 'red')))) +
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
        panel.grid.minor.x = element_line('grey', size = 0.5, linetype = "dotted"),
        plot.margin = margin(0.5,0.5,0.5,0.5, 'cm'),
        legend.position = 'bottom',
        legend.text = element_text(size = TEXT),
        legend.title = element_text(size = TEXT),
        legend.box.background = element_rect(color = 'black'),
        plot.title = element_text(size = TEXT, face = "bold"))
  # guides(fill=guide_legend(nrow=2,byrow=TRUE))
comps 

tiff(paste0(datadir, 'figures/mhhw-comparisons.tiff'), unit = 'in', height = 5, width = 6.5, res = 300)
comps
dev.off()

png(paste0(datadir, 'figures/mhhw-comparisons_slide.png'), unit = 'in', height = 6.5, width = 13.33, res = 150)
comps
dev.off()

## monthly mhhw ensemble average comparisons
t.var <- 'T1'
TEXT = 10
ea_comps <- ea.mhhw %>%
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
ea_comps 

tiff(paste0(datadir, 'figures/mhhw-ensemble-t1-comparisons.tiff'), unit = 'in', height = 5, width = 6.5, res = 300)
ea_comps
dev.off()

# png(paste0(datadir, 'figures/mhhw-ensemble-t3-comparisons_slide.png'), unit = 'in', height = 6.5, width = 13.33, res = 150)
# ea_comps
# dev.off()

## calculate MHHW difference b/t community measured and NOAA modeled estimate 
comm_noaa <- mo.mhhw %>%
  filter(site_new == t.var) %>%
  summarise(mean(avg))
comm_noaa = pull(comm_noaa[1,3])
comm_noaa - (3.1791339 * 0.3048)

## calculate MHHW difference b/t ML USGS measured and NOAA modeled estimate 
ml_noaa <- mo.mhhw %>%
  filter(site_new == 'ML') %>%
  summarise(mean(avg))
ml_noaa = pull(ml_noaa[1,3])
ml_noaa - (3.1791339 * 0.3048)

#####################################
## compare elevation datums/projects
#####################################

## import site characteristics/info 
wls.info <- read.csv(file.path(datadir, 'wls-info.csv'))

## want to add error bars to mhhw plot, so need to calculate SD when calculating mean
## will use reported error for USGS and CGEP projects as well as for RTK
## http://www.sthda.com/english/wiki/ggplot2-error-bars-quick-start-guide-r-software-and-data-visualization

## MHHW at sites (all time)
cwbp.mhhw <- df %>%
  mutate(prd = floor_date(date_time_gmt, "day")) %>%
  rename(transect_site = site) %>%
  group_by(transect_site, prd) %>%
  summarise(max_wl = max(water_level_navd88)) %>%
  # mutate(month = floor_date(prd, "month")) %>%
  group_by(transect_site) %>%
  summarize(mhhw_wls88 = mean(max_wl), sd = sd(max_wl), count = n()) %>%
  mutate(se = sd/sqrt(count)) %>%
  select(transect_site, mhhw_wls88, count, sd, se)

## merge MMHW from sites to calculate mhhw for wls using noaa
wls.info2 <- merge(wls.info, cwbp.mhhw) %>%
  mutate(mhhw_wls = mhhw_wls88 - noaa2019_88) %>%
  relocate(c(sd, se, count), .after = last_col())

## prep for plotting
wls2 <- wls.info2 %>%
  gather('source', 'meters', 8:24) %>%
  mutate(datum = if_else(str_detect(source, 'mhhw'), 'mhhw', 
                         if_else(str_detect(source, 'mllw'), 'mllw', 'na'))) %>%
  mutate(datum = if_else(str_detect(source, '88'), 'navd88', datum)) %>%
  mutate(project = if_else(str_detect(source, '19'), 'USGS', 
                           if_else(str_detect(source, '10'), 'CGEP', 
                                   if_else(str_detect(source, 'rtk|wls'), 'CWBP', 'na'))),
         meters = signif(meters, 3),
         feet = meters * 3.28084) %>%
  select(transect_site, type, source, meters, datum, project, feet, count, sd, se) %>%
  mutate(sd_ft = if_else(source == 'mhhw_wls88', sd*3.28084, 0),
         se_ft = if_else(source == 'mhhw_wls88', se*3.28084, 0)) %>%
  mutate(sd = if_else(source == 'mhhw_wls88', sd, 0),
         se = if_else(source == 'mhhw_wls88', se, 0)) %>%
  mutate(count = if_else(source == 'mhhw_wls88', count, 0))

# New facet label names for datum variables
dat.labs <- c("MHHW", "MLLW", "NAVD88")
names(dat.labs) <- c("mhhw", "mllw", "navd88")

## plot site elevations using different sources and datums
elvs <- wls2 %>%
  filter(source %in% c('cgep2010_88', 'usgs2019_88', 'rtk_site_88',
                       'mllw2010', 'mllw2019', 'mllw_rtk',
                       'mhhw2010','mhhw2019', 'mhhw_rtk')) %>%
  ggplot(aes(transect_site, feet, color = project, shape = type)) + 
  geom_point() + 
  # scale_y_continuous(name = "Elevation (m)", breaks = seq(-3.4, 2.8, 0.4)) + 
  scale_y_continuous(name = "Elevation (ft)", breaks = seq(-12, 10, 2)) + 
  scale_x_discrete(name = 'Transect-Site') + 
  scale_shape_manual(name='Type',
                     breaks=c('creek', 'ditch'),
                     values=c('creek'= 16, 'ditch'= 17),
                     labels = c('Creek', 'Ditch')) + 
  scale_color_manual(name='Project',
                     breaks=c('CGEP', 'USGS', 'CWBP'),
                     values=c('CGEP'='green3', 'USGS'='black', 'CWBP'='red')) + 
  facet_wrap(~datum, labeller = labeller(datum = dat.labs)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = 'bottom')
elvs

tiff(paste0(datadir, 'figures/site-elevations.tiff'), unit = 'in', height = 6, width = 6.5, res = 300)
elvs
dev.off()

## plot site elevations using different sources and datums
elvs88 <- wls2 %>%
  filter(source %in% c('cgep2010_88', 'usgs2019_88', 'rtk_site_88')) %>%
  ggplot(aes(transect_site, feet, color = project, shape = type)) + 
  geom_point(size = 4) + 
  # scale_y_continuous(name = "Elevation (m)", breaks = seq(-3.4, 2.8, 0.4)) + 
  scale_y_continuous(name = "Elevation (ft NAVD88)", breaks = seq(-8, 6, 2), limits = c(-8,6)) + 
  scale_x_discrete(name = 'Transect-Site') + 
  scale_shape_manual(name='Type',
                     breaks=c('creek', 'ditch'),
                     values=c('creek'= 16, 'ditch'= 17),
                     labels = c('Creek', 'Ditch')) + 
  scale_color_manual(name='Project',
                     breaks=c('CGEP', 'USGS', 'CWBP'),
                     values=c('CGEP'='green3', 'USGS'='black', 'CWBP'='red')) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.position = 'right',
        text = element_text(size = 24),
        panel.background = element_rect(color = 'grey10', fill = 'white', linewidth = 0.5),
        panel.grid = element_line(color = 'grey90'))
elvs88

png(paste0(datadir, 'figures/site-elevations-navd88_slide.png'), unit = 'in', height = 6.5, width = 13.33, res = 150)
elvs88
dev.off()

## plot elev differences across sources (CGEP, USGS, CWBP)
elvd <- wls2 %>%
  filter(source %in% c('rtk_cgep_88', 'rtk_usgs_88', 'usgs_cgep_88')) %>%
  # filter(source %in% c('usgs2019', 'cgep2010', 'rtk_site', 'mhhw2019', 'mhhw2010')) %>%
  ggplot(aes(transect_site, feet, color = source, shape = type)) + 
  geom_point() + 
  # scale_y_continuous(name = "Elevation (m NAVD88)", breaks = seq(-1.6, 0.4, 0.2)) + 
  scale_y_continuous(name = "Elevation (ft NAVD88)", breaks = seq(-5, 2, 1)) + 
  scale_x_discrete(name = 'Transect-Site') + 
  # scale_color_manual(name='Project',
  #                    breaks=c('CGEP', 'USGS', 'CWBP'),
  #                    values=c('CGEP'='red', 'USGS'='blue', 'CWBP'='green3')) + 
  scale_shape_manual(name='Type',
                     breaks=c('creek', 'ditch'),
                     values=c('creek'= 16, 'ditch'= 17),
                     labels = c('Creek', 'Ditch')) + 
  scale_color_manual(name='Comparison',
                     breaks=c('rtk_cgep_88', 'rtk_usgs_88', 'usgs_cgep_88'),
                     values=c('rtk_cgep_88'='green3', 'rtk_usgs_88'='black', 'usgs_cgep_88'='red'),
                     labels = c('RTK - CGEP2010', 'RTK - USGS2019', "USGS2019 - CGEP2010" )) +
  # facet_wrap(~datum) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = 'bottom')
elvd

tiff(paste0(datadir, 'figures/site-elev_differences.tiff'), unit = 'in', height = 6, width = 10, res = 300)
elvd
dev.off()

## plot site mhhw elevations using different sources and datums
# mhhw.comps <- wls2 %>%
#   filter(source %in% c('mhhw_wls88', 'noaa2019_88')) %>%
#   ggplot(aes(transect_site, feet, color = source, shape = type)) + 
#   geom_point() + 
#   # scale_y_continuous(name = "MHHW Elevation (m NAVD88)", breaks = seq(-0.1, 1.2, 0.1)) + 
#   scale_y_continuous(name = "MHHW Elevation (ft NAVD88)", breaks = seq(-1, 4, 1)) + 
#   scale_x_discrete(name = 'Transect-Site') + 
#   scale_shape_manual(name='Type',
#                      breaks=c('creek', 'ditch'),
#                      values=c('creek'= 16, 'ditch'= 17),
#                      labels = c('Creek', 'Ditch')) + 
#   scale_color_manual(name='Data Source',
#                      breaks=c('noaa2019_88', 'mhhw_wls88'),
#                      values=c('noaa2019_88' = 'black',
#                               'mhhw_wls88' = 'red'),
#                      labels = c('NOAA 2019', 'CWPB')) +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
#         legend.position = 'bottom')
# mhhw.comps

# png(paste0(datadir, 'figures/site-mhhw-comps.png'), unit = 'in', height = 6, width = 10, res = 150)
# mhhw.comps
# dev.off()

p <- wls2 %>%
  filter(source %in% c('mhhw_wls88', 'noaa2019_88')) %>%
  # pivot_wider(names_from = source, values_from = c(feet, meters, datum)) %>%
  ggplot(aes(transect_site, feet, shape = type)) + 
  geom_pointrange(aes(ymin = feet - (se), ymax = feet + (se), color = project), size = 1) + 
  scale_y_continuous(name = "MHHW Elevation (ft NAVD88)", breaks = seq(0, 4, 0.5), limits = c(0,4)) + 
  scale_x_discrete(name = 'Transect-Site') + 
  scale_shape_manual(name='Type',
                     breaks=c('creek', 'ditch'),
                     values=c('creek'= 16, 'ditch'= 17),
                     labels = c('Creek', 'Ditch')) + 
  scale_color_manual(name='Data Source',
                     breaks=c('USGS', 'CWBP'),
                     values=c('USGS' = 'black',
                              'CWBP' = 'red'),
                     labels = c('NOAA 2019', 'CWBP')) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
      legend.position = 'right',
      text = element_text(size = 24),
      panel.background = element_rect(color = 'grey10', fill = 'white', linewidth = 0.5),
      panel.grid = element_line(color = 'grey90'))
p

tiff(paste0(datadir, 'figures/site-mhhw-comps.tiff'), unit = 'in', height = 4, width = 6.5, res = 300)
p
dev.off()

png(paste0(datadir, 'figures/site-mhhw-comps_slide.png'), unit = 'in', height = 6.5, width = 13.33, res = 150)
p
dev.off()

# wls2 %>%
#   filter(source == 'mhhw_wls88' & transect_site == 'T1_01') %>%
#   # group_by(transect_site) %>%
#   t.test(.$meters, mu = 0.969)

##############################################################################################
# TRENDS in water level over study period
# https://www.reed.edu/data-at-reed/resources/R/loops_with_ggplot2.html
##############################################################################################

trends.graph <- function(mo.mhhw, na.rm = TRUE, ...){
  
  # create list of logger sites in data to loop over 
  trans_list <- unique(mo.mhhw$transect)
  
  ## set parameters
  TEXT = 12 ## set font size for figures
  my.formula <- y ~ x # generic formula for use in equation

  # create for loop to produce ggplot2 graphs 
  for (i in seq_along(trans_list)) {
    
  t.mhhw <- mo.mhhw %>%
    filter(transect == trans_list[i])

plot <-  ggplot(t.mhhw) + 
  geom_point(aes(month, avg, color = site_new), size = 1) +
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
  scale_y_continuous(name = 'Monhtly MHHW (ft NAVD88)', breaks = seq(-1,5,0.5), 
                     minor_breaks = seq(-1,5,0.5),
                     limits = c(-1,5), expand = c(0,0)
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
      panel.grid.minor.y = element_line('grey', size = 0.5, linetype = "dotted"),
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

trends.graph(mo.mhhw)


