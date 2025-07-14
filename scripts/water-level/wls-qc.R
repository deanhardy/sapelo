##########################################################################
## QC for each WLS field day visits to check for data alignment issues
##########################################################################
rm(list=ls())

library(tidyverse)
library(geomtextpath)
library(lubridate)
library(data.table)
library(readxl)
library("rio")
Sys.setenv(TZ='GMT')

## define data directory
datadir <- '/Users/dhardy/Dropbox/r_data/sapelo/water-level/'

## import new names for sites
names.new <- read_excel('/Users/dhardy/Dropbox/Sapelo_NSF/water_level_survey/data/sapelo-water-level-survey.xlsx', 
                        sheet = 'names_new') %>%
  mutate(Site = paste0('Site-', if_else(str_length(Site) == 1, paste0(0,Site), Site)),
         sitename = paste(Site, Name),
         sitename_new = paste(Site_New, Name),
         transect = Transect,
         site_new = Site_New) %>%
  select(sitename, transect, site_new, sitename_new)

## import field measurements data
wls.field <- read_excel('/Users/dhardy/Dropbox/Sapelo_NSF/water_level_survey/data/sapelo-water-level-survey.xlsx', 
                        sheet = 'field_measurements',
                        skip = 6) %>%
  mutate(GMT = as.POSIXct(gmt, '%m/%d/%y %H:$M:%S', tz = 'GMT'),
         date = as.Date(GMT, '%m/%d/%y', tz = 'GMT'),
         Site = paste0('Site-', if_else(str_length(site) == 1, paste0(0,site), site)),
         Name = str_to_title(name),
         sitename = paste(Site, Name)) %>%
  filter(!grepl('X0976', serial)) %>%
  select(date, GMT, Site, Name, sitename, serial, activity, type)

wls.field <- left_join(wls.field, names.new, by = 'sitename')
wls.field <- wls.field %>%
  mutate(site_date = paste(site_new, date))

## for use in qc.df creation
wls.field2 <- wls.field %>%
  rename(date_time_gmt = GMT) %>%
  select(Site, date_time_gmt, activity)
  

## filter to just field outing dates with list of sites visited
field.smry <- wls.field %>%
  group_by(date) %>%
  summarise(sites = list(Site))

## import cleaned water level data
df <- read.csv(paste(datadir, 'wls_data.csv'))[,-1] %>%
  mutate(date_time_gmt = as.POSIXct(date_time_gmt, format = "%Y-%m-%d %H:%M:%S", tz = 'GMT'),
         date = as.Date(date)) %>%
  mutate(site_date = paste(site, date))

## testing filter methods
sites_list <- unique(df$site)
dates_list <- unique(field.smry$date)
df.test <- df %>% filter(site == sites_list[16] & between(date, as.Date(dates_list[10]-1), as.Date(dates_list[10]+1)))

## https://business-science.github.io/tibbletime/articles/TT-01-time-based-filtering.html
# library(tibbletime)
# df.tbbl <- as_tbl_time(df, index = date)
# dates_list <- as_tbl_time(field.smry, index = date)
# df.test <- df.tbbl %>% filter_time(dates_list[10,]-1 ~ dates_list[10]+1)

# temp <- df %>% filter(site == "Site-06" & date == '2019-05-22')



##############################################################################################
# create function for QC of field site visit days re: data alignment
# filtered to field day site visits for data downloads plus/minus one measurement
# https://www.reed.edu/data-at-reed/resources/R/loops_with_ggplot2.html
##############################################################################################

qc <- NULL

  # create list logger sites in data to loop over 
  sites_list <- unique(df$site)
  
  # create for loop to produce df 
  for (i in seq_along(sites_list)) {
    
    # create list of date and logger sites in data to loop over 
    gmt_list <- wls.field %>% 
      filter(site_new == sites_list[i]) %>%
      pull(GMT)
    
    for (z in seq_along(gmt_list)) {
      
      # create QC dataframe first measurement post download minus last measurement pre-download
      ## gmt_list +- seconds must equal relative number of burned log events from data cleaning script
      # also percent difference column
      OUTpre <- filter(df, site == sites_list[i] & between(df$date_time_gmt, gmt_list[z] - 720, gmt_list[z])) %>%
        mutate(field = 'pre_temp')
      OUTpost <- filter(df, site == sites_list[i] & between(df$date_time_gmt, gmt_list[z], gmt_list[z] + 1440)) %>%
                          mutate(field = 'post_temp')
      OUT <- rbind(OUTpre, OUTpost) %>%
        select(site, name, date, date_time_gmt, water_level_wellcap, type, logger, serial, field)
      qc <- rbind(OUT, qc)
      
    }
  }

## create dataframe showing difference in pre and post download measurements
qc.diff_post <- qc %>%
  group_by(site, date) %>%
  select(!date_time_gmt) %>%
  pivot_wider(names_from = field, values_from = water_level_wellcap, values_fn = list) %>%
  ungroup() %>%
  hoist(pre_temp, previsit = list(1L)) %>%
  hoist(post_temp, postvisit = list(1L)) %>%
  select(!c(post_temp, pre_temp)) %>%
  # rename(postvisit = last_visit) %>%
  # mutate(previsit = as.numeric(unlist(previsit))) %>%
  ungroup() %>%
  mutate(post_pre = postvisit - previsit,
         pre_post = previsit - postvisit,
         abs_diff = abs(postvisit - previsit),
         post_pre_prc = round(post_pre / abs(previsit)*100, 1),
         pre_post_prc = round(pre_post / abs(postvisit)*100, 1),
         logger_material = if_else(str_starts(serial, '2|9'), 'hobo_titanium',
                         if_else(str_starts(serial, '1'), 'hobo_poly', 've_ceramic')),
         accuracy_x2 = if_else(logger_material == 'hobo_titanium' & abs_diff > 0.003*2, 'outside',
                               if_else(logger_material == 'hobo_poly' & abs_diff > 0.01*2, 'outside',
                                       if_else(logger_material == 've_ceramic' & abs_diff > 0.005*2, 'outside', 'inside'))),
         accuracy = if_else(str_starts(serial, '2|9'), '0.003',
                              if_else(str_starts(serial, '1'), '0.010', '0.005')),)

## plot abs difference in pre and post download measurements
## why did I previously limit x-axis to 8 cm when some values are up to 90 cm?
qc.diff_post.fig<- 
  ggplot(qc.diff_post) + 
  geom_point(aes(date, abs_diff*100, shape = logger_material, color = accuracy_x2)) + 
  scale_y_continuous(name = 'Water Level Difference (cm)', limits = c(0,90), breaks = seq(0,90,10)) + 
  scale_color_manual(name = 'Accuracy (x2)',
                     values = c('black', 'red'),
                     labels = c("Within Range","Outside Range")) +
  scale_shape_manual(name = 'Logger',
                     values = c(15, 16, 17),
                     labels = c('Hobo U20L-04 (Poly)',
                                'Hobo U20-001-04 (Metal)',
                                'Van Essen Ceramic')) + 
  ggtitle(paste0('Quality Control: Absolute Difference in Water Level Pre & Post Data Download')) + 
  facet_wrap('site')
qc.diff_post.fig

png(paste0(datadir, "figures/qaqc/qc_abs_diff.png"), width = 12, height = 8, units = 'in', res = 150)
qc.diff_post.fig
dev.off()

## summary table of QC
tbl <- qc.diff_post %>%
  group_by(logger_material, accuracy_x2) %>%
  summarise(count = n())

out.range <-qc.diff_post %>%
  filter(accuracy_x2 == 'outside' 
         # | is.na(accuracy_x2)
         ) %>%
  mutate(site_date = paste(site, date))

write.csv(out.range, paste(datadir, 'qaqc_out-of-range.csv'))

## could add assessment comparing logged measurement pre and post with field measurement to analyze different
## types of errors in measurement. 

##############################################################################################
# create graphing function for 12-minute intervals over specified interval using water level relative to wellcap
# filtered to field day site visits for data downloads plus/minus 2 days and that are ID'd as out of range
# want to add horizontal line representing substrate, bottom of wellscreen, and tip of logger
# https://www.reed.edu/data-at-reed/resources/R/loops_with_ggplot2.html
##############################################################################################
TEXT = 15 ## set font size for figures
qc.graph <- function(df, na.rm = TRUE, ...){
  
  # create list logger sites in data to loop over 
  # sites_list <- unique(out.range$sitename_new)
  sites_dates <- unique((out.range$site_date))

  # create for loop to produce ggplot2 graphs 
  for (i in seq_along(sites_dates)) {
    
    # create list of date and logger sites in data to loop over 
    dates_list <- out.range %>% 
      filter(site_date == sites_dates[i]) %>%
      pull(date)
    
    
    for (z in seq_along(dates_list)) {
      
      df2 <- filter(df, str_sub(site_date, start = 1L, end = 5L) == str_sub(sites_dates[i], 1L, 5L) & between(df$date, dates_list[z] - 3, dates_list[z] + 4))
      
      df3 <- left_join(df2, out.range, by = 'site_date')
      
      # create download datetime for vertical line
      dl <- filter(wls.field, site_date == sites_dates[i] & date == dates_list[z])
      
      # create plot for each site in df 
      plot <- 
        ggplot(df3)  + 
        geom_line(aes(date_time_gmt, water_level_wellcap)) +  
        # geom_vline(aes(xintercept = GMT), data = dl, lty = 'dashed') +
        geom_textvline(aes(xintercept = GMT), label = "data download", hjust = 0.8,
                      vjust = 1.3, color = "blue4", data = dl, show.legend = F) +
        geom_texthline(aes(yintercept = 0), label = "wellcap",
                           hjust = 0.9, color = "grey70", linetype = 2, data = df3, show.legend = F) + ## wellcap
        geom_texthline(aes(yintercept = 0-well_ht_avg), label = "substrate",
                       hjust = 0.9, color = "green4", data = df3, show.legend = F) + ## substrate relative to wellcap
        geom_texthline(aes(yintercept = -0.61), label = "bottom of screen",
                           hjust = 0.9, color = "red4", data = df3, show.legend = F) + ## bottom of screen relative to wellcap
        geom_texthline(aes(yintercept =  0-lgr_length_avg), label = "tip of logger",
                           hjust = 0.9, color = "grey30", data = df3, show.legend = F) + ## tip of logger relative to wellcap
        # scale_fill_manual(values = c('white', 'black')) + 
        scale_x_datetime(name = 'Day', date_breaks = '1 day', date_labels = '%m/%d/%y') + 
        scale_y_continuous(name = 'Water Level (m Wellcap)', 
                           breaks = seq(-1,1,0.1), limits = c(-1,1), expand = c(0,0)) +
        theme(axis.title = element_text(size = TEXT),
              axis.text = element_text(color = "black", size = TEXT),
              axis.ticks.length = unit(-0.2, 'cm'),
              axis.ticks = element_line(color = 'black'),
              axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), 
              axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")),
              axis.line = element_line(color = 'black'),
              panel.background = element_rect(fill = FALSE, color = 'black'),
              panel.grid = element_blank(),
              panel.grid.major.x = element_line('grey', linewidth = 0.5, linetype = "dotted"),
              plot.margin = margin(0.5,0.5,0.5,0.5, 'cm'),
              plot.title = element_text(size = TEXT, face = "bold")) + 
        ggtitle(paste0(sites_dates[i], ', Logger Accuracy: ', df3$accuracy, 'm', ', Abs Diff: ', df3$abs_diff, 'm'))
      
      # save plots as .png
      ggsave(plot, file=paste(datadir,
                              'figures/qaqc/', 'QC-', sites_dates[i], ".png", sep=''), width = 6, height = 5, units = 'in', scale=2)
    } 
  }
}

# run graphing function on long df
qc.graph(df)
