##########################################################################
## QC for each WLS field day visits to check for data alignment issues
##########################################################################
rm(list=ls())

library(tidyverse)
library(lubridate)
library(data.table)
library(readxl)
library("rio")
Sys.setenv(TZ='GMT')

## define data directory
datadir <- '/Users/dhardy/Dropbox/r_data/sapelo/water-level/'

## import field measurements data
wls.field <- read_excel('/Users/dhardy/Dropbox/Sapelo_NSF/water_level_survey/data/sapelo-water-level-survey.xlsx', 
                        sheet = 'field measurements',
                        skip = 6) %>%
  mutate(GMT = as.POSIXct(GMT, '%m/%d/%y %H:$M:%S', tz = 'GMT'),
         date = as.Date(GMT, '%m/%d/%y', tz = 'GMT'),
         Site = paste0('Site-', if_else(str_length(Site) == 1, paste0(0,Site), Site)),
         Name = str_to_title(Name),
         sitename = paste(Site, Name)) %>%
  filter(!grepl('X0976', Serial)) %>%
  select(date, GMT, Site, Name, sitename, Serial, Activity, Category)

## for use in qc.df creation
wls.field2 <- wls.field %>%
  rename(date_time_gmt = GMT, site = Site, activity = Activity) %>%
  select(site, date_time_gmt, activity)
  

## filter to just field outing dates with list of sites visited
field.smry <- wls.field %>%
  group_by(date) %>%
  summarise(sites = list(Site))

## import cleaned water level data
df <- read.csv(paste(datadir, 'wls_data.csv'))[,-1] %>%
  mutate(date_time_gmt = as.POSIXct(date_time_gmt, format = "%Y-%m-%d %H:%M:%S", tz = 'GMT'),
         date = as.Date(date))

## testing filter methods
sites_list <- unique(df$sitename)
dates_list <- unique(field.smry$date)
df.test <- df %>% filter(sitename == sites_list[16] & between(date, as.Date(dates_list[10]-1), as.Date(dates_list[10]+1)))

## https://business-science.github.io/tibbletime/articles/TT-01-time-based-filtering.html
# library(tibbletime)
# df.tbbl <- as_tbl_time(df, index = date)
# dates_list <- as_tbl_time(field.smry, index = date)
# df.test <- df.tbbl %>% filter_time(dates_list[10,]-1 ~ dates_list[10]+1)

temp <- df %>% filter(site == "Site-06" & date == '2019-05-22')



##############################################################################################
# create function for QC of field site visit days re: data alignment
# filtered to field day site visits for data downloads plus/minus one measurement
# https://www.reed.edu/data-at-reed/resources/R/loops_with_ggplot2.html
##############################################################################################

### TESTING

# sites_list <- unique(df$sitename)[17]
# gmt_list <- wls.field %>% 
#   filter(sitename == sites_list) %>%
#   pull(GMT)
# 
# dl <- filter(wls.field, sitename == sites_list & GMT == gmt_list[12])
# OUT <- filter(df, sitename == sites_list & between(df$date_time_gmt, gmt_list[z] - 720, dl$GMT + 720))
temp <- df %>% filter(site_new == 'T1-03', date =='2022-06-14')
  
qc <- NULL

# qc.df <- function(df, na.rm = TRUE, ...){
  
  # create list logger sites in data to loop over 
  sites_list <- unique(df$sitename)
  
  # create for loop to produce df 
  for (i in seq_along(sites_list)) {
    
    # create list of date and logger sites in data to loop over 
    gmt_list <- wls.field %>% 
      filter(sitename == 'Site-11 Library') %>%
      #filter(sitename == sites_list[i]) %>%
      pull(GMT)
    
    for (z in seq_along(gmt_list)) {
      
      # create QC dataframe first measurement post download minus last measurement pre-download
      # also percent difference column
      # OUT <- filter(df, sitename == sites_list[i] & between(df$date_time_gmt, gmt_list[z] - 720, gmt_list[z] + 720)) %>%
      #   select(site_new, sitename_new, date_time_gmt, water_level_C, type, logger, serial)
      OUTpre <- filter(df, sitename == sites_list[i] & between(df$date_time_gmt, gmt_list[z] - 720, gmt_list[z]-1)) %>%
        mutate(field = 'pre_temp')
      OUTpost <- filter(df, sitename == sites_list[i] & between(df$date_time_gmt, gmt_list[z]+1, gmt_list[z] + 720)) %>%
                          mutate(field = 'post_temp')
      OUT <- rbind(OUTpre, OUTpost) %>%
        select(site, site_new, sitename_new, date, date_time_gmt, water_level_C, type, logger, serial, field)
      # OUT <- df %>% 
      #   filter(sitename == sites_list[i]) %>%
      #   mutate(
      #         # visit = gmt_list[z],
      #          previsit = if_else(between(df$date_time_gmt, gmt_list[z] - 720, gmt_list[z]),
      #                             df$date_time_gmt, 'NA'),
      #          # postvisit = if_else(between(df$date_time_gmt, gmt_list[z], gmt_list[z] + 720),
      #          #                     df$date_time_gmt, 'NA')
      #          )
      qc <- rbind(OUT, qc)
      
    }
  }

qc.diff_post <- qc %>%
  group_by(site_new, date) %>%
  select(!date_time_gmt) %>%
  pivot_wider(names_from = field, values_from = water_level_C, values_fn = list) %>%
  ungroup() %>%
  hoist(pre_temp, previsit = list(1L)) %>%
  hoist(post_temp, postvisit = list(1L)) %>%
  select(!c(post_temp, pre_temp)) %>%
  # rename(postvisit = last_visit) %>%
  # mutate(previsit = as.numeric(unlist(previsit))) %>%
  ungroup() %>%
  mutate(post_pre = postvisit - previsit,
         pre_post = previsit - postvisit,
         diff_pre = round(post_pre / abs(previsit)*100, 1),
         diff_post = round(pre_post / abs(postvisit)*100, 1))

qc.diff_post.fig<- 
  ggplot(qc.diff_post) + 
  geom_point(aes(date, pre_post*100, color = cut(diff_post, c(-Inf, -10, 10, Inf)), shape = logger)) + 
  scale_y_continuous(name = 'Water Level Difference (cm)', limits = c(-10, 10), breaks = seq(-10,10,2)) + 
  scale_color_manual(name = 'Percent Difference',
                     values = c('red', 'black', 'blue'),
                     # values = c("(-100,-10]" = "red",
                     #            "(-10,10]" = "black",
                     #            "(10, 100]" = "red"),
                     labels = c("<-10%", "-10%>x<10%",  ">10%")) + 
  ggtitle('Quality Control: Pre - Post Water Level Difference') + 
  facet_wrap('site_new')
qc.diff_post.fig

tiff(paste0(datadir, "figures/qaqc/qc_pre_post.tiff"), width = 12, height = 8, units = 'in', res = 300)
qc.diff_post.fig
dev.off()

## could add assessment comparing logged measurement pre and post with field measurement to analyze different
## types of errrors in measurement. 

##############################################################################################
# create graphing function for 12-minute intervals over specified interval using water depth
# filtered to field day site visits for data downloads plus/minus one day
# https://www.reed.edu/data-at-reed/resources/R/loops_with_ggplot2.html
##############################################################################################
TEXT = 15 ## set font size for figures
qc <- NULL
qc.graph <- function(df, na.rm = TRUE, ...){
  
  # create list logger sites in data to loop over 
  sites_list <- unique(df$sitename)
  
  # create for loop to produce ggplot2 graphs 
  for (i in seq_along(sites_list)) {
    
    # create list of date and logger sites in data to loop over 
    dates_list <- wls.field %>% 
      filter(sitename == sites_list[i]) %>%
      pull(date)
    
    for (z in seq_along(dates_list)) {
      
      df2 <- filter(df, sitename == sites_list[i] & between(df$date, dates_list[z] - 1, dates_list[z] + 1))
      
      # create download datetime for vertical line
      dl <- filter(wls.field, sitename == sites_list[i] & date == dates_list[z])
      
      # create plot for each site in df 
      plot <- 
        ggplot(df2)  + 
        geom_line(aes(date_time_gmt, water_level_navd88)) +  ## convert to feet then add MLLW base elevation
        geom_vline(aes(xintercept = GMT), data = dl, lty = 'dashed') +
        scale_fill_manual(values = c('white', 'black')) + 
        scale_x_datetime(name = 'Day', date_breaks = '1 day', date_labels = '%m/%d/%y') + 
        scale_y_continuous(name = 'Water Level (m NAVD88)', 
                           breaks = seq(-0.5,1.8,0.1), limits = c(-0.5,1.8), expand = c(0,0)) +
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
        ggtitle(paste0(sites_list[i], " Field Date: ", dates_list[z]))
      
      # save plots as .png
      ggsave(plot, file=paste(datadir,
                              'figures/qaqc/', 'QC', sites_list[i], ' ',dates_list[z], ".png", sep=''), width = 6, height = 5, units = 'in', scale=2)
    } 
  }
}

# run graphing function on long df
qc.graph(df)
