rm(list=ls())

library(tidyverse)
library(lubridate)

## define data directory
datadir <- 'C:/Users/dhardy/Dropbox/r_data/sapelo'

df <- read.delim(file.path(datadir, 'water-level/site02-181012-181109.csv'), header = TRUE, skip = 1,
                 stringsAsFactors = FALSE)[-1] %>%
  rename(water_height_m = Water.Level..meters..LGR.S.N..20441313.,
         water_temp_f = Temp...F..LGR.S.N..20441313..SEN.S.N..20441313.,
         abs_pres_psi = Abs.Pres..psi..LGR.S.N..20441313..SEN.S.N..20441313.,
         date_time_gmt = Date.Time..GMT.00.00) %>%
  mutate(date_time_gmt = mdy_hm(date_time_gmt, tz='UTC')) %>%
  slice(., 1:(n()-1)) ## removes last row because erroneous reading 

fig <- ggplot(df) + 
  geom_line(aes(date_time_gmt, water_height_m * 3.28084)) + 
  geom_line(aes(date_time_gmt, water_temp_f/25), lty = "dotted", color = 'black') + 
  scale_x_datetime(name = 'Date (Year 2018)', date_breaks = '4 day', date_labels = '%m/%d') + 
  scale_y_continuous(name = 'Local Water Height (feet)',
                     sec.axis = sec_axis(~. * 25, 
                                         name = expression(paste('Temperature (',degree,'F)')))) +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(color = "black", size = 12),
        axis.ticks.length = unit(-0.2, 'cm'),
        axis.ticks = element_line(color = 'black'),
        axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), 
        axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")),
        axis.text.y.right = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")),
        axis.line = element_line(color = 'black'),
        panel.background = element_rect(fill = FALSE, color = 'black'),
        panel.grid = element_blank(),
        panel.grid.major.x = element_line('grey', size = 0.5, linetype = "dotted"),
        plot.margin = margin(0.5,0.5,0.5,0.5, 'cm'),
        legend.position = c(10/15,3),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.key = element_blank(),
        legend.box.background = element_rect(color = 'black'))  
#  labs(caption = "Temperature is dotted line.")
fig

tiff(file.path(datadir, 'figures/wl_site02.tif'), res = 300, compression = 'lzw', units = 'in', 
     height = 4, width = 7)
fig
dev.off()
