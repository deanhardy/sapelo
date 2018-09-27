rm(list=ls())

library(tidyverse)
library(lubridate)
library(gridExtra)
library(tidyquant)

## define data directory
datadir <- 'C:/Users/dhardy/Dropbox/r_data/sapelo'

## import data
sales <- read.csv(file.path(datadir, "property/transactions_sapelo_master.csv"), stringsAsFactors = F) %>%
  mutate(date = as.Date(date, "%m/%d/%Y")) %>%
  mutate(year = year(date), group = ifelse(price >0, "Money", "No Money")) %>%
  mutate(group = factor(group, levels = (c("Money", "No Money"))))

## select most recent sales for each property
latest_sales <- sales %>%
  group_by(parcel.id) %>%
  slice(which.max(date))

## export sales data as table in PDF
# pdf("sales_table.pdf", height=11, width=8.5)
# grid.table(sales)
# dev.off()

size_legend <- guides(color = 'black', fill = 'none')
## sales prices per acre
saleplot <- ggplot(filter(sales, reason != "MT", price != 0),  
                   aes(date, price.acre/100000, color = sale.type)) + 
  geom_smooth(aes(date, price.acre/100000, color = sale.type, fill = sale.type),
              method = "loess", se = TRUE,
              linetype = 'dashed', lwd = 0.5, show.legend = TRUE) +
  geom_point(aes(date, price.acre/100000, size = acres)) +
  scale_x_date(name = "Year", date_breaks = "5 year", date_labels = "%Y",
               date_minor_breaks = "1 year", expand = c(0,0)) +
  scale_y_continuous(name = "Sale price per acre (x $100,000)",
                     breaks = seq(0,16, 2),
                     limits = c(-3,16), expand = c(0.05,0),
                     sec.axis = sec_axis(~., breaks = seq(0,16,2), labels = NULL)) +
  coord_x_date(ylim = c(0,16), xlim = c('1990-01-01', '2020-01-01')) +
  scale_color_manual(name = "Sale Type", values = c('black', 'grey55'),
                     labels = c('Land Only', 'Land with Building')) +
  scale_fill_manual(name = "Sale Type", values = c('black', 'grey55'),
                    labels = c('Land Only', 'Land with Building')) +
  scale_size_continuous(name = 'Parcel Size (Acres)') +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(color = "black", size = 10),
        axis.ticks.length = unit(-0.2, 'cm'),
        axis.ticks = element_line(color = 'black'),
        axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), 
        axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")),
        axis.line = element_line(color = 'black'),
        panel.background = element_rect(fill = FALSE, color = 'black'),
        panel.grid = element_blank(),
        panel.grid.major.x = element_line('grey', size = 0.5, linetype = "dotted"),
        plot.margin = margin(1,1,0.5,0.5, 'cm'),
        legend.position = c(0.25,0.65),
        legend.text = element_text(size = 10),
        legend.key = element_blank(),
        legend.box.background = element_rect(color = 'black'))
saleplot

tiff(file.path(datadir, "figures/sales_priceperacre.tif"), units = "in", height = 5, width = 5, res = 300, compression = "lzw")
saleplot
dev.off()

## prep data for plotting total count of sales by year
sales2 <- sales %>%
  group_by(group, year) %>%
  summarise(freq = n())

## plot sales frequency totals and fit curve 
sale_rate <- ggplot(filter(sales2, year > 1990)) +
  geom_smooth(aes(year, freq, fill = group, color = group), level = 0.95, fullrange = FALSE, 
              method = "lm", se = TRUE, linetype = "dashed", lwd =0.5) +
  geom_point(aes(year, freq, color = group)) +
  scale_x_continuous(name = "Year", limits = c(1990,2020), 
                     breaks = seq(1990,2020,5), expand = c(0,0),
                     sec.axis = sec_axis(~., labels = NULL, 
                                         breaks = seq(1990,2020,5))) +
  scale_y_continuous(name = "Number of Transactions", breaks = seq(0,45, 5), 
                     limits = c(-10,45), expand = c(0,0),
                     sec.axis = sec_axis(~., labels = NULL, 
                                         breaks = seq(0,45, 5))) + 
  coord_cartesian(ylim = c(0,45)) + 
  scale_color_manual(name = "Type", values = c("chartreuse4", "grey20"), 
                    labels = c("Money", "No Money")) +
  scale_fill_manual(name = "Type", values = c("chartreuse4", "grey20"), 
                     labels = c("Money", "No Money")) +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(color = "black",
                                   size = 10),
        axis.ticks.length = unit(-0.2, 'cm'),
        axis.ticks = element_line(color = 'black'),
        axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), 
        axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")),
        axis.line = element_line(color = 'black'),
        legend.position = c(0.18,0.85),
        legend.text = element_text(size = 10),
        # legend.title = element_text(size = 10),
        # legend.background = element_rect(color = 'grey70'),
        # panel.grid.major.y = element_line(linetype = 'solid',
        #                                 color = 'grey80'),
        panel.grid = element_blank(), 
        panel.background = element_blank(),
        plot.margin = margin(1,1,0.5,0.5, 'cm'),
        legend.box.background = element_rect(color = 'black'))
sale_rate

tiff(file.path(datadir, 'figures/sales_rate.tiff'), units = "in", height = 5, width = 5, res = 300,
     compression = "lzw")
sale_rate
dev.off()


## transaction freq by year
annual.trans <- sales %>%
  mutate(years = year(date)) %>%
  group_by(years) %>%
  summarise(annual_trans = n())
## unique parcels by year
annual.uniqpar <- sales %>%
  distinct(parcel.id, .keep_all = TRUE) %>%
  mutate(years = year(date)) %>%
  group_by(years) %>%
  summarise(annual_uniqpar = n())
## combine
annual.freq <- merge(annual.uniqpar, annual.trans)

## transaction freq by decade
decadal.trans <- sales %>%
  mutate(decade = floor_date(date, years(10))) %>%
  mutate(decade = year(decade)) %>%
  group_by(decade) %>%
  summarise(decadal_trans = n())
## unique parcels by decade
decadal.uniqpar <- sales %>%
  distinct(parcel.id, .keep_all = TRUE) %>%
  mutate(decade = floor_date(date, years(10))) %>%
  mutate(decade = year(decade)) %>%
  group_by(decade) %>%
  summarise(decadal_uniqpar = n())
## combine
decadal.freq <- merge(decadal.uniqpar, decadal.trans)
