rm(list=ls())

library(tidyverse)
library(lubridate)
library(gridExtra)
library(tidyquant)

## define data directory
datadir <- '/Users/dhardy/Dropbox/r_data/sapelo'

## import data
sales <- read.csv(file.path(datadir, "property/transactions_sapelo_primary.csv"), stringsAsFactors = F) %>%
  mutate(date = as.Date(date, "%m/%d/%y")) %>%
  mutate(year = year(date), group = ifelse(price >0, "Money", "No Money")) %>%
  mutate(group = factor(group, levels = (c("No Money", "Money")))) %>%
  mutate(price.ha = price/(acres * 0.404686)) %>%
  filter(acres != 'na')

## select most recent sales for each property
latest_sales <- sales %>%
  group_by(parcel.id) %>%
  slice(which.max(date))

sales2012 <- sales %>%
  filter(date >= as.Date("2012-01-01") & price > 0)
  
  
## export sales data as table in PDF
# pdf("sales_table.pdf", height=11, width=8.5)
# grid.table(sales)
# dev.off()

fnt = 8
# size_legend <- guides(color = 'black', fill = 'none')
## sales prices per acre
saleplot <- ggplot(filter(sales, reason != "MT", price != 0),  
                   aes(date, price.acre/100000, color = sale.type)) + 
  geom_smooth(aes(date, price.acre/100000, color = sale.type, fill = sale.type),
              method = "loess", se = TRUE,
              linetype = 'dashed', lwd = 0.5, show.legend = F) +
  geom_point(aes(date, price.acre/100000, size = acres), alpha = 0.5) +
  scale_x_date(name = "Year", date_breaks = "5 year", date_labels = "%Y",
               date_minor_breaks = "1 year", expand = c(0,0)) +
  scale_y_continuous(name = "Sale price per acre (x $100,000)",
                     breaks = seq(0,20, 5),
                     limits = c(-20,20), expand = c(0.05,0),
                     sec.axis = sec_axis(~., breaks = seq(0,20,5), labels = NULL)) +
  coord_x_date(ylim = c(0,20), xlim = c('1990-01-01', '2020-01-01')) +
  scale_color_manual(name = "Sale Type", values = c('darkgreen', 'grey75'),
                     labels = c('Land Only', 'Land with Building')) +
  scale_fill_manual(name = "Sale Type", values = c('darkgreen', 'grey75'),
                    labels = c('Land Only', 'Land with Building')) +
  scale_size_continuous(name = 'Parcel Size (Ac)', range = c(1, 4)) +
  ggtitle('B)') + 
  theme(axis.title = element_text(size = fnt),
        axis.title.x = element_text(margin = margin(t=-10,r=0,b=0,l=0)),
        axis.title.y = element_text(margin = margin(t=0,r=-10,b=0,l=0)),
        axis.text = element_text(color = "black", size = fnt),
        axis.ticks.length = unit(-0.2, 'cm'),
        axis.ticks = element_line(color = 'black'),
        axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), 
        axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")),
        axis.line = element_line(color = 'black'),
        panel.background = element_rect(fill = FALSE, color = 'black'),
        panel.grid = element_blank(),
        panel.grid.major.x = element_line('grey', size = 0.5, linetype = "dotted"),
        plot.margin = margin(1,1,0.5,0.5, 'cm'),
        legend.position = c(0.28,0.7),
        legend.text = element_text(size = fnt),
        legend.title = element_text(size = fnt),
        legend.key = element_blank(),
        legend.spacing.y = unit(0.005, 'cm'),
        legend.box.margin = margin(0.05,0.05,0.05,0.05, 'cm'),
        legend.box.background = element_rect(color = 'black'))
saleplot

tiff(file.path(datadir, "figures/sales_priceperha.tif"), units = "in", height = 5, width = 5, res = 300, compression = "lzw")
saleplot
dev.off()

## prep data for plotting total count of sales by year
sales2 <- sales %>%
  group_by(group, year) %>%
  summarise(freq = n())

sales3 <- sales2 %>% filter(group == "Money" & year >= 1991)
sum(sales3$freq)

## plot sales frequency totals and fit curve 
sale_rate <- ggplot(filter(sales2, year > 1990)) +
  # geom_smooth(aes(year, freq, fill = group, color = group), level = 0.95, fullrange = FALSE, 
  #             method = "lm", se = TRUE, linetype = "dashed", lwd =0.5, show.legend = F) +
  geom_col(aes(year, freq, fill = group), width = 0.5) + 
#           position = position_dodge2(width = 0.3, padding = 0)) +
  scale_x_continuous(name = "Year", limits = c(1990,2020), 
                     breaks = seq(1990,2020,5), expand = c(0,0)) +
                     # sec.axis = sec_axis(~., labels = NULL, 
                     #                     breaks = seq(1990,2020,5))) +
  scale_y_continuous(name = "Number of Transactions", breaks = seq(0,45, 5), 
                     limits = c(-10,45), expand = c(0,0),
                     sec.axis = sec_axis(~., labels = NULL, 
                                         breaks = seq(0,45, 5))) + 
  coord_cartesian(ylim = c(0,45)) + 
  scale_color_manual(name = "Type", values = c("grey65", "black"), 
                    labels = c("No Money", "Money")) +
  scale_fill_manual(name = "Type", values = c("grey65", "black"), 
                     labels = c("No Money", "Money")) +
  ggtitle('A)') +
  theme(axis.title = element_text(size = fnt),
        axis.title.x = element_text(margin = margin(t=-10,r=0,b=0,l=0)),
        axis.title.y = element_text(margin = margin(t=0,r=-10,b=0,l=0)),
        axis.text = element_text(color = "black",
                                   size = fnt),
        axis.ticks.length = unit(-0.2, 'cm'),
        axis.ticks = element_line(color = 'black'),
        axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), 
        axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")),
        axis.line = element_line(color = 'black'),
        legend.position = c(0.21,0.85),
        legend.text = element_text(size = fnt),
        legend.title = element_text(size = fnt),
        panel.grid.major.x = element_line('grey', size = 0.5, linetype = "dotted"),
        # legend.background = element_rect(color = 'grey70'),
        # panel.grid.major.y = element_line(linetype = 'solid',
        #                                 color = 'grey80'),
        panel.grid = element_blank(), 
        panel.background = element_rect(fill = FALSE, color = 'black'),
        plot.margin = margin(1,1,0.5,0.5, 'cm'),
        legend.box.margin = margin(0.05,0.05,0.05,0.05, 'cm'),
        legend.box.background = element_rect(color = 'black'))
sale_rate

tiff(file.path(datadir, 'figures/sales_rate.tiff'), units = "in", height = 5, width = 5, res = 300,
     compression = "lzw")
sale_rate
dev.off()

## combine sales rate and sales amounts into one graphic
tiff(file.path(datadir, 'figures/sales_rate_amounts.tif'), units = "in", height = 5, width = 8, res = 300,
     compression = "lzw")
grid.arrange(sale_rate, saleplot, ncol = 2)
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

sum(annual.freq$annual_trans)

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


## filter transactions by specific group
who <- 'PFS GROUP LLC'

test <- sales %>%
  filter(grantor %in% who | grantee %in% who)
