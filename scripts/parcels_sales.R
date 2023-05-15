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

#### descendant and outsiders losses and gains
## descendant losses
loss <- sales %>% 
  filter(grantor_category %in% c('descendant', 'l_descendant') & grantee_category %in% c('outsider', 'l_outsider'))
sum(loss$price)

## filter by unique parcel IDs and with financial exchange
l.yr <- loss %>% 
  distinct(parcel.id, .keep_all = TRUE) %>%
  filter(price > 0) 

## calculate total land losses annually as well as cumulatively
sum.yr <- l.yr %>%
  group_by(year) %>%
  summarize(acres = sum(acres), count = n()) %>%
  mutate(cumulative = CUMULATIVE_SUM(acres))
sum(sum.yr$acres)

## filter to land lost since 2001 (last two decades)
d2.loss <- sum.yr %>% filter(year >= 2001)
sum(d2.loss$acres)

## filter to land lost during tax hike
t.hike <- sum.yr %>% filter(year %in% c(2013, 2014, 2015))
sum(t.hike$acres)

## percent loss during t.hike
sum(t.hike$acres) / sum(d2.loss$acres) *100

## plot number sales per year with financial exchange
ggplot(sum.yr, aes(year, count)) +
  geom_col()

## plot acreage losses by year, cumulative, with tax hike period highlighted
fnt <- 12
dl <- ggplot(sum.yr) +
  geom_col(aes(year, acres, fill = 'Annual')) +
  geom_line(aes(year, cumulative, color = 'Cumulative')) + 
  scale_y_continuous(name = 'Descendant Land Losses (Acres)',
                   breaks = seq(0,25, 5),
                   limits = c(0,25),
                   expand = c(0,0)) + 
  scale_x_continuous(name = "Year",
                     breaks = seq(1999, 2022, 1)) +
  # ggtitle("Descendant Land Losses") + 
  theme(
    panel.background = element_rect(fill = FALSE, color = 'black'),
    # panel.grid = element_blank(),
    panel.grid.major.y = element_line(linetype = 'dashed', color = 'grey'),
    plot.title = element_text(size =fnt),
    axis.title = element_text(size = fnt),
    axis.text = element_text(size = fnt),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
    legend.position = 'bottom',
    legend.text = element_text(size = fnt),
    legend.title = element_text(size = fnt),
    # legend.key = element_blank(),
    legend.box.margin = margin(0.005,0.005,0.005,0.005, 'cm'),
    legend.box.background = element_rect(color = 'black')
  ) + 
  annotate("rect", xmin = 2012.5, xmax = 2015.5, ymin = 0, ymax = 25,
           alpha = .1,fill = "blue") +
  geom_text(aes(label = 'TAX HIKE PERIOD >>>', x = 2008, y = 17.5),
            size = 3, hjust = 0) +
  guides(color = guide_legend(title="Loss Category"),
         fill = guide_legend(title = element_blank()))
dl

tiff(file.path(datadir, "figures/descendant-landloss.tif"), units = "in", 
     height = 5, width = 7, res = 300, compression = "lzw")
dl
dev.off()

png(file.path(datadir, "figures/descendant-landloss.png"), units = "in", 
    height = 7.5, width = 13.33, res = 150)
dl
dev.off()

## plots acreage loss trends by individual parcels
ind <- ggplot(filter(loss, price > 0), aes(year, acres)) +
  geom_point(size = 1) + 
  geom_smooth(method = 'lm', se = T) + 
  # geom_smooth(method = 'lm', se = T) + 
  scale_y_continuous(name = 'Acres',
                     breaks = seq(0,2, 0.5),
                     limits = c(0,2),
                     expand = c(0,0)) + 
  scale_x_continuous(name = "Year",
                     breaks = seq(2000, 2020, 5)) +
  # ggtitle("Estimated Land Losses") + 
  theme(
    panel.background = element_rect(fill = FALSE, color = 'black'),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(linetype = 'dashed')
  )
ind

tiff(file.path(datadir, "figures/descendant-landloss-individual.tif"), units = "in", height = 3, width = 5, res = 600, compression = "lzw")
ind
dev.off()

png(file.path(datadir, "figures/descendant-landloss-individual.png"), units = "in", height = 3, width = 5, res = 150)
ind
dev.off()

## acres sold/lost per year
sum(l.yr$acres) / (last(l.yr$year) - first(l.yr$year))

gain <- sales %>% 
  filter(grantee_category %in% c('descendant', 'l_descendant') & grantor_category %in% c('outsider', 'l_outsider'))
sum(gain$price)
gain %>% distinct(parcel.id, .keep_all = TRUE) %>% summarize(sum(acres))

## select most recent sales for each property
latest_sales <- sales %>%
  group_by(parcel.id) %>%
  slice(which.max(date))

sales2012 <- sales %>%
  filter(date >= as.Date("2012-01-01") & price > 0)

##################################
## financial transactions analysis
##################################
money <- filter(sales, price > 0)
mean(money$price.acre)

m.cost <- sales %>% 
  filter(price > 0) %>%
  group_by(year, sale.type) %>%
  summarise(count = n(),
            mean = mean(price.acre),
            median = median(price.acre))

fnt = 20

annual.cost <- ggplot(filter(sales, year >= 1990 & price >0), aes(year, price/100000, color = sale.type)) +
  geom_point(aes(size = acres)) + 
  geom_smooth(method = 'lm', se = F) + 
  # stat_smooth(geom='lm', aes(ymin = ifelse(..ymin.. < 0, 0, ..ymin..)), 
  #             alpha = .3) +
  scale_y_continuous(name = 'Sale Price (x$100,000)',
                     breaks = seq(0,10, 1),
                     limits = c(0,10),
                     expand = c(0,0)) + 
  scale_x_continuous(name = "Year",
                     breaks = seq(1990, 2020, 5),
                     ) + 
  scale_color_manual(name = "Category", values = c('black', 'red')) + 
  ggtitle('Property Sales Trends') + 
  theme(axis.title = element_text(size = fnt),
        axis.title.x = element_text(margin = margin(t=0,r=0,b=0,l=0)),
        axis.title.y = element_text(margin = margin(t=0,r=0,b=0,l=0)),
        axis.text = element_text(color = "black", size = fnt),
        axis.ticks.length = unit(-0.1, 'cm'),
        axis.ticks = element_line(color = 'black'),
        axis.text.x = element_text(margin=unit(c(0.2,0.2,0.2,0.2), "cm")), 
        axis.text.y = element_text(margin=unit(c(0.2,0.2,0.2,0.2), "cm")),
        axis.line = element_line(color = 'black'),
        panel.background = element_rect(fill = FALSE, color = 'black'),
        panel.grid = element_blank(),
        # panel.grid.major.x = element_line('grey', size = 0.5, linetype = "dotted"),
        # panel.grid.major.y = element_line('grey', size = 0.5, linetype = "dotted"),
        plot.margin = margin(0.25,0.25,0.5,0.5, 'cm'),
        panel.grid.major.y = element_line(linetype = 'dashed'),
        plot.title = element_text(size = 20),
        legend.position = c(0.15,0.7),
        legend.text = element_text(size = fnt),
        legend.title = element_text(size = fnt),
        legend.key = element_blank(),
        legend.box.margin = margin(0.005,0.005,0.005,0.005, 'cm'),
        legend.box.background = element_rect(color = 'black')) 
annual.cost

tiff(file.path(datadir, "figures/sales_priceperacre-cwbp-rb02.tif"), units = "in", height = 3, width = 5, res = 600, compression = "lzw")
annual.cost
dev.off()

png(file.path(datadir, "figures/sales_priceperacre.png"), units = "in", height = 7, width = 13, res = 150)
annual.cost
dev.off()

## export sales data as table in PDF
# pdf("sales_table.pdf", height=11, width=8.5)
# grid.table(sales)
# dev.off()

# size_legend <- guides(color = 'black', fill = 'none')
## sales prices per acre
saleplot <- ggplot(filter(sales, reason != "MT", price != 0),  
                   aes(date, price.acre/100000, color = sale.type)) + 
  geom_smooth(aes(date, price.acre/100000, color = sale.type, fill = sale.type),
              method = "loess", se = TRUE,
              linetype = 'dashed', lwd = 0.5, show.legend = F) +
  geom_point(aes(date, price.acre/100000, size = acres), alpha = 0.5, show.legend = F) +
  scale_x_date(name = "Year", date_breaks = "2 year", date_labels = "%y",
               date_minor_breaks = "1 year", expand = c(0,0)) +
  scale_y_continuous(name = "Sale price per acre (x $100,000)",
                     breaks = seq(0,16, 1),
                     limits = c(-16,16), expand = c(0.05,0),
                     sec.axis = sec_axis(~., breaks = seq(0,16,1), labels = NULL)) +
  coord_x_date(ylim = c(0,16), xlim = c('1992-01-01', '2022-01-01')) +
  scale_color_manual(name = "Sale Type", values = c('darkgreen', 'grey75'),
                     labels = c('Land Only', 'Land with Building')) +
  scale_fill_manual(name = "Sale Type", values = c('darkgreen', 'grey75'),
                    labels = c('Land Only', 'Land with Building')) +
  scale_size_continuous(name = 'Parcel Size (Ac)', range = c(1, 4)) +
  # ggtitle('B)') + 
  theme(axis.title = element_text(size = fnt),
        axis.title.x = element_text(margin = margin(t=0,r=0,b=0,l=0)),
        axis.title.y = element_text(margin = margin(t=0,r=0,b=0,l=0)),
        axis.text = element_text(color = "black", size = fnt),
        axis.ticks.length = unit(-0.1, 'cm'),
        axis.ticks = element_line(color = 'black'),
        axis.text.x = element_text(margin=unit(c(0.2,0.2,0.2,0.2), "cm")), 
        axis.text.y = element_text(margin=unit(c(0.2,0.2,0.2,0.2), "cm")),
        axis.line = element_line(color = 'black'),
        panel.background = element_rect(fill = FALSE, color = 'black'),
        panel.grid = element_blank(),
        # panel.grid.major.x = element_line('grey', size = 0.5, linetype = "dotted"),
        # panel.grid.major.y = element_line('grey', size = 0.5, linetype = "dotted"),
        plot.margin = margin(0.25,0.25,0.5,0.5, 'cm'),
        legend.position = c(0.15,0.75),
        legend.text = element_text(size = fnt),
        legend.title = element_text(size = fnt),
        legend.key = element_blank(),
        legend.spacing.y = unit(-0.05, 'cm'),
        legend.box.margin = margin(0.005,0.005,0.005,0.005, 'cm'),
        legend.box.background = element_rect(color = 'black')) 
  # guides(color = guide_legend(byrow = TRUE),
  #        fill = guide_legend(byrow = TRUE))
saleplot

tiff(file.path(datadir, "figures/sales_priceperacre.tif"), units = "in", height = 3, width = 5, res = 600, compression = "lzw")
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
