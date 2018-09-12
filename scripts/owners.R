# set working directory on Windows
setwd("C:/Users/dhardy/Dropbox/sesync/data/R")

rm(list=ls())

library(tidyverse)
library(lubridate)
library(readxl)

## import property transactions & owner data
sales <- read.csv("data/sales/sales_sapelo_master.csv") %>%
  mutate(parcel_id = as.character(parcel.id), date = as.Date(date, "%m/%d/%Y")) %>%
  mutate(year = year(date), group = ifelse(price >0, "Money", "No Money")) %>%
  mutate(group = factor(group, levels = (c("Money", "No Money"))))

owner <- read_excel("data/owners/owner_info_master.xlsx", 1) %>%
  mutate(own_cat = as.character(own_cat), parcel_id = as.character(parcel_id))

## select most recent sales for each property
latest_sales <- sales %>%
  group_by(parcel_id) %>%
  slice(which.max(date))



## summarize by owners
sum <- owner %>%
  group_by(own_cat) %>%
  summarise(count = n(), acres = sum())

## plot freq of land holdings by owner category
sumplot <- ggplot(owner, aes(own_cat)) +
  geom_bar() +
  geom_text(stat='count',aes(label=..count..),vjust=-0.5) + 
  ggtitle("Sapelo Ownership") + 
  labs(x = "Owner Category", y = "Frequency")
sumplot

tiff("figures/owner_category_sums.tif", height = 5, width = 5, unit = "in", compression = "lzw", res = 300)
sumplot
dev.off()
