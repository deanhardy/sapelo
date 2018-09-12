# set working directory on Windows
setwd("C:/Users/dhardy/Dropbox/sesync/data/R")

rm(list=ls())

library(tidyverse)
library(lubridate)

## import data from qpublic diwnloaded on 12/20/2017; includes sales through 8/20/2017
qpub <- read.csv("data/sales/171220_sales_qpub.csv") %>%
  rename(parcel.id = Parcel.ID, price = Sale.Price, sale.type = Sale..Type, date = Sale.Date,
         qualified = Qualified.Sales, reason = Reason, acres = Acres, prop.type = Property.Type, assessed.value= Assessed..Value.,
         legaldesc = LegalDesc, year.built = YearBuilt, address = Address) %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y"),
         price.acre = price/acres, parcel.id = as.character(parcel.id)) %>%
  select(-Alternate.ID, -FinishedSqft, -PricePerFinishedSqft, -(Neighborhood:Ref.Number)) %>%
  select(date, everything()) %>%
  arrange(date)

## import who to whom sales data
## before 1/1/2015 based on McIntish County Tax Assessor Office GIS data
## after 12/31/2014 based on data manually entered from qpublic
Y <- read.csv("data/sales/sales_whom.csv") %>%
  # rename(realkey = REALKEY, grantee = GRANTEE, grantor = GRANTOR, date = SALEDATE, deedpage = DEEDPAGE, 
  #        plotpage = PLOTPAGE, saleclass = SALECLASS, reason = REASON, parcel.id = PARCEL_NO) %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y"), parcel.id = as.character(parcel.id)) %>%
  select(-reason) %>%
  arrange(date)

## filter qpublic data to match same time range as whom data (ie <2015)
X <- qpub %>%
  filter(date < as.Date("2015-01-01"))

## create df with unique site:date combinations
SD <- select(X, parcel.id, date)
SD <- distinct(SD)

## create two input and one output df for use in for loop
xx <- filter(X, parcel.id==SD[1,1], date==SD[1,2])
yy <- filter(Y, parcel.id==SD[1,1], date==SD[1,2])
zz <- cbind(xx, yy)

## attach whom to qpub data iteratively such that duplicates in either are not overduplicated
## this deals with multiple matches without creating joins for all possible match combinations
## thanks to Jessica for this one
for(i in 2:nrow(SD)){
  xx <- filter(X, parcel.id==SD[i,1], date==SD[i,2])
  yy <- filter(Y, parcel.id==SD[i,1], date==SD[i,2])
  if(nrow(yy) < nrow(xx)){
    q <- matrix(ncol=ncol(yy), nrow=(nrow(xx)-nrow(yy)))
    q <- as.data.frame(q)
    colnames(q) <- colnames(yy)
    yy <- rbind(yy, q)
  } else{}
  outt <- cbind(xx, yy)
  zz <- rbind(zz, outt)
}

Y15 <- qpub %>%
  filter(date >= as.Date("2015-01-01")) %>%
  mutate(parcel.id = as.character(parcel.id), grantee = "NA", grantor = "NA")

zz2 <- zz %>%
  subset(select = -c(14:15)) %>%
  bind_rows(Y15)

## export data for later updating with new sales and grantor/grantee info
write.csv(zz2, "data/sales/sales_sapelo_original.csv", row.names = FALSE)



