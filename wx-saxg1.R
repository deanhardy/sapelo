rm(list=ls())

library(tidyverse)
library(lubridate)
Sys.setenv(TZ='GMT')

## define data directory
datadir <- '/Users/dhardy/Dropbox/r_data/sapelo'

## import NERR weather data
wx <- read.csv()
