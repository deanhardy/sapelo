####
## revise van essen time from EST to GMT
####

rm(list=ls())

library(tidyverse)
library(lubridate)
library(data.table)
library("rio")
Sys.setenv(TZ='GMT')

## define data directory
datadir <- '/Users/dhardy/Dropbox/r_data/sapelo/water-level/'

filz.ve.est <- list.files(path = file.path(datadir, 'new-logger-data/vanessen_est'),
                          pattern= '*.CSV',
                          full.names = TRUE,
                          recursive = TRUE) 