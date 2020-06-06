rm(list=ls())

library(sf)
library(tidycensus)
library(tidyverse)
library(readxl)
library(stringr)

#define data directory
datadir <- file.path('/Users/dhardy/Dropbox/r_data/sapelo')

cl <- st_read(file.path(datadir, 'cl19'))

hp <- read_excel(file.path(datadir, 'heritage-preserves/heritage-preserves.xlsx'), sheet = 'preserves')
