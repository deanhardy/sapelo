###############################################################################################
## PURPOSE: generate gif animation of relational maps
## BY: Dean Hardy
###############################################################################################
rm(list=ls())

library(tidyverse)
library(magrittr)
library(magick)
library(gifski)

## define data directory
datadir <- '/Users/dhardy/Dropbox/r_data/sapelo/'

png_files <- list.files(paste0(datadir, 'figures/relational/maps'), pattern = ".*png$", full.names = TRUE)

gifski(png_files, gif_file = paste0(datadir, "figures/relational/maps/animation.gif"), width = 1800, height = 1200, delay = 1)
