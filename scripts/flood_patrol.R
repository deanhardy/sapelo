#========================================================================
# script for identifying predicted spring tides of any NOAA tide station
# by: dean hardy
#========================================================================

rm(list=ls())

library(httr2)
library(jsonlite)
library(dplyr)
library(lubridate)
library(readnoaa) # https://cran.r-project.org/web/packages/readnoaa/readme/README.html

## define data directory
datadir <- '/Users/dhardy/Library/CloudStorage/OneDrive-UniversityofSouthCarolina/r_data/sapelo/water-level/'

# set parameters
begin_date = as.Date("2026-10-01")
end_date = as.Date("2026-12-31")

#===================================================================================================================
# import & tidy data
# call one quarter of data for year
# station: Fort Pulaski, GA (8670870)
# api info: https://api.tidesandcurrents.noaa.gov/api/prod/
#===================================================================================================================

req <- request("https://api.tidesandcurrents.noaa.gov/api/prod/datagetter") |>
  req_url_query(
    station = "8670870",
    product = "predictions",   # or "water_level", "predictions"
    interval = "hilo",
    begin_date = begin_date,
    end_date = end_date,
    datum = "MLLW",
    time_zone = "lst_ldt",
    units = "english",           # or "metric"
    format = "json"              # or "csv"
  )

resp <- req_perform(req)
raw_json <- resp_body_json(resp, simplifyVector = TRUE)

tide_df <- as_tibble(raw_json$predictions) |>
  mutate(
    t = as.POSIXct(t, format = "%Y-%m-%d %H:%M"),
    v = as.numeric(v)
  ) |>
  filter(type == "H")

#========================================
# convert data to subordinate station
# & filter to highest daily tide
# station: Old Tower Sapelo, GA (8675622)
#========================================
sub_df <- tide_df |>
  mutate(
    t = t + minutes(15),
    v = v*0.99,
    event.date = as.Date(t)
    ) |>
  group_by(event.date) |>
  slice_max(v) |>
  rename(predicted = v) |>
  select(event.date, predicted)

#========================================
# import lunar data
#========================================
lnr <- read.csv(file.path(datadir, 'lunar.csv')) %>%
  mutate(
    t = as.POSIXct(date_time_gmt, format = '%Y-%m-%d %H:%M:%S'),
    phase = ifelse(phase == 'New Moon', 'New', 'Full'),
    event.date = as.Date(t, format = '%m/%d/%y')
    ) |>
  select(event.date, phase, dist_rad, dist_name)

temp <- left_join(sub_df, lnr)
  
