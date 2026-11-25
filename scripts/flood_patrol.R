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
end_date = as.Date("2027-09-30")

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
    tide_lt = as.POSIXct(t, format = "%Y-%m-%d %H:%M"),
    height = as.numeric(v)
  ) |>
  filter(type == "H") |>
  select(tide_lt, height)

#========================================
# convert data to subordinate station
# & filter to highest daily tide
# station: Old Tower Sapelo, GA (8675622)
#========================================
sub_df <- tide_df |>
  mutate(
    tide_lt = tide_lt + minutes(15),
    height = height*0.99,
    event_date = as.Date(tide_lt, tz = Sys.timezone())
    ) |>
  group_by(event_date) |>
  slice_max(height) |>
  rename(predicted = height) |>
  select(event_date, tide_lt, predicted)

#========================================
# import lunar data & join with tidal
#========================================
lnr <- read.csv(file.path(datadir, 'lunar.csv')) %>%
  mutate(
    lnr_ut = as.POSIXct(date_time_gmt, format = '%Y-%m-%d %H:%M:%S', tz = "UTC"),
    lnr_lt = with_tz(lnr_ut, tzone = Sys.timezone()),
    # phase = ifelse(phase == 'New Moon', 'New', 'Full'),
    event_date = as.Date(lnr_lt, format = '%m/%d/%y', Sys.timezone())
    ) |>
  select(event_date, lnr_lt, phase, dist_rad, dist_name)

ht_daily <- left_join(sub_df, lnr) %>%
  ungroup()

#====================================================
# filter tidal data to spring tides using lag method
#====================================================
library(slider)
library(tidyr)

spring_tides <- ht_daily %>%
  # 1. Find the max tide within 3 rows before and 3 rows after each row
  mutate(
    spring_tide = slide(
      .x = pick(tide_lt, predicted),
      .f = ~ slice_max(.x, order_by = predicted, n = 1, with_ties = FALSE),
      .before = 3,
      .after = 3
    )
  ) %>%
  # 2. Keep only the rows that actually have a moon phase recorded
  filter(!is.na(phase) & phase %in% c('Full Moon', 'New Moon')) %>%
  # 3. Unpack the nested peak_event data frame into normal columns
  unnest(spring_tide, names_sep = "_") %>%
  # 4. Clean up dataframe for export
  select(spring_tide_tide_lt, spring_tide_predicted, phase, lnr_lt, dist_name, dist_rad)

write.csv(spring_tides, paste0(datadir, 'flood_patrol/', Sys.Date(), '_spring_tides_predicted.csv'))
