################################################################################
## By: Dean Hardy
## script to download and graph mean monthly sea level from NOAA tide gage stations
## depends on rnoaa & tidyverse packages
################################################################################
rm(list=ls())

library(rnoaa) ## package info https://cran.r-project.org/web/packages/rnoaa/rnoaa.pdf
library(tidyverse)

## define data directory
datadir <- '/Users/dhardy/Dropbox/r_data/sapelo'

## import average seasonal cycle data
# test <- read.csv("https://tidesandcurrents.noaa.gov/sltrends/data/USAverageSeasonalCycleData.csv",
#                  stringsAsFactors = FALSE)

## define variables
STATION <- c(8670870, 8720030) ## define stations
DATUM <- 'MSL' ## define datum
T <- c(0,1,2,3) ## define number of decades of data to grab where 0 = 1 decade, 1 = 2 decades, etc
df <- NULL ## empty dataframe

## for loop to grab data in decadal increments for multiple stations
for (z in 1:length(STATION)) {
  
for (i in 1:length(T)) {
DATE <- Sys.Date()-(3653*T)

OUT <-
  coops_search(
    begin_date = as.character(as.Date(DATE[[i]]-3653), format = '%Y%m%d') %>% 
      gsub('-', '', .) %>%
      as.numeric(),
    end_date = as.character(as.Date(DATE[[i]]), format = '%Y%m%d') %>%
      gsub('-', '', .) %>%
      as.numeric(),
    station_name = STATION[[z]],
    product = 'monthly_mean', 
    datum = DATUM, 
    units = 'metric', 
    time_zone = 'GMT')$data 
  
  OUT2 <- OUT %>%
    mutate(station = as.character(STATION[[z]])) %>%
    mutate(yrmo = paste(year, month, sep = '-')) %>%
    mutate(date = as.Date(paste(yrmo, '-01', sep = '')))

  df <- rbind(df, OUT2)
}
}

## plot mean monthly sea levels for all stations and draw regression line for each station
fig <- ggplot(df, aes(x = date, y = MSL, color = station)) +
  geom_smooth(method = 'lm') + 
  geom_point(size = 1) + 
  scale_y_continuous(name = paste('Datum', DATUM, '(m)'),
                     breaks = round(seq(-0.2, 0.4, by = 0.1), 2),
                     minor_breaks = seq(-0.2, 0.4, by = 0.05)) + 
  scale_x_date(name = 'Date', 
               date_breaks = '12 months', 
               date_minor_breaks = '6 month',
               date_labels = '%b-%y') + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 16),
        legend.position = 'right') + 
  ggtitle(paste("Monthly Mean Sea Level")) 
#   facet_wrap(~station)
fig

# save plots as .png
ggsave(fig, file=paste(datadir,
                        '/figures/sea-level', ".png", sep=''), width = 6, height = 4, units = 'in', scale=2)


## working on adding equation with slope
library(ggpmisc)
# GET EQUATION AND R-SQUARED AS STRING
# SOURCE: https://groups.google.com/forum/#!topic/ggplot2/1TgH-kG5XMA
## from https://stackoverflow.com/questions/7549694/add-regression-line-equation-and-r2-on-graph
# lm_eqn <- function(dat2){
#   m <- lm(MSL ~ date, dat2);
#   eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
#                    list(a = format(unname(coef(m)[1]), digits = 3),
#                         b = format(unname(coef(m)[2]), digits = 2),
#                         r2 = format(summary(m)$r.squared, digits = 3)))
#   as.character(as.expression(eq));
# }

## set parameters
my.formula <- y ~ x # generic formula for use in equation
D <- c(1,2,3)

for (i in D) {

T <- 3653*i ## set time period
T_name <- ifelse(T == 3653, "Decade", 
                 ifelse(T == 3653*2, 'Two Decades', 'Three Decades'))
## filter all data to one station and selected time period
dat2 <- filter(df, station == '8670870' & date >= Sys.Date()-T) %>%
  mutate(MSL = MSL*100) %>%
  arrange(date)
m <- lm(MSL ~ date, dat2) ## create regression line

## 
## mimic style in NOAA graph here: https://tidesandcurrents.noaa.gov/sltrends/sltrends_station.shtml?id=8670870
fig2 <- ggplot(dat2, aes(x = date, y = MSL)) +
  geom_hline(yintercept = 0, linetype = 1.5, lwd = 0.5) +
  geom_line(color = 'blue', lwd = 0.2) + 
  geom_smooth(method = 'lm', color = 'black', formula = my.formula) +
  ## next three lines add linear trend lines for each decade
  geom_smooth(data = filter(dat2, date >= last(date)-3653), method = 'lm', color = 'purple', formula = my.formula, se = FALSE) +
  geom_smooth(data = filter(dat2, date >= last(date)-3653*2 & date <= last(date)-3653), method = 'lm', color = 'coral', formula = my.formula, se = FALSE) +
  geom_smooth(data = filter(dat2, date <= first(date)+3653), method = 'lm', color = 'red', formula = my.formula, se = FALSE) +
  geom_smooth(method = 'loess', color = 'grey30', linetype = 2, se = FALSE) +
  # stat_poly_eq(formula = my.formula, 
  #              aes(label = paste(..eq.label.., sep = "~~~")), 
  #              parse = TRUE) +
  # geom_text(x = Sys.Date()-(T/1.1), y = 35, label = paste("Total SLR = ", round(coef(m)[2]*T, 2), 'cm')) + 
  scale_y_continuous(name = paste(DATUM, '(cm)'),
                     breaks = seq(-40, 40, by = 10),
                     minor_breaks = seq(-40, 40, by = 5),
                     limits = c(-40, 40),
                     expand = c(0,0)) + 
  scale_x_date(name = 'Year', 
               date_breaks = '12 months', 
               date_minor_breaks = '6 month',
               date_labels = '%Y',
               expand = c(0,0)) + 
              # limits = c(first(date), last(date))) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        legend.position = 'bottom',
        panel.background = element_rect(fill = "white", color = 'black', linetype = 1),
        #panel.border = element_rect(fill = 'white', color = 'black'),
        panel.grid.major.y = element_line(colour = 'black', linetype = 2),
        axis.ticks.x = element_line(color = 'black')) + 
  ggtitle(paste("Sea Level Trend Over Past ", T_name, sep = ''))  +
  annotate(geom = 'text', label = paste("Total Observed SLR for Period =", round(coef(m)[2]*T, 2), 'cm'), 
           x = Sys.Date()-(T), y = Inf, hjust = -0.1, vjust = 5) + 
  # annotate(geom = 'segment', color = 'red', 
  #          x = Sys.Date()-(T), y = Inf) + 
  annotate(geom = 'text', label = paste("Linear Relative Sea Level Trend (black line) =", round((coef(m)[2]*T)/(T/100), 2)*10, 'mm/yr'), 
           x = Sys.Date()-(T), y = Inf, hjust =-0.08, vjust = 7) + 
  labs(caption = paste("Data: Monthly ", DATUM, " for NOAA Station ID: ", dat2$station, 
                       ', ', first(dat2$date), ' to ', last(dat2$date), sep = ''))
#   facet_wrap(~station)
fig2

# as.Date(date >= Sys.Date()-T)))

# save plots as .png
ggsave(fig2, file=paste(datadir,
                       '/figures/sea-level-trends_', i, "-decade_station-", dat2$station[1], ".png", sep=''), width = 6, height = 4, units = 'in', scale=2)
}

