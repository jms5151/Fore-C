# extract predictor data for each survey point-------------------------------------
rm(list=ls()) #remove previous variable assignments

# load library
library(tidyverse)

# load predictor data
load("Compiled_data/surveys_with_benthic_and_fish_data.RData")
load("Compiled_data/Night_Lights.RData")
load("Compiled_data/surveys_sst_90dMean.RData")
wave_data <- read.csv("Compiled_data/msec_out_wave.csv", head = T)

# load response data
load("Compiled_data/GA.RData")
load("Compiled_data/WS.RData")

# Growth anomalies ---------------------------------------------------------------
GA_data <- ga %>%
  left_join(benthic_and_fish_data, by = c("Latitude", "Longitude")) %>%
  left_join(reef_nightlights, by = c("Latitude", "Longitude")) %>%
  left_join(sst_90d, by = c("Date", "Latitude", "Longitude")) %>%
  left_join(wave_data, by = c("Latitude", "Longitude"))

# format and save
GA_data$Region <- as.character(GA_data$Region)
GA_data$Month <- format(GA_data$Date, "%m")

save(GA_data, file = "Compiled_data/GA_with_predictors.RData")

# White syndromes ----------------------------------------------------------------
WS_data <- ws %>%
  left_join(benthic_and_fish_data, by = c("Latitude", "Longitude")) %>%
  left_join(wave_data, by = c("Latitude", "Longitude"))

# format and save
WS_data$Region <- as.character(WS_data$Region)
WS_data$Month <- format(WS_data$Date, "%m")

save(WS_data, file = "Compiled_data/WS_with_predictors.RData")
