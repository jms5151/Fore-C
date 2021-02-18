# extract predictor data for each survey point-------------------------------------
rm(list=ls()) #remove previous variable assignments

# load library
library(tidyverse)

# load predictor data
load("Compiled_data/surveys_with_benthic_and_fish_data.RData")
load("Compiled_data/Night_Lights.RData")
wave_data <- read.csv("Compiled_data/msec_out_wave.csv", head = T)

# load response data
load("Compiled_data/GA.RData")
load("Compiled_data/WS.RData")

# Growth anomalies ---------------------------------------------------------------
GA_data <- ga %>%
  left_join(benthic_and_fish_data, by = c("Latitude", "Longitude")) %>%
  left_join(reef_nightlights, by = c("Latitude", "Longitude")) %>%
  left_join(wave_data, by = c("Latitude", "Longitude"))

save(GA_data, file = "Compiled_data/GA_with_predictors.RData")

# White syndromes ----------------------------------------------------------------
WS_data <- ws %>%
  left_join(benthic_and_fish_data, by = c("Latitude", "Longitude")) %>%
  left_join(wave_data, by = c("Latitude", "Longitude"))

save(WS_data, file = "Compiled_data/WS_with_predictors.RData")