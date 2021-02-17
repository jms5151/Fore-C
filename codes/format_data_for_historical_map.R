# format historical data for map -------------------------------------------------------------
rm(list=ls()) #remove previous variable assignments

# load library
library(tidyverse)

# load data
load("Compiled_data/observational_data.RData")

# format location
observations$Location <- as.character(observations$Island)
observations$Location[is.na(observations$Location) == T] <- "Great Barrier Reef"

# format data for historical survey map on shiny app
historical_data <- observations %>%
  group_by(Longitude, Latitude, Location, Project) %>%
  summarize(N = length(Longitude), minYr = substr(min(Date),1,4), maxYr = substr(max(Date),1,4)) %>%
  filter(!is.na(Longitude))

# add pop up data
historical_data$survey_text <- paste0("<h3> <b> Location: </b>", historical_data$Location,
                           "<br> <b> Number of surveys: </b>", historical_data$N,
                           "<br> <b> Earliest survey: </b>", historical_data$minYr,
                           "<br> <b> Latest survey: </b>", historical_data$maxYr,
                           "<br> <b> Data source: </b>", historical_data$Project)

# make all longitudes negative to on same side of Pacific Ocean in leaflet
historical_data$Longitude <- ifelse(historical_data$Longitude>0, historical_data$Longitude-360, historical_data$Longitude)

# save data
save(historical_data, file="Compiled_data/historical_surveys.RData")

# Number of surveys -----------------------------------------------
# format data
observations$Region[observations$Region=="MHI"|observations$Region=="NWHI"] <- "Hawaii"
observations$Region[observations$Region=="MARIAN"] <- "Marianas"
observations$Region[observations$Region=="SAMOA"] <- "Samoa"

surveys <- observations %>%
  group_by(Region, Disease_type) %>%
  summarise(N = length(Region))

# save data
save(surveys, file="Compiled_data/number_surveys.RData")
