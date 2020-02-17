# NASA FORE-C project: map survey locations -----------------------------
rm(list=ls()) #remove previous variable assignments

# load packages 
library(leaflet)

# load data 
gpsPoints <- read.csv("gpsPoints_2018April26.csv", head=T)
source("C:/Users/Jeremy/Box Sync/R_functions/addScaleBar.R")

# map data points -------------------------------------------
# add pop up data
gpsPoints$survey_years <- paste0("Survey years: ", gpsPoints$YEARS,
                                 "<br> Data: ", gpsPoints$data,
                                 "<br> Site: ", gpsPoints$SITE)

# colors
pal <- colorFactor(palette = c('red', 'blue', 'yellow', 'orange'), domain = gpsPoints$data)

# make map
forcMap <- leaflet() %>% 
  addTiles(urlTemplate="http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}") %>%
  addCircles(data=gpsPoints, lat = ~latitude, lng = ~longitude, color = ~pal(data), popup = ~survey_years)
addScaleBar(forcMap)

