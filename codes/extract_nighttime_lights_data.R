# extract nasa night time lights values for coral regions ---------------------- 

# load libraries
library(raster)

# load data
nightLights <- brick("Data/BlackMarble_2016_3km_geo.tif")
load("Compiled_data/Survey_points.RData")

# extract nighttime lights data
nightlights <- extract(nightLights, cbind(surveys$Longitude, surveys$Latitude))

# merge data
reef_nightlights <- cbind(surveys, nightlights)

# save data
save(reef_nightlights, file = "Compiled_data/Night_Lights.RData")