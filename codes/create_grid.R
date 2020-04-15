# create grid for fore-c map based on CRW virtual station pixels ---------------------
rm(list=ls()) #remove previous variable assignments

# libraries
library(raster)

# Open the NC file as a raster brick 
thermHistBrick <- brick("Data/noaa_crw_thermal_history_sst_trend_v2.1.nc", varname="mask")

# Turn the brick into a points file
reefMaskPts <- as.data.frame(rasterToPoints(x = thermHistBrick[[1]]))

# Change the names of the columns
colnames(reefMaskPts) <- c("Longitude", "Latitude", "ReefMask")

# Only keep the points equal to 1
reefMaskPts <- subset(reefMaskPts, ReefMask == 1)

# remove ReefMask column
grid <- reefMaskPts[,c("Longitude", "Latitude")]

# add region name for regions of interest 
grid$Region <- NA
grid$Region[grid$Longitude > 140.5 & grid$Longitude < 146.5 & grid$Latitude > -12.1 & grid$Latitude < -9] <- "GBR"
grid$Region[grid$Longitude > 140.5 & grid$Longitude < 155.5 & grid$Latitude > -26.5 & grid$Latitude < -12.1] <- "GBR"
grid$Region[grid$Longitude > -178.5 & grid$Longitude < -153.5 & grid$Latitude > 16.5 & grid$Latitude < 37.5] <- "Hawaii"
grid$Region[grid$Longitude > -178.5 & grid$Longitude < -174.5 & grid$Latitude > -1.5 & grid$Latitude < 2.5] <- "PRIAs"
grid$Region[grid$Longitude > -170.5 & grid$Longitude < -168.5 & grid$Latitude > 15.5 & grid$Latitude < 17.5] <- "PRIAs"
grid$Region[grid$Longitude > 165.5 & grid$Longitude < 168.5 & grid$Latitude > 17.5 & grid$Latitude < 21.5] <- "PRIAs"
grid$Region[grid$Longitude > 140.5 & grid$Longitude < 150.5 & grid$Latitude > 11.5 & grid$Latitude < 21.5] <- "Marianas"
grid$Region[grid$Longitude > -174.5 & grid$Longitude < -166.5 & grid$Latitude > -16.5 & grid$Latitude < -8.5] <- "Samoa"

# subset grid to regions of interest
grid <- subset(grid, !is.na(Region))

# save grid
save(grid, file="Compiled_data/grid.RData")
