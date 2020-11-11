# create grid for fore-c map based on CRW virtual station pixels ---------------------
rm(list=ls()) #remove previous variable assignments

# create Hawaii grid from Ocean Tipping Points data
create_grid <- function(rasterName){
  OTP_grid <- raster(rasterName)
  OTP_grid_df <- as.data.frame(rasterToPoints(OTP_grid))
  OTP_grid_df <- OTP_grid_df[,1:2] # subset to X and Y
  colnames(OTP_grid_df) <- c("Longitude", "Latitude") # rename columns
  OTP_grid_df
}

OTP_grid <- create_grid("Data/hi_otp_all_osds_effluent.tif")
save(OTP_grid_df, file="Compiled_data/grid_OTP_HI.RData") # save grid

# load libraries
library(caTools)
library(reshape2)

# load file
grid <- read.gif("Data/reefMask.GIF")

# extract grid from gif
grid <- grid[[1]]

# set column and rows ids into longitude and latitude coordinates 
colnames(grid) <- seq(-179.975, 179.975, by=0.05)
rownames(grid) <- seq(89.975, -89.975, by=-0.05) # or -89.975

# format from matrix to long data frame
grid <- melt(as.matrix(x))

# subset reef mask pixels
grid <- subset(grid, value == 0)

# remove value column
grid <- grid[,c("Var2", "Var1")]

# rename columns
colnames(grid) <- c("Longitude", "Latitude")

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