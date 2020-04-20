# create grid for fore-c map based on CRW virtual station pixels ---------------------
rm(list=ls()) #remove previous variable assignments

# load file
grid <- read.csv("Data/mask5km.csv", head=F)

# format file
grid <- grid[,c("V3", "V4")]
colnames(grid) <- c("Latitude", "Longitude")
  
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
