# fill grid with data --------------------------------------------------
rm(list=ls()) #remove previous variable assignments

# load libraries
library(RANN)
# library(sp)
# library(rgdal)
library(raster)

# load data
load("Compiled_data/grid.RData") # need 5km x 5km equally spaced grid, and then island mask
load("Compiled_data/ga_condition_index.RData")
load("Compiled_data/SST_MMM.RData")
load("Compiled_data/CHL_MMM.RData")
load("Compiled_data/Night_Lights.RData")
load("Compiled_data/prevalence_samples.RData")

# set current month
currentMonth <- as.numeric(substr(Sys.time(),6,7))

# subset monthly predictor variables, removing NA values
sst <- subset(SST_MMM, Month == currentMonth)
sst <- sst[complete.cases(sst),]
chl <- subset(CHL_MMM, Month == currentMonth)
chl <- chl[complete.cases(chl),]

# add night time lights data for each grid location 
nearTable <- as.data.frame(nn2(data = subset(nightLights_df, select = c(Longitude, Latitude)), query = subset(grid, select = c(Longitude, Latitude)), k = 1))
grid$dev_new <- nightLights_df$BlackMarble_2016_3km_geo[nearTable$nn.idx]

# add chl data for each grid location 
nearTable <- as.data.frame(nn2(data = subset(chl, select = c(Longitude, Latitude)), query = subset(grid, select = c(Longitude, Latitude)), k = 1))
grid$chl_new <- chl$CHL_MMM[nearTable$nn.idx]

# add SST data for each grid location 
nearTable <- as.data.frame(nn2(data = subset(sst, select = c(Longitude, Latitude)), query = subset(grid, select = c(Longitude, Latitude)), k = 1))
grid$temp_new <- sst$SST_MMM[nearTable$nn.idx]

# find the index (column name) with the most similar temp, chl-a, and night lights values
nearTable <- as.data.frame(nn2(data = subset(X_new_backtransformed, select = c(temp_new, chl_new, dev_new)), query = subset(grid, select = c(temp_new, chl_new, dev_new)), k = 1))
grid$index <- X_new_backtransformed$index[nearTable$nn.idx]

# save nowcast full posterior distribution of current risk for comparison with long term risk
ga_nowcast_risk <- merge(grid, prevalence, by="index")
ga_nowcast_risk <- ga_nowcast_risk[,c("Region", "Prevalence")]
ga_nowcast_risk$Disease_type <- "GA"
ga_nowcast_risk$Risk <- "Nowcast"
fileName <- paste0("Compiled_data/nowcasts/csv/ga_nowcast_risk_", substr(Sys.time(),1,10), ".RData")
save(ga_nowcast_risk, file=fileName)

# add mean prevalence values based on current conditions to visualize
prevalence2 <- prevalence %>% group_by(index) %>% summarize (Prevalence = mean(Prevalence))
grid_filled_ga <- merge(grid, prevalence2, by="index", all.x=T)

# subset data for raster
grid_filled_ga <- grid_filled_ga[,c("Longitude", "Latitude", "Prevalence")]

# create raster from point data
grid_filled_ga <- rasterFromXYZ(grid_filled_ga)

# set projection
crs(grid_filled_ga) <- CRS("+init=epsg:4326")

# save data
fileName <- paste0("Compiled_data/nowcasts/raster/ga_", substr(Sys.time(),1,10), ".tif")
writeRaster(grid_filled_ga, file=fileName)
