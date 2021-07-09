# extract fish and benthic data to grid
# load libraries
library(rgdal)
library(sf)
library(sp)
library(spatialEco)
library(tidyverse)
library(raster)

# load data
load("Compiled_data/grid.RData")
load("Compiled_data/Benthic_cover_by_noaa_sector.RData")
load("Compiled_data/Fish_abundance_by_noaa_sector.RData")
shapename <- shapefile('Data/NOAA_shapefiles/ALLPacific_Sectors_Islands.shp')
r = raster("Data/GBR_LTMP_Fish-Herbivores/LTMP_Fish-Herbivores-Value.asc") 
rhis_surveys <- read.csv("Data/GBRMPA_RHIS_01012009-18092020.csv")
load("Compiled_data/GA.RData")
load("Compiled_data/WS.RData")

# function to extract aggregated point values within each pixel
aggregate_point_values <- function(df, x, fn){
  # create raster of df data
  dat <- df[, c(x, "Longitude", "Latitude")]
  coordinates(dat) = ~Longitude+Latitude
  proj4string(dat) = CRS("+init=epsg:4326") # set it to lat-long
  reefs_raster = raster(dat)
  # calculate mean values across all df points within each grid pixel
  dat2 <- rasterize(dat, reefs_raster, x, fun = fn, na.rm = T)
  # extract data points for each pixel in grid
  extract(dat2, cbind(reefsDF$Longitude, reefsDF$Latitude))
}

# NOAA data ----------------------------------------------------------------
# extract points from nearest noaa sectors shapefile
dat <- reefsDF[, c("Longitude", "Latitude")]
coordinates(dat) = ~Longitude+Latitude
proj4string(dat) = CRS("+init=epsg:4326") 

# Set up containers for results
reefsDF$Island_code <- NA
reefsDF$Sector <- NA
reefsDF$DistanceToNearestSector <- NA

## For each point, find name of nearest polygon (this takes a bit of time)
minNOAArow <- which.max(reefsDF$ID[reefsDF$Region == "gbr"])
for (i in minNOAArow:nrow(reefsDF)) {
  gDists <- gDistance(dat[i,], shapename, byid=TRUE)
  reefsDF$Island_code[i] <- shapename$ISLAND_CD[which.min(gDists)]
  reefsDF$Sector[i] <- shapename$SEC_NAME[which.min(gDists)]
  reefsDF$DistanceToNearestSector[i] <- min(gDists)
}

# calculate median coral sizes for different families based on survey data
ga_spread <- ga %>% spread(Family, Median_colony_size)
ws_spread <- ws %>% spread(Family, Median_colony_size)
dz <- rbind(ga_spread[, c("Date", "Latitude", "Longitude", "Acroporidae", "Poritidae")],
            ws_spread[, c("Date", "Latitude", "Longitude", "Acroporidae", "Poritidae")])
dz <- unique(dz)
reefsDF$Median_colony_size_Acroporidae <- aggregate_point_values(dz, "Acroporidae", median)
reefsDF$Median_colony_size_Poritidae <- aggregate_point_values(dz, "Poritidae", median)

# merge benthic and fish data
noaa_benthic_and_fish <- reefsDF %>%
  left_join(noaa_benthic_sub[,c("Sector", "Poritidae_mean_cover", "Acroporidae_mean_cover")], by = "Sector") %>%
  left_join(noaa_fish_sub[,c("Sector", "H_abund", "Parrotfish_abund", "Butterflyfish_abund")], by = "Sector")


# GBR data ----------------------------------------------------------------
# extract fish data by grid
fish <- extract(r, cbind(reefsDF$Longitude, reefsDF$Latitude))
fish_gbr <- cbind(reefsDF, "Fish_abund" = fish) 

# calculate mean coral cover based on RHIS surveys
reefsDF$Coral_cover <- aggregate_point_values(rhis_surveys, "Plate.Table.Coral....Total.", mean)

# merge and save data -----------------------------------------------------
benthic_and_fish_data <- noaa_benthic_and_fish %>%
  left_join(fish_gbr, by = c("Latitude", "Longitude"))

save(benthic_and_fish_data, file = "Compiled_data/grid_with_benthic_and_fish_data.RData")
