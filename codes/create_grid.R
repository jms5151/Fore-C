# create grid for fore-c map ----------------------------------------------------
library(raster)
library(leaflet)

# Read in reef mask CSV. 0 = water, 1 = reef
# dims are 3600 rows (lats) 7200 cols (lons)
reefs <- read.csv("Data/Reef_grid/reef_plus_survey_gbr_deep_reef_remove_20210308.csv", header = FALSE)
regions_df <- read.csv("Data/big_region_coords.csv")

# Make 5km Grid
lon5km <- -179.975+0.05*seq(0,7199) # Columns
lat5km <- -89.975+0.05*seq(0,3599) # Rows

# Get reef pixel Lats and Lons
inds <- which(reefs == 1, arr.ind=TRUE)
reefLat <- lat5km[inds[,1]]
reefLon<- lon5km[inds[,2]]

reefsDF <- data.frame("Longitude" = reefLon, "Latitude" = reefLat)

# add region and ID
set_regional_ids <- function(df, minLat, maxLat, minLon, maxLon, regionName, regionNumber){
  x <- subset(df, Latitude > minLat & Latitude < maxLat & Longitude > minLon & Longitude < maxLon)
  x$Region <- regionName
  startingId <- as.numeric(paste0(as.character(regionNumber), "0001"))
  x$ID <- seq(startingId, startingId+nrow(x)-1) 
  x
}

for(i in 1:nrow(regions_df)){
  x <- set_regional_ids(reefsDF, regions_df$lat1[i], regions_df$lat2[i], regions_df$lon1[i], regions_df$lon2[i], regions_df$region[i], regions_df$region_id[i])
  assign(regions_df$region[i], x)
  x <- NULL
}

# combine above datasets
reefsDF <- do.call(rbind, list(gbr, guam_cnmi, hawaii, johnston, prias, samoas, wake))

# save grid as csv
save(reefsDF, file = "Compiled_data/grid.RData")

# to display over antimeridian in leaflap maps, add +360 to longitudes below zero
reefsDF$Longitude <- ifelse(reefsDF$Longitude < 0, reefsDF$Longitude + 360, reefsDF$Longitude) 

# create raster from point data
reefsDF2 <- rasterFromXYZ(reefsDF, crs = "+init=epsg:4326")

# add sim prev data
reefsDF$drisk <- rnorm(nrow(reefsDF), mean = 0.10, sd = 0.05)
reefsDF$drisk[reefsDF$drisk < 0] <- 0
rr <- rasterize(reefsDF[,c("Longitude", "Latitude")], reefsDF2, field = reefsDF[,c("ID", "drisk")])

# create spatial polygon from raster
reefs2 <- as(rr, "SpatialPolygonsDataFrame") # reefsDF2 go back to this when removing simulated prevalence

# save spatial polygon
save(reefs2, file = "Compiled_data/spatial_grid.Rds")

# check map 
leaflet() %>%
  addTiles(group = "OpenStreetMap") %>%
  addPolygons(data = reefs2)