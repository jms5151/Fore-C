# create grid for fore-c map ----------------------------------------------------
rm(list=ls()) #remove previous variable assignments

# Read in reef mask CSV. 0 = water, 1 = reef
# dims are 3600 rows (lats) 7200 cols (lons)
reefs <- read.csv("Data/Reef_grid/reef_plus_survey_gbr_deep_reef_remove_20210308.csv", header = FALSE)

# Make 5km Grid
lon5km <- -179.975+0.05*seq(0,7199) # Columns
lat5km <- -89.975+0.05*seq(0,3599) # Rows

# Get reef pixel Lats and Lons
inds <- which(reefs == 1, arr.ind=TRUE)
reefLat <- lat5km[inds[,1]]
reefLon<- lon5km[inds[,2]]

reefsDF <- data.frame("Longitude" = reefLon, "Latitude" = reefLat)
reefsDF$ID <- seq(1, nrow(reefsDF))

# save grid as csv
save(reefsDF, file = "Compiled_data/grid.RData")

# create raster from point data
reefsDF <- rasterFromXYZ(reefsDF)

# set projection
crs(reefsDF) <- CRS("+init=epsg:4326")

# create spatial polygon from raster
reefs2 <- as(reefsDF, "SpatialPolygonsDataFrame")

# save spatial polygon
save(reefs2, file = "Compiled_data/spatial_grid.Rds")

# check map 
leaflet() %>%
  addTiles(group = "OpenStreetMap") %>%
  addPolygons(data = reefs2) %>%
  setView(lng = -156, lat = 20 , zoom = 7)

