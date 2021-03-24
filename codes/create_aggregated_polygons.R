# create polygons for virtual station or regions of reef grid pixels
library(raster)
library(sf)
library(maptools)
library(gpclib)
library(rgeos)
library(rgdal)
library(dplyr)

load("Compiled_data/grid.RData")

create_forec_poly <- function(df, minLat, maxLat, minLon, maxLon, polyName){
  # subset data
  x <- subset(df, Latitude > minLat & Latitude < maxLat & Longitude > minLon & Longitude < maxLon)
  # shift longitude for mapping over antimeridian
  x$Longitude <- ifelse(x$Longitude < 0, x$Longitude + 360, x$Longitude) 
  # make spatial object
  x <- rasterFromXYZ(x, crs = "+init=epsg:4326")
  x <- as(x, "SpatialPolygonsDataFrame") 
  # aggregate polygon pixels to VS/regional polygon
  polya <- aggregate(x, dissolve = TRUE)
  # dynamically rename
  assign(x = polyName, value = polya)
  # save
  save(list = polyName, file = paste0("Compiled_data/regional_polygons/", polyName, ".Rds"))
}

# west amd south are negative
create_forec_poly(reefsDF, -30, -8, 140, 155, "GBR")
create_forec_poly(reefsDF, 18, 30, -180, -152, "Hawaii")
create_forec_poly(reefsDF, 12, 21, 143, 147, "CNMI")
create_forec_poly(reefsDF, -16, -10, -174, -167, "Samoas")
create_forec_poly(reefsDF, 18, 21, 165, 168, "Wake")
create_forec_poly(reefsDF, -2, 18, -178, -159, "PRIAs")