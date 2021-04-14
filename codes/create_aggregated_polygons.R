# create polygons for virtual station or regions of reef grid pixels
library(raster)
library(sf)
library(maptools)
library(gpclib)
library(rgeos)
library(rgdal)
library(dplyr)

load("Compiled_data/grid.RData")
regions_df <- read.csv("Data/regional_polygon_coords.csv")

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
  polya$ID <- polyName
  polya <- spChFIDs(polya, polyName)
}


for(i in 1:15){ # keep PRIAs as one polygon for now, below row 14 splits them by smaller island groups
  x <- create_forec_poly(reefsDF, regions_df$lat1[i], regions_df$lat2[i], regions_df$lon1[i], regions_df$lon2[i], regions_df$region[i])
  assign(regions_df$region[i], x)
  x <- NULL
}

# combine polygons data
listPolys <- Filter(function(x) is(x, "SpatialPolygons"), mget(ls()))
region_poly <- do.call(rbind, listPolys)

# check polygons merged correctly by mapping
plot(region_poly)

library(leaflet)
leaflet() %>%
  addTiles(group = "OpenStreetMap") %>%
  addPolygons(data = region_poly,
              color = brewer.pal(length(region_poly), "Dark2"))

# save data
save(region_poly, file = "Compiled_data/regional_polygons.Rds")
