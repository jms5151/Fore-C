# load libraries
library(raster)
library(sf)
library(maptools)
library(gpclib)
library(rgeos)
library(rgdal)
library(dplyr)
library(sp)

rm(list=ls()) #remove previous variable assignments so when later code pulls from environment
# no msc polygons are listed

# create polygons for regions of reef grid pixels -----------------------------------------
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
  polya #<- spChFIDs(polya, polyName)
}

for(i in 6:15){ # keep PRIAs as one polygon for now, below row 14 splits them by smaller island groups
  x <- create_forec_poly(reefsDF, regions_df$lat1[i], regions_df$lat2[i], regions_df$lon1[i], regions_df$lon2[i], regions_df$region[i])
  assign(as.character(regions_df$region[i]), x)
  x <- NULL
}

# add GBRMPA regional management zones from https://gbrmpa.maps.arcgis.com/apps/webappviewer/index.html?id=f0c6bfcb6ccd413ca55c47d9440fc86d
gbr_major_management_areas <- rgdal::readOGR("Data/GBRMPA_shapefiles/Management_Areas_of_the_GBRMP__Poly_.shp")
gbr_major_management_areas <- spTransform(gbr_major_management_areas, CRS("+init=epsg:4326"))
gbr_major_management_areas@data <- data.frame("ID" = paste0("gbr_", gbr_major_management_areas@data$OBJECTID))

# combine regional polygon data
listPolys <- Filter(function(x) is(x, "SpatialPolygons"), mget(ls()))
region_poly <- do.call(rbind, listPolys)

# check polygons merged correctly by mapping
plot(region_poly)

library(leaflet)
library(RColorBrewer)
leaflet() %>%
  addTiles(group = "OpenStreetMap") %>%
  addPolygons(data = region_poly,
              color = brewer.pal(length(region_poly), "Dark2"))

# save data
save(region_poly, file = "Compiled_data/regional_polygons.Rds")

# create polygons for local management zones of reef grid pixels -------------------------
# might need to keep a second column in dataframe to indicate type of MPA for visualizing by color
# zoning areas
gbr_park_zoning <- rgdal::readOGR("Data/GBRMPA_shapefiles/Great_Barrier_Reef_Marine_Park_Zoning.shp")
gbr_park_zoning@data <- data.frame("ID" = paste0("gbr_", gbr_park_zoning@data$OBJECTID),
                                   "Type" = gbr_park_zoning@data$ALT_ZONE)
gbr_park_zoning <- spTransform(gbr_park_zoning, CRS("+init=epsg:4326"))

# Guam MPA layer from https://www.oc.nps.edu/CMSP/Guam/
guam_mpas <- rgdal::readOGR("Data/Guam_shapefiles/Guam_MPA_Boundaries.shp")
guam_mpas <- spTransform(guam_mpas, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
guam_mpas@data <- data.frame("ID" = paste0("guam_", guam_mpas@data$Name),
                             "Type" = guam_mpas@data$Name)
guam_mpas <- spTransform(guam_mpas, CRS("+init=epsg:4326"))

# CNMI MPA layer from https://www.oc.nps.edu/CMSP/CNMI/index.html
cnmi_mpas <- rgdal::readOGR("Data/CNMI_shapefiles/CNMI_Marine_Protected_Areas.shp")
cnmi_mpas <- spTransform(cnmi_mpas, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
cnmi_mpas@data <- data.frame("ID" = paste0("cnmi_", cnmi_mpas@data$OBJECTID),
                             "Type" = cnmi_mpas@data$Regulation)
cnmi_mpas <- spTransform(cnmi_mpas, CRS("+init=epsg:4326"))

# Hawaii layers from https://planning.hawaii.gov/gis/download-gis-data-expanded/
# used Marine Managed Areas (DAR)
hi_management_areas <- rgdal::readOGR("Data/Hawaii_shapefiles/MMA_DAR.shp")
# re-project from UTM to lat-lon
hi_management_areas <- spTransform(hi_management_areas, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
# change latitude to plot over antimeridian
for(i in  1:length(hi_management_areas@polygons)){
  for(j in 1:length(hi_management_areas@polygons[[i]]@Polygons)){ # polygons within polygons
    hi_management_areas@polygons[[i]]@Polygons[[j]]@coords[,1] <- hi_management_areas@polygons[[i]]@Polygons[[j]]@coords[,1] + 360
    hi_management_areas@polygons[[i]]@Polygons[[j]]@labpt[1] <- hi_management_areas@polygons[[i]]@Polygons[[j]]@labpt[1] + 360
  }
}
# rerun spTransform to change extent
hi_management_areas <- spTransform(hi_management_areas, CRS("+init=epsg:4326"))
hi_management_areas@data <- data.frame("ID" = paste0("hawaii_", hi_management_areas@data$Site_Name),
                                       "Type" = hi_management_areas@data$MMA_design)

management_zone_polys_list <- list(gbr_park_zoning,
                                   guam_mpas,
                                   cnmi_mpas,
                                   hi_management_areas) 

mpa_poly <- do.call(rbind, management_zone_polys_list)

# check polygons merged correctly by mapping
leaflet() %>%
  addTiles(group = "OpenStreetMap") %>%
  addPolygons(data = mpa_poly,
              color = brewer.pal(length(mpa_poly$Type), "Dark2"))

# save data
save(mpa_poly, file = "Compiled_data/local_polygons.Rds")

# list 5km polygons in each regional and local management polygon ----------------------------
load("Compiled_data/spatial_grid.Rds")

# pixels in regional polygons
RG_poly_pixels <- raster::intersect(region_poly, reefs2)
RPolygonID <- RG_poly_pixels$ID.1
RPixelID <- RG_poly_pixels$ID.2
regional_poly_pix_ids <- data.frame("PolygonID" = RPolygonID, "PixelID" = RPixelID)
save(regional_poly_pix_ids, file = "Compiled_data/pixels_in_regional_polygons.RData")

# pixels in local management polygons
LC_poly_pixels <- raster::intersect(mpa_poly, reefs2)
LPolygonID <- LC_poly_pixels$ID.1
LPixelID <- LC_poly_pixels$ID.2
local_poly_pix_ids <- data.frame("PolygonID" = LPolygonID, "PixelID" = LPixelID)
save(local_poly_pix_ids, file = "Compiled_data/pixels_in_local_polygons.RData")
