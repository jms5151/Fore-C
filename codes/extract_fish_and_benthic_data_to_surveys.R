# extract fish and benthic data to survey points

# load libraries
library(rgdal)
library(sf)
library(sp)
library(spatialEco)
library(tidyverse)
library(raster)

# load data
load("Compiled_data/Benthic_cover_by_noaa_sector.RData")
load("Compiled_data/Fish_abundance_by_noaa_sector.RData")
r = raster("Data/GBR_LTMP_Fish-Herbiores/LTMP_Fish-Herbivores-Value.asc") 
load("Compiled_data/survey_points_for_map.RData")
shapename <- read_sf('Data/NOAA_shapefiles/ALLPacific_Sectors_Islands.shp')

# NOAA data ----------------------------------------------------------------
# Convert data frame to sf object
my.sf.point <- st_as_sf(x = surveys,
                        coords = c("Longitude", "Latitude"),
                        crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

# Overlay points and extract just the code column: 
pts.poly2 <- point.in.poly(my.sf.point, shapename)
dat <- as.data.frame(pts.poly2)
dat2 <- dat[ , !names(dat) %in% c("pt.ids","SEC_FISHIN", "SEC_HABITA", "Region")] # remove columns by name
colnames(dat2) <- c("Island_code", "Sector", "Longitude", "Latitude")

# merge with benthic data
noaa_benthic <- merge(dat2, noaa_benthic_sub[,c("Sector", "Poritidae_mean_cover", "Acroporidae_mean_cover")], by = "Sector", all.x = T)

# merge with fish data
noaa_fish <- merge(dat2, noaa_fish_sub[,c("Sector", "H_abund", "Parrotfish_abund", "Butterflyfish_abund")], by = "Sector", all.x = T)

# merge benthic and fish data
noaa_benthic_and_fish <- cbind(noaa_fish, noaa_benthic[,c("Poritidae_mean_cover", "Acroporidae_mean_cover")])

# GBR data ----------------------------------------------------------------
# extract fish data by survey points
fish <- extract(r, cbind(surveys$Longitude, surveys$Latitude))
fish_gbr <- cbind(surveys, "Fish_abund" = fish) 

# merge and save data -----------------------------------------------------
benthic_and_fish_data <- noaa_benthic_and_fish %>%
  left_join(fish_gbr, by = c("Latitude", "Longitude"))

save(benthic_and_fish_data, file = "Compiled_data/surveys_with_benthic_and_fish_data.RData")
