# extract nasa night time lights values for coral regions ---------------------- 
rm(list=ls()) #remove previous variable assignments

# load libraries
library(raster)
library(rgdal)

# load data
nightLights <- raster("Data/BlackMarble_2016_3km_geo.tif")

# raster to data frame
nightLights_df <- as.data.frame(nightLights, xy = TRUE)

# subset lat/lon to region of interest
nightLights_df <- subset(nightLights_df, x <= -120 | x >= 140 & y <= 30 & y >= -30)

# rename columns
colnames(nightLights_df) <- c("Longitude", "Latitude", "BlackMarble_2016_3km_geo")

# save data
save(nightLights_df, file="Compiled_data/Night_Lights.RData")

# visualize
library(ggplot2)
library(viridis)

ggplot() +
  geom_raster(data = x , aes(x = Longitude, y = Latitude, fill = BlackMarble_2016_3km_geo)) +
  scale_fill_viridis_c() +
  coord_quickmap() + 
  scale_x_continuous(limits = c(-180, 180))
