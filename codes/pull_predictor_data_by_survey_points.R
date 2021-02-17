# extract predictor data for each survey point-------------------------------------
rm(list=ls()) #remove previous variable assignments

# load library
library(RANN)
library(tidyverse)
library(raster)
library(rgdal)

# load predictor data
load("Compiled_data/SST_MMM.RData")
load("Compiled_data/CHL_MMM.RData")
load("Compiled_data/Night_Lights.RData")
load("Compiled_data/GA_with_noaa_data.RData")
sesync_data <- read.csv("Compiled_data/msec_out_wave_and_market.csv", head = T)
nightLights <- raster("Data/nightlights.tif")

# load response data
load("Compiled_data/GA.RData")

# # format month
# ga$Month <- as.numeric(format(ga$Date, "%m"))
# 
# create new data frame to add predictor data
merged_ga <- ga
# merged_ga$SST_MMM <- NA
# merged_ga$SST_MMM_dist <- NA
# merged_ga$CHL_MMM <- NA
# merged_ga$CHL_MMM_dist <- NA
# 
# # pull Chl-a and SST predictor data by survey data date and location
# months <- sort(unique(ga$Month))
# pred.dfs <- list(CHL_MMM, SST_MMM)
# 
# for(i in months){
#   # subset survey data by month
#   ga_tmp <- subset(ga, Month == i)
#   for (j in 1:length(pred.dfs)){
#     # subset predictor data by type and month
#     tmpdf <- subset(pred.dfs[[j]], Month == i)
#     tmpdf <- tmpdf[complete.cases(tmpdf),]
#     # Get near neighbor values and distances for each survey point
#     nearTable <- as.data.frame(nn2(data = subset(tmpdf, select = c(Longitude, Latitude)), query = subset(ga_tmp, select = c(Longitude, Latitude)), k = 1))
#     ga_tmp[colnames(tmpdf)[3]] <- tmpdf[,3][nearTable$nn.idx]
#     ga_tmp[paste0(colnames(tmpdf)[3], "_dist")] <- nearTable$nn.dists
#   }  
#   # add predictor data to original dataset
#   merged_ga[which(merged_ga$SITEVISITID %in% ga_tmp$SITEVISITID),c("CHL_MMM", "CHL_MMM_dist", "SST_MMM", "SST_MMM_dist")] <- ga_tmp[,c("CHL_MMM", "CHL_MMM_dist", "SST_MMM", "SST_MMM_dist")]
# }


surveyNTL <- extract(nightLights, cbind(merged_ga$Longitude, merged_ga$Latitude))
temp.df <- cbind(merged_ga, "NL" = surveyNTL[,])

# then take the raster value with lowest distance to point AND non-NA value in the raster
sampled = apply(X = merged_ga[,c("Longitude", "Latitude")], MARGIN = 1, FUN = function(xy) surveyNTL@data@values[which.min(replace(distanceFromPoints(surveyNTL, xy), is.na(surveyNTL), NA))])

# then take the raster value with lowest distance to point AND non-NA value in the raster
sampled = apply(X = xy, MARGIN = 1, FUN = function(xy) r@data@values[which.min(replace(distanceFromPoints(r, xy), is.na(r), NA))])


# add night time lights predictor data by survey location
# merged_ga_backup <-merged_ga
nearTable <- as.data.frame(nn2(data = subset(nightLights_df, select = c(Longitude, Latitude)), query = subset(merged_ga, select = c(Longitude, Latitude)), k = 1))
merged_ga$NightLights <- nightLights_df$BlackMarble_2016_3km_geo[nearTable$nn.idx]
merged_ga$NightLights_dist <- nearTable$nn.dists

# # remove survey with chl-a outlier
# merged_ga$CHL_MMM[merged_ga$CHL_MMM > 4] <- NA
# merged_ga <- merged_ga[complete.cases(merged_ga),]

# add NOAA benthic and fish data
merged_ga <- merge(merged_ga, ga_benthic_and_fish, by = "HS_ID")#c("Region", "Island", "Sector", "Longitude", "Latitude", "HS_ID"))

# add sesync data
# colnames(sesync_data)[1:2] <- c("Longitude", "Latitude")
merged_ga <- merge(merged_ga, sesync_data[,c("HS_ID", "wave_mean", "dist_market")], by = c("HS_ID"), all.x = T)

# save data
save(merged_ga, file="compiled_data/GA_w_predictor_data.RData")

