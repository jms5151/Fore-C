# extract predictor data for each survey point-------------------------------------
rm(list=ls()) #remove previous variable assignments

# load library
library(RANN)
library(tidyverse)

# load predictor data
load("Compiled_data/SST_MMM.RData")
load("Compiled_data/CHL_MMM.RData")
load("Compiled_data/Night_Lights.RData")

# load response data
load("Compiled_data/GA.RData")

# format month
ga$Month <- as.numeric(format(ga$Date, "%m"))

# create new data frame to add predictor data
merged_ga <- ga
merged_ga$SST_MMM <- NA
merged_ga$SST_MMM_dist <- NA
merged_ga$CHL_MMM <- NA
merged_ga$CHL_MMM_dist <- NA

# pull Chl-a and SST predictor data by survey data date and location
months <- sort(unique(ga$Month))
pred.dfs <- list(CHL_MMM, SST_MMM)

for(i in months){
  # subset survey data by month
  ga_tmp <- subset(ga, Month == i)
  for (j in 1:length(pred.dfs)){
    # subset predictor data by type and month
    tmpdf <- subset(pred.dfs[[j]], Month == i)
    tmpdf <- tmpdf[complete.cases(tmpdf),]
    # Get near neighbor values and distances for each survey point
    nearTable <- as.data.frame(nn2(data = subset(tmpdf, select = c(Longitude, Latitude)), query = subset(ga_tmp, select = c(Longitude, Latitude)), k = 1))
    ga_tmp[colnames(tmpdf)[3]] <- tmpdf[,3][nearTable$nn.idx]
    ga_tmp[paste0(colnames(tmpdf)[3], "_dist")] <- nearTable$nn.dists
  }  
  # add predictor data to original dataset
  merged_ga[which(merged_ga$SITEVISITID %in% ga_tmp$SITEVISITID),c("CHL_MMM", "CHL_MMM_dist", "SST_MMM", "SST_MMM_dist")] <- ga_tmp[,c("CHL_MMM", "CHL_MMM_dist", "SST_MMM", "SST_MMM_dist")]
}

# add night time lights predictor data by survey location
merged_ga_backup <-merged_ga
nearTable <- as.data.frame(nn2(data = subset(nightLights_df, select = c(Longitude, Latitude)), query = subset(merged_ga, select = c(Longitude, Latitude)), k = 1))
merged_ga$NightLights <- nightLights_df$BlackMarble_2016_3km_geo[nearTable$nn.idx]
merged_ga$NightLights_dist <- nearTable$nn.dists

# remove survey with chl-a outlier
merged_ga$CHL_MMM[merged_ga$CHL_MMM > 4] <- NA
merged_ga <- merged_ga[complete.cases(merged_ga),]

# save data
save(merged_ga, file="compiled_data/GA_w_predictor_data.RData")

