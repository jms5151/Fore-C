# extract SST values for coral health surveys ---------------------- 
rm(list=ls()) #remove previous variable assignments

# load libraries
library(lubridate)
library(RCurl)
library(raster)
library(ncdf4)
library(dplyr)

# load site data
load("merged_data_tls.R")

# create vector of dates
dateRange <- unique(tls$Date)
dateRange <- gsub("-", "", dateRange) # remove hyphens from dates

# name of subdirectory for nc files
subDir <- "coralTemp_ncFiles"

# download data ----------------------------------------------------
# create folder for file downloads if it does not already exist
if (!file.exists(subDir)){
  dir.create(file.path(subDir))
}

# download coralTemp data from ftp server
for (i in 1:length(dateRange)){ 
  YR <- substr(dateRange[i], 1, 4)
  coralTemp.URL <- paste0("ftp://ftp.star.nesdis.noaa.gov/pub/sod/mecb/crw/data/coraltemp/v1.0/nc/", YR, "/coraltemp_v1.0_", dateRange[i], ".nc")
  fileName <- paste0(subDir, "/coraltemp_", dateRange[i], ".nc")
  download.file(coralTemp.URL, mode="wb", fileName)
}

# extract sst data ------------------------------------------------
library(ncdf4)
library(sp)
library(RANN)

# create new variable to store sst values 
tls$sst <- NA
tls$dist <- 0
  
# extract daily temperature data for all sites from coralTemp and save in "sst_tls.csv"
ncFiles <- list.files(subDir)

for (j in 1:length(dateRange)){
  date_of_interest <- paste(substr(dateRange[j],1,4), substr(dateRange[j],5,6), substr(dateRange[j],7,8), sep="-")
  # subset survey data to specified date
  tls_temp <- subset(tls, Date == date_of_interest)
  # open netcdf for specified date
  ncFileName <- paste0(subDir, "/", ncFiles[grepl(dateRange[j], ncFiles)])
  crw_5km <- nc_open(ncFileName)
  # subset sst, lat, and lon data
  sst <- ncvar_get(crw_5km, attributes(crw_5km$var)$names[1])
  nc_lat <- ncvar_get(crw_5km, attributes(crw_5km$dim)$names[3]) # get latitude data
  nc_lon <- ncvar_get(crw_5km, attributes(crw_5km$dim)$names[2]) # get longitude data 
  # extract sst values for each survey
  for (k in 1:nrow(tls_temp)){
    closestLat <- which.min(abs(nc_lat-tls_temp$Latitude[k]))
    closestLon <- which.min(abs(nc_lon-tls_temp$Longitude[k]))
    # add sst data back to survey dataframe
    id_temp <- tls_temp$ID2[k]
    tls$sst[id_temp] <- ifelse(length(sst[closestLon, closestLat])!=0, sst[closestLon, closestLat], NA) 
    # if sst is NA, find sst value for nearest neighbor and distance to that pixel
    if (is.na(tls$sst[id_temp])==TRUE){
      # turn survey coordinates into matrix 
      SP_survey <- as.matrix(data.frame("lon"=nc_lon[closestLon], "lat"=nc_lat[closestLat]))
      # create data frame of nearby pixels
      lons <- c(nc_lon[closestLon-2], nc_lon[closestLon-1], nc_lon[closestLon+1], nc_lon[closestLon+2])
      lats <- c(nc_lat[closestLat], nc_lat[closestLat-2], nc_lat[closestLat-1],nc_lat[closestLat+1], nc_lat[closestLat+2])
      x <- as.data.frame(expand.grid("lon"=lons, "lat"=lats))
      if (nrow(x)!=0){
        # calculate distance to each nearby pixel
        nearTable <- as.data.frame(nn2(data = SP_survey, query = x, k = 1))
        nearTable <- cbind(nearTable, x)
        nearTable <- nearTable[order(nearTable$nn.dists),]
        # calculate sst at nearby pixels and stop at first pixel with sst data
        sstNear <- NA
        while (is.na(sstNear)==TRUE){
          for (l in 1:nrow(nearTable)){
            closestLat <- which.min(abs(nc_lat-nearTable$lat[l]))
            closestLon <- which.min(abs(nc_lon-nearTable$lon[l]))    
            sstNear <- ifelse(length(sst[closestLon, closestLat])!=0, sst[closestLon, closestLat], NA) 
            if (l == nrow(nearTable)){
              sstNear <-99
            }
          }
        }
        tls$sst[id_temp] <- sstNear
        tls$dist[id_temp] <- nearTable$nn.dists[l]
      }        
    }
  }  
}

# replace 99 values with NA
tls$sst[tls$sst == 99] <- NA

# save data
save(tls, file="sst_tls.RData")