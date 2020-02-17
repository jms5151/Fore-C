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

# format date
tls$Date <- as.Date(tls$Date, "%Y-%m-%d")

# create vector of dates
dateRange <- unique(tls$Date)
dateRange <- gsub("-", "", dateRange) # remove hyphens from dates

# create folder for file downloads if it does not already exist
subDir <- "coralTemp_ncFiles"

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

# create new file to store daily temperautre data 
newFileName <- "sst_tls.csv"
sst.df <- data.frame(matrix(ncol=3, nrow=0))
colnames(sst.df) <- c("SiteName", "Date", "SST")
write.csv(sst.df, newFileName, row.names = F)

# extract daily temperature data for all sites from coralTemp and save in "sst_tls.csv"
ncFiles <- list.files(subDir)

for (j in 1:length(ncFiles)){ 
  ncFileName <- paste0(subDir, "/", ncFiles[j])
  ncTempBrick <- brick(ncFileName)
  surveySST <- extract(ncTempBrick, cbind(tls$Longitude, tls$Latitude))
  temp.df <- data.frame("SiteName"=tls$SiteName, "Date"=substr(ncFiles[j],11,18), "SST"=surveySST[,])
  write.table(temp.df, file=newFileName, row.names = F, sep = ",", col.names = !file.exists(newFileName), append = T)
  # cat("fininshed", ncFiles[j], "\n) # uncomment if you want a print message of progress
  # do not worry about warning messages, they are providing information about the "brick" function
}

# format and merge sst.df data with original survey data
sst.df <- read.csv(newFileName, head=T, stringsAsFactors=F)
sst.df$Date <- paste(substr(sst.df$Date,1,4), substr(sst.df$Date,5,6), substr(sst.df$Date,7,8), sep="-")
sst.df$Date <- as.Date(sst.df$Date, "%Y-%m-%d")
sst.df <- merge(tls, sst.df, by=c("SiteName", "Date"))

# save intermediate SST data
write.csv(sst.df, "sst_tls.csv", row.names=F)