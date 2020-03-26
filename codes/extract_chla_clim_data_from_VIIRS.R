# extract nasa VIIRS S-NPP Chl-a climatology values for coral regions ---------------------- 
rm(list=ls()) #remove previous variable assignments

# load libraries
library(ncdf4)

# list chlorophyll-a data files
chl_files <- list.files("Data/VIIRS_chla_monthly_climatology/")

# extract data, this takes a lot of memory, may need to break up into two groups
mmms_df <- data.frame(matrix(ncol=4,nrow=0))

for (i in 9:12){
  # load data file
  chl <- nc_open(paste0("Data/VIIRS_chla_monthly_climatology/", chl_files[i]))
  nc_chl <- ncvar_get(chl, attributes(chl$var)$names[1])
  # subset lat and lon data
  nc_lat <- ncvar_get(chl, attributes(chl$dim)$names[1]) # get latitude data
  nc_lon <- ncvar_get(chl, attributes(chl$dim)$names[2]) # get longitude data 
  # label rows and columns by lon and lat
  rownames(nc_chl) <- nc_lon
  colnames(nc_chl) <- nc_lat
  # format to data frame
  mmmCHL_temp <- as.data.frame(as.table(nc_chl))  
  # add time stamp
  mmmCHL_temp$Month <- substr(chl_files[[i]],6,8)
  # add data to dataframe
  mmms_df <- rbind(mmms_df, mmmCHL_temp)
}

#  name columns
colnames(mmms_df) <- c("Longitude", "Latitude", "CHL_MMM", "Month")

# format longitude and latitude as numeric data
mmms_df$Longitude <- as.numeric(as.character(mmms_df$Longitude))
mmms_df$Latitude <- as.numeric(as.character(mmms_df$Latitude))

# subset data frame to lat/lon region of interest
CHL_MMM <- subset(mmms_df, Longitude <=-120 | Longitude >=140)
CHL_MMM <- subset(CHL_MMM, Latitude >= -30 & Latitude <= 30)

# format time stamp from days to months
CHL_MMM$Month <- as.numeric(CHL_MMM$Month)
CHL_MMM$Month[CHL_MMM$Month==32] <- 2
CHL_MMM$Month[CHL_MMM$Month==61] <- 3
CHL_MMM$Month[CHL_MMM$Month==92] <- 4
CHL_MMM$Month[CHL_MMM$Month==122] <- 5
CHL_MMM$Month[CHL_MMM$Month==153] <- 6
CHL_MMM$Month[CHL_MMM$Month==183] <- 7
CHL_MMM$Month[CHL_MMM$Month==214] <- 8
CHL_MMM$Month[CHL_MMM$Month==245] <- 9
CHL_MMM$Month[CHL_MMM$Month==275] <- 10
CHL_MMM$Month[CHL_MMM$Month==306] <- 11
CHL_MMM$Month[CHL_MMM$Month==336] <- 12

# save data
save(CHL_MMM, file="Compiled_data/CHL_MMM.RData")

# visualize
library(ggplot2)
library(viridis)

tmpdata <- subset(CHL_MMM, Month==5)

ggplot() + geom_raster(data = tmpdata, aes(x=Longitude, y = Latitude, fill=CHL_MMM)) +
  coord_fixed(ratio = 1) +
  scale_fill_viridis(direction = -1) +
  theme_bw() + 
  scale_x_continuous(limits = c(-180, 180))
