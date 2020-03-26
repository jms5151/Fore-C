# extract noaa monthly mean SST climatology values for coral regions ------------------------- 
rm(list=ls()) #remove previous variable assignments

# load libraries
library(ncdf4)

# open file
crw_5km <- nc_open("Data/ct5km_climatology_v3.1.nc")

# subset lat and lon data
nc_lat <- ncvar_get(crw_5km, attributes(crw_5km$dim)$names[2]) # get latitude data
nc_lon <- ncvar_get(crw_5km, attributes(crw_5km$dim)$names[3]) # get longitude data 

# subset sst mmm 
jan <- ncvar_get(crw_5km, attributes(crw_5km$var)$names[2])
feb <- ncvar_get(crw_5km, attributes(crw_5km$var)$names[3])
mar <- ncvar_get(crw_5km, attributes(crw_5km$var)$names[4])
apr <- ncvar_get(crw_5km, attributes(crw_5km$var)$names[5])
may <- ncvar_get(crw_5km, attributes(crw_5km$var)$names[6])
jun <- ncvar_get(crw_5km, attributes(crw_5km$var)$names[7])
jul <- ncvar_get(crw_5km, attributes(crw_5km$var)$names[8])
aug <- ncvar_get(crw_5km, attributes(crw_5km$var)$names[9])
sep <- ncvar_get(crw_5km, attributes(crw_5km$var)$names[10])
oct <- ncvar_get(crw_5km, attributes(crw_5km$var)$names[11])
nov <- ncvar_get(crw_5km, attributes(crw_5km$var)$names[12])
dec <- ncvar_get(crw_5km, attributes(crw_5km$var)$names[13])

# reshape data
mmms <- list(jan, feb, mar, apr, may, jun, jul, aug, sep, oct, nov, dec)
months <- seq(1,12,1)

mmms_df <- data.frame(matrix(ncol=4,nrow=0))

for (i in 1:12){
  mmmSST_temp <- mmms[[i]]
  rownames(mmmSST_temp) <- nc_lon
  colnames(mmmSST_temp) <- nc_lat
  mmmSST_temp <- as.data.frame(as.table(mmmSST_temp))  
  mmmSST_temp$Month <- months[i]
  mmms_df <- rbind(mmms_df, mmmSST_temp)
}

#  name columns
colnames(mmms_df) <- c("Longitude", "Latitude", "SST_MMM", "Month")

# format longitude and latitude as numeric data
mmms_df$Longitude <- as.numeric(as.character(mmms_df$Longitude))
mmms_df$Latitude <- as.numeric(as.character(mmms_df$Latitude))

# subset data frame to lat/lon region of interest
SST_MMM <- subset(mmms_df, Longitude <=-120 | Longitude >=140)
SST_MMM <- subset(SST_MMM, Latitude >= -30 & Latitude <= 30) 

# save data
save(SST_MMM, file="Compiled_data/SST_MMM.RData")

# visualize
library(ggplot2)
library(viridis)

tmpdata <- subset(SST_MMM, Month==5)

ggplot() + geom_raster(data = tmpdata, aes(x=Longitude, y = Latitude, fill=SST_MMM)) +
  coord_fixed(ratio = 1) +
  scale_fill_viridis(direction = -1) +
  theme_bw() + 
  scale_x_continuous(limits = c(-180, 180))
