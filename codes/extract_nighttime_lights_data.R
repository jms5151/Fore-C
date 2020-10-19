# extract nasa night time lights values for coral regions ---------------------- 
# https://gist.github.com/giacfalk/33e78e64161ae532390052dc578d6197

library(raster)
library(rhdf5)
library(rgdal)
library(maps)

f = H5Fopen("data/VNP46A1.A2020288.h00v00.001.2020289062608.h5")

spInfo <- h5readAttributes(f,"/HDFEOS/GRIDS/VNP_Grid_DNB/") 

myCrs<-4326
res <- 2400
xMin<-spInfo$WestBoundingCoord
yMin<-spInfo$SouthBoundingCoord
yMax<-spInfo$NorthBoundingCoord
xMax<-spInfo$EastBoundingCoord
nRows <- 2400
nCols <- 2400
myNoDataValue <- NA

band2Raster <- function(file, noDataValue, xMin, yMin, res, crs){
  # select the band you wish to export, here set to be the DNB_At_Sensor_Radiance_500m
  out<- h5read(f,"/HDFEOS/GRIDS/VNP_Grid_DNB/Data Fields/DNB_At_Sensor_Radiance_500m",index=list(1:nCols,1:nRows))
  
  #transpose data to fix flipped row and column order 
  #depending upon how your data are formatted you might not have to perform this
  out <-t(out)
  
  #assign data ignore values to NA
  out[out == myNoDataValue] <- NA
  
  #turn the out object into a raster
  outr <- raster(out,crs=myCrs)
  
  #create extents class
  rasExt  <- raster::extent(c(xMin,xMax,yMin,yMax))
  
  #assign the extents to the raster
  extent(outr) <- rasExt
  
  #return the raster object
  return(outr)
}

r = band2Raster(file = f, noDataValue = myNoDataValue, xMin = xMin, yMin = yMin, res = res, crs = myCrs)

# save raster
writeRaster(r, "Data/nightlights.tif")

# save data
# save(nightLights_df, file="Compiled_data/Night_Lights.RData")

# visualize
# library(ggplot2)
# library(viridis)
# 
# ggplot() +
#   geom_raster(data = x , aes(x = Longitude, y = Latitude, fill = BlackMarble_2016_3km_geo)) +
#   scale_fill_viridis_c() +
#   coord_quickmap() + 
#   scale_x_continuous(limits = c(-180, 180))
