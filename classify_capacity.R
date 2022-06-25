setwd('C:/Users/user/OneDrive/Documents/RMP')

##import necessary packages
library(rgdal)
library(ggplot2)
library(raster)
library(gdalUtils)
library(broom)
library(maptools)
library(rgeos)
library(xlsx)
library(wdpar)
library(countrycode)

library(tidyverse)
library(data.table)
library(sf)

#quick reprojection (for my dataset) #run once
path_2_gdal_function <- "C:/OSGEO4~1/bin/gdalwarp.exe"
outRaster <- "C:/Users/user/OneDrive/Documents/RMP/capacity2.tif"
inVector <- "C:/Users/user/OneDrive/Documents/RMP/Data/capacity.tif"
theCommand <- sprintf("%s -t_srs WGS84 -te -180 -55 180 60 -tr 0.01 0.01 %s  %s", path_2_gdal_function,inVector, outRaster)
system.time(system(theCommand))

#load capacity raster
speed_file <- "wind_speed_100m.tif"
speed <- raster(speed_file)

#classI can use speeds greater than 10
capacity1 <- raster("capacity_IEC1.tif")
system.time(speed[speed < 10] <- NA) #deletes any windspeed less than 10
system.time(capacity1 <- mask(capacity1, speed))
writeRaster(capacity1, "capacity_IEC1.tif", type = "GTiff", overwrite = TRUE)

#classII can use speeds between than 10 and 7.5
speed <- raster(speed_file)
capacity2 <- raster("capacity_IEC2.tif")
system.time(speed[speed >= 10] <- NA) #deletes any windspeed greater than 10
system.time(speed[speed <= 7.5] <- NA) #deletes any windspeed less than 7.5
system.time(capacity2 <- mask(capacity2, speed))
writeRaster(capacity2, "capacity_IEC2.tif", type = "GTiff", overwrite = TRUE)

#classIII can use speeds less than 7.5
speed <- raster(speed_file)
capacity3 <- raster("capacity_IEC3.tif")
system.time(speed[speed > 7.5] <- NA) #deletes any windspeed greater than 7.5
system.time(capacity3 <- mask(capacity3, speed))
writeRaster(capacity3, "capacity_IEC3.tif", type = "GTiff", overwrite = TRUE)


#reproject
path_2_gdal_function <- "C:/OSGEO4~1/bin/gdalwarp.exe"
outRaster <- "C:/Users/user/OneDrive/Documents/RMP/capacity_IEC2w.tif"
inVector <- "C:/Users/user/OneDrive/Documents/RMP/capacity_IEC2.tif"
theCommand <- sprintf("%s -t_srs WGS84 -te -180 -55 180 60 -tr 0.01 0.01 %s  %s", path_2_gdal_function,inVector, outRaster)
system.time(system(theCommand))


path_2_gdal_function <- "C:/OSGEO4~1/bin/gdalwarp.exe"
outRaster <- "C:/Users/user/OneDrive/Documents/RMP/capacity_IEC3w.tif"
inVector <- "C:/Users/user/OneDrive/Documents/RMP/capacity_IEC3.tif"
theCommand <- sprintf("%s -t_srs WGS84 -te -180 -55 180 60 -tr 0.01 0.01 %s  %s", path_2_gdal_function,inVector, outRaster)
system.time(system(theCommand))

path_2_gdal_function <- "C:/OSGEO4~1/bin/gdalwarp.exe"
outRaster <- "C:/Users/user/OneDrive/Documents/RMP/capacity_IEC1w.tif"
inVector <- "C:/Users/user/OneDrive/Documents/RMP/capacity_IEC1.tif"
theCommand <- sprintf("%s -t_srs '+proj=longlat +datum=WGS84' -te -180 -55 180 60 -tr 0.01 0.01 %s  %s", path_2_gdal_function,inVector, outRaster)
system.time(system(theCommand))

#mosaic rasters of different IEc classes. For some reason, must do one by one, as mosaic_rasters() has errors

capacity1 <- raster("capacityIEC1.tif")
capacity2 <- raster("capacityIEC2.tif")
capacity3 <- raster("capacityIEC3.tif")

system.time(mosaic(capacity1,capacity2, fun = "max",filename = "capacity_classified.tif", overwrite = TRUE))

capacity <- raster("capacity_classified.tif")
system.time(mosaic(capacity,capacity3, fun = "max",filename = "capacity_classified1.tif"))


capacity <- raster("capacity_classified1.tif")


