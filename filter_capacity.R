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
outRaster <- "C:/Users/user/OneDrive/Documents/RMP/capacity_classified2.tif"
inVector <- "C:/Users/user/OneDrive/Documents/RMP/capacity_classified1.tif"
theCommand <- sprintf("%s -te -180 -55 180 60 -ts 43200 13800 %s  %s", path_2_gdal_function,inVector, outRaster)
system.time(system(theCommand))

#load capacity raster
capacity_file <- "capacity_classified2.tif"
capacity <- raster(capacity_file)

#load forest raster, not excluded from wind power
#http://www.fao.org/geonetwork/srv/en/main.home?uuid=ba4526fd-cdbf-4028-a1bd-5a559c4bff38, tree covered area download
#f_file <- "forests/glc_shv10_04.tif"
#f <- raster(f_file)

#mask out forested areas from capacity
#f <- crop (f, extent(capacity))

#NAvalue(f) <- 0
#system.time(capacity_f <- mask(capacity, f, inverse = TRUE))
#plot(capacity_f)

#rasterize polygon protected area data downloaded from the wdpa #only need to run once
#https://www.protectedplanet.net/en/thematic-areas/wdpa?tab=WDPA
#extract 3 separate wdpa files and delete point folders. We only need the polygons.

#protected area file 0
path_2_gdal_function <- "C:/OSGEO4~1/bin/gdal_rasterize.exe"
outRaster <- "C:/Users/user/OneDrive/Documents/RMP/raster_wdpa_00.tif"
inVector <- "C:/Users/user/OneDrive/Documents/RMP/WDPA_Jun2021_Public_shp_0/WDPA_Jun2021_Public_shp-polygons.shp"
theCommand <- sprintf("%s -burn 1 -a_nodata 0 -te -180 -55 180 60 -ts 43200 13800 %s  %s", path_2_gdal_function,inVector, outRaster)
#theCommand <- sprintf("%s -burn 1 -a_nodata 0 -tr 0.008333333 0.008333333 %s  %s", path_2_gdal_function,inVector, outRaster)
system.time(system(theCommand))

#protected area file 1
outRaster <- "C:/Users/user/OneDrive/Documents/RMP/raster_wdpa_1.tif"
inVector <- "C:/Users/user/OneDrive/Documents/RMP/WDPA_Jun2021_Public_shp_1/WDPA_Jun2021_Public_shp-polygons.shp"
theCommand <- sprintf("%s -burn 1 -a_nodata 0 -te -180 -55 180 60 -ts 43200 13800 %s  %s", path_2_gdal_function,inVector, outRaster)
system.time(system(theCommand))

#protected area file 2
outRaster <- "C:/Users/user/OneDrive/Documents/RMP/raster_wdpa_2.tif"
inVector <- "C:/Users/user/OneDrive/Documents/RMP/WDPA_Jun2021_Public_shp_2/WDPA_Jun2021_Public_shp-polygons.shp"
theCommand <- sprintf("%s -burn 1 -a_nodata 0 -te -180 -55 180 60 -ts 43200 13800 %s  %s", path_2_gdal_function,inVector, outRaster)
system.time(system(theCommand))
#can delete original shapefiles after rasterizing for storage

#read and mask
wdpa_0 <- raster("raster_wdpa_00.tif")
system.time(capacity_f <- mask(capacity, wdpa_0, inverse = TRUE))
rm(wdpa_0)

wdpa_1 <- raster("raster_wdpa_1.tif")
system.time(capacity_f <- mask(capacity_f, wdpa_1, inverse = TRUE))
rm(wdpa_1)

wdpa_2 <- raster("raster_wdpa_2.tif")
system.time(capacity_f <- mask(capacity_f, wdpa_2, inverse = TRUE))
rm(wdpa_2)

plot(capacity_f)

#slope and elevation data downloaded from faostat soil terrain data, 30as, greater than 15 degree slope #run once
#http://www.fao.org/soils-portal/data-hub/soil-maps-and-databases/harmonized-world-soil-database-v12/terrain-data/en/
slopes1 <- raster("GloSlopesCl6_30as.asc")
NAvalue(slopes1) <- 0


slopes2 <- raster("GloSlopesCl7_30as.asc")
NAvalue(slopes2) <- 0

slopes3 <- raster("GloSlopesCl8_30as.asc")
NAvalue(slopes3) <- 0

f <-list.files(pattern = ".asc")  
mosaic_rasters(f, "15slopes.tif", "GTiff")


#retrieve raster and mask
s <- raster("15slopes.tif")
s <- crop(s, c(-180,180,-55,60))
NAvalue(s) <- 0
plot(s)
capacity_f <- mask(capacity_f, s, inverse = TRUE)
rm(s)

#retrieve elevation raster and mask
elev <- raster("GloElev_30as.asc")
plot(elev)
#elevations above 2000 meters are not suitable for wind power production
elev <- calc(elev, fun=function(x){ x[x > 2000] <- NA; return(x)} )
elev <- crop(elev, extent(capacity_f))
capacity_f <- mask(capacity_f, elev)


#load urban area raster downloaded from https://data.apps.fao.org/map/catalog/srv/eng/catalog.search#/metadata/ba4526fd-cdbf-4028-a1bd-5a559c4bff38
urban <- raster("Data/GlcShare_v10_01/glc_shv10_01.Tif")
NAvalue(urban) <- 0

urban <- crop(urban, extent(capacity_f))
system.time(capacity_f <- mask(capacity_f, urban, inverse = TRUE))
plot(capacity_f)

#write raster to file
writeRaster(capacity_f, "capacity_masked", "GTiff", overwrite = TRUE)
capacity_f <- raster("capacity_masked.tif")
plot(capacity_f)





#########junk code######
#create empty list
wdpa <- data.frame(matrix(ncol=0,nrow=0))

#take polygons, cast to multipolygons, and add to current list
for (i in 1:2){ #length(countries.na)
  tryCatch({
    #check if iso is valid
    if(is.na(suppressWarnings(countrycode::countrycode(countries.na[i], "iso3c", "country.name.en")))){
      print(sprintf("%s was not included", countries.na[i]))
      next
    }
    start <- Sys.time() #start time
    
    temp <- wdpa_fetch(countries.na[i]) %>% as.data.table()
    temp <- temp[st_dimension(temp$geometry) > 1]
    temp <- temp[, st_cast(temp$geometry, 'MULTIPOLYGON')]
    wdpa <- rbind(wdpa, temp)
    
    print(i)
    end <- Sys.time() #end time
    print(end-start) #iterate time
  }, error=function(e){
    print(sprintf("%s was not available for download", countries.na[i]))
  })
}

