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

#load PVOUT raster
pvout_file <- "Data/World_PVOUT_GISdata_LTAy_DailySum_GlobalSolarAtlas_GEOTIFF/PVOUT.tif"
pvout <- raster(pvout_file)
plot(pvout)

#loud forest raster
f_file <- "forests/glc_shv10_04.tif"
f <- raster(f_file)

#crop both to NA only
na <- extent(-180, -50,10,60)
f.na <- crop(f, na)
pvout.na <- crop(pvout, na)
NAvalue(f.na) <- 0

##check crops (not necessary to run)
plot(pvout.na)
plot(f.na)

#mask out forested areas from pvout
f <- crop (f, extent(pvout))
NAvalue(f) <- 0
system.time(pvout_f <- mask(pvout, f, inverse = TRUE))
plot(pvout_f)


#load countries by continent NA
library("jsonlite") #install.packages("jsonlite", repos="https://cran.rstudio.com/")
json_file <- 'https://datahub.io/JohnSnowLabs/country-and-continent-codes-list/datapackage.json'
json_data <- fromJSON(paste(readLines(json_file), collapse=""))

for(i in 1:length(json_data$resources$datahub$type)){
  if(json_data$resources$datahub$type[i]=='derived/csv'){
    path_to_file = json_data$resources$path[i]
    data <- read.csv(url(path_to_file))
  }
}
data <- data %>% select('Continent_Name', 'Country_Name', 'Three_Letter_Country_Code') %>% as.data.table() 
#head(data)

#vector of ISOs in NA
countries.na <- data[Continent_Name == 'North America', Three_Letter_Country_Code]
countries.na <- countries.na[!is.na(suppressWarnings(countrycode::countrycode(countries.na, "iso3c", "country.name.en")))]

rm(json_file, json_data, codelist, i, path_to_file, data)

#rasterize polygon protected area data downloaded from the wdpa #only need to run once

#protected area file 0
path_2_gdal_function <- "C:/OSGEO4~1/bin/gdal_rasterize.exe"
outRaster <- "C:/Users/user/OneDrive/Documents/RMP/raster_wdpa_00.tif"
inVector <- "C:/Users/user/OneDrive/Documents/RMP/WDPA_Mar2021_Public_shp_0/WDPA_Mar2021_Public_shp-polygons.shp"
theCommand <- sprintf("%s -burn 1 -a_nodata 0 -te -180 -55 180 60 -ts 43200 13800 %s  %s", path_2_gdal_function,inVector, outRaster)
#theCommand <- sprintf("%s -burn 1 -a_nodata 0 -tr 0.008333333 0.008333333 %s  %s", path_2_gdal_function,inVector, outRaster)
system.time(system(theCommand))

#protected area file 1
outRaster <- "C:/Users/user/OneDrive/Documents/RMP/raster_wdpa_1.tif"
inVector <- "C:/Users/user/OneDrive/Documents/RMP/WDPA_Mar2021_Public_shp_1/WDPA_Mar2021_Public_shp-polygons.shp"
theCommand <- sprintf("%s -burn 1 -a_nodata 0 -te -180 -55 180 60 -ts 43200 13800 %s  %s", path_2_gdal_function,inVector, outRaster)
system.time(system(theCommand))

#protected area file 2
outRaster <- "C:/Users/user/OneDrive/Documents/RMP/raster_wdpa_2.tif"
inVector <- "C:/Users/user/OneDrive/Documents/RMP/WDPA_Mar2021_Public_shp_2/WDPA_Mar2021_Public_shp-polygons.shp"
theCommand <- sprintf("%s -burn 1 -a_nodata 0 -te -180 -55 180 60 -ts 43200 13800 %s  %s", path_2_gdal_function,inVector, outRaster)
system.time(system(theCommand))
#can delete original shapefiles after rasterizing for storage

#read and mask
wdpa_0 <- raster("raster_wdpa_0.tif")
system.time(pvout_f <- mask(pvout_f, wdpa_0, inverse = TRUE))
rm(wdpa_0)

wdpa_1 <- raster("raster_wdpa_1.tif")
system.time(pvout_f <- mask(pvout_f, wdpa_1, inverse = TRUE))
rm(wdpa_1)

wdpa_2 <- raster("raster_wdpa_2.tif")
system.time(pvout_f <- mask(pvout_f, wdpa_2, inverse = TRUE))
rm(wdpa_2)

plot(pvout_f)

#slope data downloaded from faostat soil terrain data, 30as, greater than 15 degree slope #run once
slopes1 <- raster("GloSlopesCl6_30as.asc")
NAvalue(slopes1) <- 0


slopes2 <- raster("GloSlopesCl7_30as.asc")
NAvalue(slopes2) <- 0

slopes3 <- raster("GloSlopesCl8_30as.asc")
NAvalue(slopes3) <- 0
# make a list of file names, perhaps like this:  
f <-list.files(pattern = ".asc")  
# as you have the arguments as a list call 'merge' with 'do.call'  
mosaic_rasters(f, "15slopes.tif", "GTiff")


#retrieve raster and mask
s <- raster("15slopes.tif")
s <- crop(s, extent(pvout_f))
NAvalue(s) <- 0
plot(s)
pvout_f <- mask(pvout_f, s, inverse = TRUE)


plot(pvout_f)


#write raster to file
writeRaster(pvout_f, "pvout_masked.tif", "GTiff")
pvout_f <- raster("Products/pvout_masked.tif")
plot(pvout_f)
capacity_f <- raster("Products/capacity_masked.tif")
plot(capacity_f)



#load urban area raster downloaded from https://data.apps.fao.org/map/catalog/srv/eng/catalog.search#/metadata/ba4526fd-cdbf-4028-a1bd-5a559c4bff38
urban <- raster("Data/GlcShare_v10_01/glc_shv10_01.Tif")
NAvalue(urban) <- 0
pvout_f <- raster("Products/pvout_masked.tif")

urban <- crop(urban, extent(pvout_f))
system.time(pvout_f <- mask(pvout_f, urban, inverse = TRUE))
plot(pvout_f)

writeRaster(pvout_f, "pvout_masked.tif", "GTiff")


#########junk code that I worked too hard on to delete######
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

