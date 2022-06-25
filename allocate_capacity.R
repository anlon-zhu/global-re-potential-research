##import necessary packages
library(rgdal)
library(raster)
library(data.table)
library(maps)

setwd('C:/Users/user/OneDrive/Documents/RMP')

#create table of points on raster
capacity <- raster("capacity_masked.tif")
system.time(point <- rasterToPoints(capacity))
point <- as.data.table(point)

#load in country borders
countriesSP <- readOGR('Data/TM_WORLD_BORDERS-0.3')

# returns a vector (name) of the country that contains the x and y coordinates of a point
coords2country = function(points) {
  v <- map.where(database = countriesSP, namefield = "NAME", 
                 x = points$x, y = points$y)
  
  # Remove character after ':'
  gsub("\\:.*","",v)
}

#populate a table with an additional column that states which country each point is in, done in batches for monitoring progress
populate_country <- function(start, stop, batch_size) {
  end <- start + batch_size - 1
  while (start < stop & end <= stop) {
    countrynames <- coords2country(point[start:end, 1:2])
    point[start:end, country := countrynames]
    print(sprintf("Updated country column from row#%i to row#%i", start, end))
    
    start <- end + 1
    end <- start + batch_size - 1
    if (end > stop) {
      end <- stop
    }
  }
}

#Populate countries, calculate summed capacity outputs, and save results in CSV file.
run <- function(start, stop, batch_size) {
  populate_country(start, stop, batch_size)
  
  # Group by country and sum over capacity layer for each country
  res <- tapply(point$capacity_masked, point$country, sum)
  #res <- res * 3.45 #* 120 #times rated power, times 120 turbines per 1.2km^2 given impact area
  countries <- names(res)
  capacity <- sapply(countries, function(country) {res[[country]]})
  df <- data.frame(country = countries, capacity = capacity)
  sorted <- df[order(-capacity), ]
  
  # Write to CSV files
  write.csv(df, "wind_capacity_unsorted.csv", row.names = FALSE)
  write.csv(sorted, "wind_capacity_sorted.csv", row.names = FALSE)
}

print(paste("Started at", Sys.time()))
system.time(run(1, nrow(point), 1000000))
print(paste("Finished at", Sys.time()))



