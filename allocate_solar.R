##import necessary packages
library(rgdal)
library(raster)
library(data.table)
library(maps)

setwd('C:/Users/user/OneDrive/Documents/RMP')

#create table of points on raster
pvout <- raster("pvout_masked.tif")
system.time(point <- rasterToPoints(pvout))
point <- as.data.table(point)

#load in country borders
countriesSP <- readOGR('Data/TM_WORLD_BORDERS-0.3')

# returns a vector (name) of the country that contains the x and y coordinates of a point
coords2country = function(points) {
    v <- map.where(database = countriesSP, namefield = "NAME", 
                   x = points$x, y = points$y)
    
    # Remove ':' and characters after it
    gsub("\\:.*","",v)
}

#populate a table with an additional column that states which country each point is in, done in batches for monitoring progress
populate_country <- function(start, stop, batch_size) {
    end <- start + batch_size - 1
    while (start < stop & end <= stop) {
        countrynames <- coords2country(point[start:end, 1:2])
        point[start:end, country := countrynames]
        print(sprintf("%s: updated country column from row#%i to row#%i", 
                      Sys.time(), start, end))
        
        start <- end + 1
        end <- start + batch_size - 1
        if (end > stop) {
            end <- stop
        }
    }
}

#Populate countries, calculate summed PV outputs, and save results in CSV file.
run <- function(start, stop, batch_size) {
    populate_country(start, stop, batch_size)
    
    # Group by country and sum over PVOUT for each country
    res <- tapply(point$pvout_masked, point$country, sum)
    
    countries <- names(res)
    pvout <- sapply(countries, function(country) {res[[country]]})
    df <- data.frame(country = countries, pvout = pvout)
    sorted <- df[order(-pvout), ]
    
    # Write to CSV files
    write.csv(df, "solar_result_unsorted.csv", row.names = FALSE)
    write.csv(sorted, "solar_result_sorted.csv", row.names = FALSE)
}

print(sprintf("%s: start running...", Sys.time()))
system.time(run(1, nrow(point), 1000000))
print(sprintf("%s: finished", Sys.time()))
