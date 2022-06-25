setwd('C:/Users/user/OneDrive/Documents/RMP')

##import necessary packages
library(ggplot2)
library(broom)
library(countrycode)
library(tidyverse)
library(data.table)
library(rgdal)
library(readxl)

#area roof = population * Afloor/capita * roof:floor ratio * roof_suitability * facade:floor ratio * facade_suitability

ref_floor_area <- read.csv("Data/reference_countries_floorarea.csv") %>% as.data.table()

ref_roof_floor <- read.csv("Data/reference_countries_roof_to_floor.csv") %>% as.data.table()

floor_area_pcapita <- read.csv("Data/floorarea_per_capita.csv") %>% as.data.table()
floor_area_pcapita <- select(floor_area_pcapita, "X", "X2030.residential", "X2030.Non.Residential") %>% setnames(c("X2030.residential", "X2030.Non.Residential"), c("res_floor_area", "nonres_floor_area"))

floor_ratios_res <- read.csv("Data/floor_ratios_residential.csv") %>% as.data.table()
floor_ratios_nonres <- read.csv("Data/floor_ratios_nonresidential.csv") %>% as.data.table()

#focus on 2030
#df framework
#c("country", "population", "floor_area_ref", "ratio_ref", "res_floor_area", "nonres_floor_area", 
#  "res_roof_ratio", "nonres_roof_ratio", "res_facade_N", "res_facade_S", "res_facade_E", "res_facade_W", "nonres_facade_N", "nonres_facade_S", "nonres_facade_E", "nonres_facade_W")

#import populations, codes
pop_df <- read_xlsx('Data/WPP2019_POP.xlsx', sheet = 2)
df <- pop_df[, c("Region, subregion, country or area *", "2030", "Country code")] %>% as.data.table() %>% 
  setnames(old = c("Region, subregion, country or area *", "2030", "Country code"), new = c("Country", "Population", "iso3n"))

df[, iso2c := countrycode(pop_df[["Country code"]], "iso3n", "iso2c")]

#join floor area reference countries
s <- strsplit(ref_floor_area$iso, split = "\\, |\\,| ")
rf <- data.frame(ref_area = rep(ref_floor_area$ref.country, sapply(s, length)), iso = unlist(s))

df <- left_join(df, rf, by = c("iso2c" = "iso"))

#join floor to roof/facade ratio reference countries
s <- strsplit(ref_roof_floor$iso, split = "\\, |\\,| ")
rf <- data.frame(ref_ratio = rep(ref_roof_floor$ref.country, sapply(s, length)), iso = unlist(s))

df <- left_join(df, rf, by = c("iso2c" = "iso"))

#clean workspace
rm(s, rf)

#join floor area per capita for residential and nonresidential buildings, based on reference countries
df <- left_join(df, floor_area_pcapita, by = c("ref_area" = "X")) 

#join floor to roof/facade ratios for residential buildings, based on reference countries
df <- left_join(df, floor_ratios_res, by = c("ref_ratio" = "reference")) %>% 
  setnames(c("residential.roof","residential.facade.N", "residential.facade.S", "residential.facade.E", "residential.facade.W" ),
           c("res_roof_ratio", "res_facade_N", "res_facade_S", "res_facade_E", "res_facade_W"))


#join floor to roof/facade ratios for nonresidential buildings, based on reference countries
df <- left_join(df, floor_ratios_nonres, by = c("ref_ratio" = "reference")) %>% 
  setnames(c("nonresidential.roof","nonresidential.facade.N", "nonresidential.facade.S", "nonresidential.facade.E", "nonresidential.facade.W" ),
           c("nonres_roof_ratio", "nonres_facade_N", "nonres_facade_S", "nonres_facade_E", "nonres_facade_W"))

#calculate area available
df[, lapply(.SD, as.numeric), .SDcols = c("Population", "res_floor_area", "nonres_floor_area", "res_roof_ratio", "nonres_roof_ratio", "res_facade_N", "res_facade_S", "res_facade_E", "res_facade_W", "nonres_facade_N", "nonres_facade_S", "nonres_facade_E", "nonres_facade_W")]

#area given in meters squared
#33% suitability roofs, 10% facades
df[, suitable_area_res := Population * res_floor_area * (res_roof_ratio * 0.33 + 0.1 * (res_facade_N + res_facade_S + res_facade_E + res_facade_W))]
df[, suitable_area_nonres := Population * nonres_floor_area * (nonres_roof_ratio * 0.33 + 0.1 * (nonres_facade_N + nonres_facade_S + nonres_facade_E + nonres_facade_W))]
#total suitalbe area in km^2 by summing residential and nonresidential areas
df[, total_suitable_area_km2 := (suitable_area_res + suitable_area_nonres) * 10**-6]


#find average MWh output of solar panels per km^2 in each country
pvout_unmasked <- read.csv("Data/Unmasked_PVOUT_summary.csv") %>% as.data.table

#load areas
countriesSP <- readOGR('Data/TM_WORLD_BORDERS-0.3')
areas <- countriesSP@data[["AREA"]]
names(areas) <- countriesSP@data[["NAME"]]
#values are all multiplied by 0.1 (in km^2) for some reason, so correct
areas <- areas * 10
rm(countriesSP)

#join areas to pvout
pvout_unmasked[country %in% names(areas), area := areas[country]]
pvout_unmasked[, average_MWh_per_km2 := MWh_output/area]
pvout_unmasked <- select(pvout_unmasked, c("country", "average_MWh_per_km2"))
pvout_unmasked$iso3n <- countrycode(sourcevar = pvout_unmasked[["country"]],
                                       origin = "country.name",
                                       destination = "iso3n")

df <- left_join(df, pvout_unmasked, by = "iso3n")

#multiply average output by roof area for PV potential of rooftops
df <- df[, roof_output_MWh := average_MWh_per_km2 * total_suitable_area_km2]

write.csv(df, file = "rooftop_potential.csv")

#check if large countries are missing values
df[is.na(roof_output_MWh)]
