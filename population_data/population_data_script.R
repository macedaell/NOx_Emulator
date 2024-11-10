# plot scripts
library(readr)
library(tidyverse)
library(ncdf4)

setwd("C:/Users/71000/Desktop/NOx_Emulator-main/")


# Script Description:
# - area.csv is the format: lon, lat, area (which is the variable). 
# - This is nice because the lat/lon is in the same resolution as the rest of the CTM variables
# - We want to incorporate the SEDAC variables of population count + density
# - These are NCDF4 files with latitude and longitude (good!) BUT they are in a different resolution
# - HENCE, this file must do the following:
#     - Transform the area.csv lat/lon into groups we will use for binning
#     - Import the SEDAC lat/lon and the variables pop density, pop count
#     - Bin the SEDAC lat/lon into those area.csv lat/lon groups we made earlier
#     - transform the lat/lon groups back into numeric variables
#     - save the pop density and pop count data as 2 separate csv's of the format: lat, lon, variable (just like area.csv)


# importing area data
area_data = read_csv("C:/Users/71000/Desktop/NOx_Emulator-main/area_data/area.csv")[,-1]#$DXYP__

# create the longitude groups... make sure the groups cover the entire globe
lon_groups = cut(area_data$lon, breaks = c(-Inf, unique(area_data$lon)))
levels(lon_groups)[levels(lon_groups)=="(175,178]"] <- "(175,180]"

# create the latitude groups... make sure the groups cover the entire globe
lat_groups = cut(area_data$lat, breaks = c(-Inf, unique(area_data$lat)))
levels(lat_groups)[levels(lat_groups)=="(88,89.5]"] <- "(88,90]"

# save the grouped versions of the coordinates
area_data_expanded = area_data %>% 
  mutate(LAT_column_grouped = lat_groups,
         LON_column_grouped = lon_groups)
# we have groups for each of the LON/LAT the CTM is defined over

# we bring in new population data from the SEDAC...

# import SEDAC lat/lon (more fine than the CTM)
population_density_ncdf4_obj = nc_open("./population_data/density/gpw_v4_population_density_rev11_1_deg.nc")
LON_column = rep(population_density_ncdf4_obj$dim$longitude$vals, length(population_density_ncdf4_obj$dim$latitude$vals))
LON_column[LON_column == -179.5] = -180
LAT_column = rep(population_density_ncdf4_obj$dim$latitude$vals, each = length(population_density_ncdf4_obj$dim$longitude$vals)) 

# import Population Density variable
population_density = c(ncvar_get(population_density_ncdf4_obj, "Population Density, v4.11 (2000, 2005, 2010, 2015, 2020): 1 degree")[,,3]) # retrieves the variable and its SURFACE data
population_density[is.na(population_density)] = 0
pop_density_df = as.data.frame(cbind(LON_column, LAT_column, population_density))

# import Population Total
population_count_ncdf4_obj = nc_open("./population_data/count/gpw_v4_population_count_rev11_1_deg.nc")
population_count = c(ncvar_get(population_count_ncdf4_obj, "Population Count, v4.11 (2000, 2005, 2010, 2015, 2020): 1 degree")[,,3]) # retrieves the variable and its SURFACE data
population_count[is.na(population_count)] = 0
pop_count_df = as.data.frame(cbind(LON_column, LAT_column, population_count))

# combine them into pop_df
pop_df = left_join(pop_density_df, pop_count_df) # has lat, lon, pop density, pop count

# now since the SEDAC resolution is more fine than the CTM, we need to be able 
# to identify which coordinates correspond to which CTM coordinates 
# (using the grouped variables I defined earlier)

# this will bin the pop_df lat/lon into the lat/lon groups of the CTM area variable
check_in_interval = function(lon_or_lat_vector, unique_grouping_vector){ # use this to convert lon OR lat into a grouped version, which we can merge with
  
  # unique_grouping_vector = unique(lon_groups)
  unique_grouping_vector = as.character(unique_grouping_vector)
  
  grouped_lon_or_lat_vector = character(length(lon_or_lat_vector))
  
  for (unique_grouping_scalar in unique_grouping_vector){
    
    lower = as.numeric(substr(strsplit(unique_grouping_scalar, ",")[[1]][1],2,nchar(strsplit(unique_grouping_scalar, ",")[[1]][1])))
    upper = as.numeric(substr(strsplit(unique_grouping_scalar, ",")[[1]][2],1,nchar(strsplit(unique_grouping_scalar, ",")[[1]][2])-1))
    
    grouped_lon_or_lat_vector[(lower < lon_or_lat_vector) & (lon_or_lat_vector <= upper)] = unique_grouping_scalar
    
    
  }
  
  return(grouped_lon_or_lat_vector)
  
}

# now create new columns for the population SEDAC data that identify which CTM gridpoints each location corresponds to
pop_df_summarized = pop_df %>% 
  mutate(LON_column_grouped = check_in_interval(LON_column,unique(area_data_expanded$LON_column_grouped)),
         LAT_column_grouped = check_in_interval(LAT_column,unique(area_data_expanded$LAT_column_grouped))) %>% 
  group_by(LON_column_grouped, LAT_column_grouped) %>% 
  summarize(avg_population_density = mean(population_density),  #let's average population density over grid points
            total_population_count = sum(population_count))     #let's sum population count over grid points

# we are finished grouping them, so ungroup
pop_df_summarized = pop_df_summarized %>% 
  ungroup()

# combine with the area dataset, since we now have common lat/lon groups
demo_combined = left_join(area_data_expanded, pop_df_summarized)


# now save the data. The SEDAC data is now in terms of the CTM resolution!
# we'll use the same format as the area data to make integration in data_script easier


avg_population_density_data = demo_combined %>% # take out unncessary variables
  select(c(lat,lon,avg_population_density))

write_csv(avg_population_density_data, "avg_population_density_data.csv") # save

total_population_count_data = demo_combined %>% # take out unncessary variables
  select(c(lat,lon,total_population_count))

write_csv(total_population_count_data, "total_population_count_data.csv") # save

# each of these are: lat, lon, variable... just like the original area.csv
