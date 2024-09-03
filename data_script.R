# data_importing_and_cleaning scripts

# packages
library(sf)
library(ncdf4)
library(ggplot2)
library(stringr)
library(readr)
library(tidyverse)
library("rnaturalearth")


scale_0_1 = function(column_of_df){
  result = (column_of_df - min(column_of_df)) / (max(column_of_df) - min(column_of_df))
  return(result)
}



csv_from_nc_data = function(daily_nc_directory = "D:/CTM_daily_nc",
                            years_to_take = c("2010"),
                            months_to_take = c("01"),
                            area_csv_file_path = "C:/Users/71000/Downloads/NOx_Emulator-main/NOx_Emulator-main/area_data/area.csv",
                            only_land = TRUE){
  
  
  # daily_nc_directory = "D:/CTM_daily_nc"
  # years_to_take = c("2010")
  # months_to_take = c("01")
  # area_csv_file_path = "C:/Users/71000/Downloads/NOx_Emulator-main/NOx_Emulator-main/area_data/area.csv"
  # only_land = TRUE
  
  
  ncdf4_obj = nc_open(file.path(daily_nc_directory, years_to_take[1], paste0("gctm.omi.no2.", years_to_take[1], "0101.nc")))
  LON_column = rep(ncvar_get(ncdf4_obj, "LON"), ncdf4_obj$var[["LAT"]]$varsize)
  LAT_column = rep(ncvar_get(ncdf4_obj, "LAT"), each = ncdf4_obj$var[["LON"]]$varsize) 
  
  area_data = read_csv(area_csv_file_path)[,-1]$DXYP__
  
  
  
  # meterological_variables = c("DAO-FLDS__RADSWG", 
  #                             "DAO-FLDS__PBL", 
  #                             "DAO-FLDS__U10M",    
  #                             "DAO-FLDS__V10M", 
  #                             "DAO-3D-S__UWND",    
  #                             "DAO-3D-S__VWND",    
  #                             "DAO-3D-S__TMPU",
  #                             "DAO-3D-S__SPHU")
  
  sources_of_NOx = c("NOX-AC-S__NOx",     
                     "NOX-AN-S__NOx",     
                     "NOX-BIOB__NOx",     
                     "NOX-BIOF__NOx",  
                     "NOX-LI-S__NOx",     
                     "NOX-SOIL__NOx",     
                     "NOX-FERT__NOx",     
                     "NOX-STRT__NOx")
  
  num_cols = length(sources_of_NOx) + 9 # LAT, LON, DAY, MONTH, YEAR, Prior Estimates, CTM concentrations, Satellite concentrations, area
  
  training_matrix = matrix(ncol = num_cols, nrow = 1)
  colnames(training_matrix) = c("LON", "LAT", "Day", "Month", "Year", "CTM_Concentrations", "Satellite_Concentrations", "Prior_Estimates","Solar_Radiation", 
                                "PBL_Depth", "Surface_UWinds", "Surface_VWinds", "UWinds", "VWinds", "Temperature", "Specific_Humidity", "area")
  
  day = 0
  
  for (year in years_to_take){
    for (month in months_to_take){
      gctm_nc_files_to_take = Sys.glob(file.path(daily_nc_directory, year, paste0("gctm.omi.no2.", year, month, "*.nc")))
      for (gctm_nc_file in gctm_nc_files_to_take){
        # push the day forward
        day = day + 1
        
        # get the new satellite concentrations
        gctm_nc_obj = nc_open(gctm_nc_file)
        gctm_variable = ncvar_get(gctm_nc_obj, "IJ-AVG-S__NOx", count = c(-1,-1,2)) # retrieves the variable and its SURFACE data
        CTM_concentrations = c(gctm_variable[,,1])
        Satellite_concentrations = c(gctm_variable[,,2])
        
        # check which spots have more (valid) information for us
        filtering_vector = Satellite_concentrations > 0
        number_rows_added = sum(filtering_vector)
        
        if (number_rows_added > 0){
          
          additional_matrix = matrix(ncol = num_cols, nrow = number_rows_added)
          
          # get the corresponding CTM NC file
          date = strsplit(basename(gctm_nc_file), "[.]")[[1]][4]
          ctm_nc_file = file.path(daily_nc_directory, year, paste0("ctm.bpch.",date,".nc"))
          ctm_nc_obj = nc_open(ctm_nc_file)
          
          # get the prior estimates (total sums from all NOx sources)
          Prior_Estimates = rep(0, number_rows_added)
          for (NOx_source in sources_of_NOx){
            NOx_variable = c(ncvar_get(ctm_nc_obj, NOx_source, count = c(-1,-1,1))) # retrieves the variable and its SURFACE data
            Prior_Estimates = Prior_Estimates + NOx_variable[filtering_vector]
          }
          
          # get the rest of the meterological variables
          Solar_Radiation = c(ncvar_get(ctm_nc_obj, "DAO-FLDS__RADSWG", count = c(-1,-1,1))) # retrieves the variable and its SURFACE data
          PBL_Depth = c(ncvar_get(ctm_nc_obj, "DAO-FLDS__PBL", count = c(-1,-1,1))) # retrieves the variable and its SURFACE data
          Surface_UWinds = c(ncvar_get(ctm_nc_obj, "DAO-FLDS__U10M", count = c(-1,-1,1))) # retrieves the variable and its SURFACE data
          Surface_VWinds = c(ncvar_get(ctm_nc_obj, "DAO-FLDS__V10M", count = c(-1,-1,1))) # retrieves the variable and its SURFACE data
          UWinds = c(ncvar_get(ctm_nc_obj, "DAO-3D-S__UWND", count = c(-1,-1,1))) # retrieves the variable and its SURFACE data
          Vwinds = c(ncvar_get(ctm_nc_obj, "DAO-3D-S__VWND", count = c(-1,-1,1))) # retrieves the variable and its SURFACE data
          Temperature = c(ncvar_get(ctm_nc_obj, "DAO-3D-S__TMPU", count = c(-1,-1,1))) # retrieves the variable and its SURFACE data
          Specific_Humidity = c(ncvar_get(ctm_nc_obj, "DAO-3D-S__SPHU", count = c(-1,-1,1))) # retrieves the variable and its SURFACE data
          
          
          # Load everything into our training matrix 
          additional_matrix[,1] = LON_column[filtering_vector]
          additional_matrix[,2] = LAT_column[filtering_vector]
          additional_matrix[,3] = rep(day, number_rows_added)
          additional_matrix[,4] = rep(as.numeric(month), number_rows_added)
          additional_matrix[,5] = rep(as.numeric(year), number_rows_added)
          additional_matrix[,6] = CTM_concentrations[filtering_vector]
          additional_matrix[,7] = Satellite_concentrations[filtering_vector]
          additional_matrix[,8] = Prior_Estimates # this has already been filtered out
          additional_matrix[,9] = Solar_Radiation[filtering_vector]
          additional_matrix[,10] = PBL_Depth[filtering_vector]
          additional_matrix[,11] = Surface_UWinds[filtering_vector]
          additional_matrix[,12] = Surface_VWinds[filtering_vector]
          additional_matrix[,13] = UWinds[filtering_vector]
          additional_matrix[,14] = Vwinds[filtering_vector]
          additional_matrix[,15] = Temperature[filtering_vector]
          additional_matrix[,16] = Specific_Humidity[filtering_vector]
          additional_matrix[,17] = area_data[filtering_vector]
          
          training_matrix = rbind(training_matrix, additional_matrix)
          
        }
        
      }
    }
  }
  
  training_matrix = training_matrix[-1,] # take out the first NA column
  
  analysis_df = data.frame(training_matrix) %>% 
    mutate(days_in_month = case_when(Month == 1 ~ 31,
                                     Month == 2 ~ 28,
                                     Month == 3 ~ 31,
                                     Month == 4 ~ 30,
                                     Month == 5 ~ 31,
                                     Month == 6 ~ 30,
                                     Month == 7 ~ 31,
                                     Month == 8 ~ 31,
                                     Month == 9 ~ 30,
                                     Month == 10 ~ 31,
                                     Month == 11 ~ 30,
                                     Month == 12 ~ 31),
           Prior_Estimates = Prior_Estimates*area/6.023e23*14*10^4*days_in_month*24*3600*1e-12) %>% 
    select(-c(days_in_month, area))
  
  if (only_land){
    sf::sf_use_s2(FALSE)
    worldmap <- ne_countries(scale = "medium", returnclass = "sf")
    coordinate_array = matrix(c(analysis_df$LON, analysis_df$LAT), byrow = FALSE, ncol = 2)
    multipoint_object = st_multipoint(coordinate_array)
    multipoint_sfc_object = st_sfc(multipoint_object)
    many_point_geometries = st_cast(multipoint_sfc_object, "POINT")
    
    
    
    st_crs(many_point_geometries) = st_crs(worldmap) # use the same coordinate system that the world map uses
    myland = lengths(st_intersects(many_point_geometries,worldmap)) > 0 # check which coordinates are above land
    myland_named = ifelse(myland, "land", "sea") # check which coordinates are above land
    
    analysis_df = analysis_df %>% 
      filter(myland_named == "land")
  }
  
  
  
  training_df = analysis_df %>% 
    mutate(LON_transformed = LON - min(LON),
           LON_cos = cos(2*pi*LON_transformed/max(LON_transformed)),
           LON_sin = sin(2*pi*LON_transformed/max(LON_transformed)),
           LAT_transformed = LAT - min(LAT),
           LAT_cos = cos(2*pi*LAT_transformed/max(LAT_transformed)),
           LAT_sin = sin(2*pi*LAT_transformed/max(LAT_transformed)),
           Month_cos = cos(2*pi*Month/max(Month)),
           Month_sin = sin(2*pi*Month/max(Month))) %>% 
    select(-c(LON, LAT, Month, LON_transformed, LAT_transformed, Year))
  
  # training_df[,-c(4)] = scale(training_df[,-c(4)]) # old method of scaling
  
  
  
  
  
  training_df[,-c(4)] = apply(training_df[,-c(4)], MARGIN = 2, FUN = scale_0_1) # check if this works
  
  
  results = list(data = training_df, interpretable_data = analysis_df)
  return(results)
}
