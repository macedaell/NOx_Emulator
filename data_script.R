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

scale_checking = function(column_of_df){ # only used for debugging
  print(min(column_of_df))
  print(max(column_of_df))
  return(column_of_df)
}



# have this use any inputted predictors
csv_from_nc_data_v3 = function(daily_nc_directory = "D:/CTM_daily_nc",
                               years_to_take = c("2010", "2011"),
                               months_to_take = c("01", "02", "03", "04", "05","06", "07", "08", "09","10","11","12"),
                               lat_locations_to_take = c(-90, 90),
                               lon_locations_to_take = c(-180, 180),
                               additional_predictor_variables = c(), # choices: c(), c("all"), c("original"), c("thing1", "thing2", "thing3")
                               sources_of_NOx = c("NOX-AC-S__NOx",   # must have at least one source of NOx 
                                                  "NOX-AN-S__NOx",     
                                                  "NOX-BIOB__NOx",     
                                                  "NOX-BIOF__NOx",  
                                                  "NOX-LI-S__NOx",     
                                                  "NOX-SOIL__NOx",     
                                                  "NOX-FERT__NOx",     
                                                  "NOX-STRT__NOx"),
                               filter_negative_satellite_conc = TRUE,
                               area_csv_file_path = "C:/Users/71000/Desktop/NOx_Emulator-main/area_data/area.csv",
                               use_pop_vars = TRUE,
                               avg_population_density_csv_file_path = "C:/Users/71000/Desktop/NOx_Emulator-main/population_data/density/avg_population_density_data.csv",
                               total_population_count_csv_file_path = "C:/Users/71000/Desktop/NOx_Emulator-main/population_data/count/total_population_count_data.csv",
                               only_land = TRUE,
                               show_locations = TRUE){
  
  if(!is.null(additional_predictor_variables)){if(additional_predictor_variables[1]=="all"){additional_predictor_variables = c("DAO-FLDS__HFLUX",
                                                                                                                               "DAO-FLDS__RADSWG",
                                                                                                                               "DAO-FLDS__PREACC",
                                                                                                                               "DAO-FLDS__PRECON",
                                                                                                                               "DAO-FLDS__TS",
                                                                                                                               "DAO-FLDS__RADSWT",
                                                                                                                               "DAO-FLDS__USTAR",
                                                                                                                               "DAO-FLDS__Z0",
                                                                                                                               "DAO-FLDS__PBL",
                                                                                                                               "DAO-FLDS__CLDFRC",
                                                                                                                               "DAO-FLDS__U10M",
                                                                                                                               "DAO-FLDS__V10M",
                                                                                                                               "DAO-FLDS__PS-PBL",
                                                                                                                               "DAO-FLDS__ALBD",
                                                                                                                               "DAO-FLDS__PHIS",
                                                                                                                               "DAO-FLDS__CLDTOP",
                                                                                                                               "DAO-FLDS__TROPP",
                                                                                                                               "DAO-FLDS__SLP",
                                                                                                                               "DAO-FLDS__TSKIN",
                                                                                                                               "DAO-FLDS__PARDF",
                                                                                                                               "DAO-FLDS__PARDR",
                                                                                                                               "DAO-FLDS__GWET",
                                                                                                                               "DAO-3D-S__UWND",
                                                                                                                               "DAO-3D-S__VWND",
                                                                                                                               "DAO-3D-S__TMPU",
                                                                                                                               "DAO-3D-S__SPHU",
                                                                                                                               "DAO-3D-S__CLDMAS",
                                                                                                                               "DAO-3D-S__DTRAIN", additional_predictor_variables[-1])}
    else if(additional_predictor_variables[1]=="original"){additional_predictor_variables = c("DAO-FLDS__RADSWG",
                                                                                              "DAO-FLDS__PBL",
                                                                                              "DAO-FLDS__U10M",
                                                                                              "DAO-FLDS__V10M",
                                                                                              "DAO-3D-S__UWND",
                                                                                              "DAO-3D-S__VWND",
                                                                                              "DAO-3D-S__TMPU",
                                                                                              "DAO-3D-S__SPHU", additional_predictor_variables[-1])}}
  
  
  
  
  # # to debug
  # daily_nc_directory = "D:/CTM_daily_nc"
  # years_to_take = c("2010", "2011")
  # months_to_take = c("01", "02", "03", "04", "05","06", "07", "08", "09","10","11","12")
  # area_csv_file_path = "C:/Users/71000/Desktop/NOx_Emulator-main/area_data/area.csv"
  # avg_population_density_csv_file_path = "C:/Users/71000/Desktop/NOx_Emulator-main/population_data/density/avg_population_density_data.csv"
  # total_population_count_csv_file_path = "C:/Users/71000/Desktop/NOx_Emulator-main/population_data/count/total_population_count_data.csv"
  # only_land = TRUE
  
  ncdf4_obj = nc_open(file.path(daily_nc_directory, years_to_take[1], paste0("gctm.omi.no2.", years_to_take[1], "0101.nc")))
  LON_column = rep(ncvar_get(ncdf4_obj, "LON"), ncdf4_obj$var[["LAT"]]$varsize)
  LAT_column = rep(ncvar_get(ncdf4_obj, "LAT"), each = ncdf4_obj$var[["LON"]]$varsize) 
  nc_close(ncdf4_obj)

  area_data = read_csv(area_csv_file_path)[,-1]$DXYP__
  
  num_necessary_variables = 9
  necessary_var_names = c("LON", "LAT", "Day", "Month", "Year", "CTM_Concentrations", "Satellite_Concentrations", "Prior_Estimates", "area")
  
  
  if (use_pop_vars){
    avg_population_density_data = read_csv(avg_population_density_csv_file_path)$avg_population_density
    total_population_count_data = read_csv(total_population_count_csv_file_path)$total_population_count
    num_pop_vars = 2
    pop_var_names = c("avg_pop_density", "total_pop_count")
  } else {num_pop_vars = 0; pop_var_names = c()}
  
  num_cols = num_necessary_variables + num_pop_vars + length(additional_predictor_variables)
  col_names = c(necessary_var_names, pop_var_names, additional_predictor_variables)
  
  training_matrix = matrix(ncol = num_cols, nrow = 1)
  colnames(training_matrix) = gsub("-", "_", col_names) # take out hyphen for potential problems later on
  
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
        nc_close(gctm_nc_obj)
        
        # check which spots have more (valid) information for us -- positive satellite concentrations
        filtering_vector = (lat_locations_to_take[1] <= LAT_column)&(LAT_column <= lat_locations_to_take[2])&(lon_locations_to_take[1] <= LON_column)&(LON_column <= lon_locations_to_take[2])
        if (filter_negative_satellite_conc){filtering_vector = filtering_vector&(Satellite_concentrations > 0)}
        number_rows_added = sum(filtering_vector)
        
        if (number_rows_added > 0){
          
          additional_datapoints = matrix(ncol = num_cols, nrow = number_rows_added)
          
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
          
          # Load base variables into our additional datapoint matrix 
          additional_datapoints[,1] = LON_column[filtering_vector]
          additional_datapoints[,2] = LAT_column[filtering_vector]
          additional_datapoints[,3] = rep(day, number_rows_added)
          additional_datapoints[,4] = rep(as.numeric(month), number_rows_added)
          additional_datapoints[,5] = rep(as.numeric(year), number_rows_added)
          additional_datapoints[,6] = CTM_concentrations[filtering_vector]
          additional_datapoints[,7] = Satellite_concentrations[filtering_vector]
          additional_datapoints[,8] = Prior_Estimates # this has already been filtered out
          additional_datapoints[,9] = area_data[filtering_vector]
          
          # Load population variables into our additional datapoint matrix, if desired 
          if(use_pop_vars){
            additional_datapoints[,10] = avg_population_density_data[filtering_vector]
            additional_datapoints[,11] = total_population_count_data[filtering_vector]
            next_index = 12
          } else {
            next_index = 10
          }
          
          # Load additional predictors into our additional datapoint matrix, if desired 
          for (add_var in additional_predictor_variables){
            additional_datapoints[,next_index] = c(ncvar_get(ctm_nc_obj, add_var, count = c(-1,-1,1)))[filtering_vector]
            next_index = next_index + 1
          }
          nc_close(ctm_nc_obj)
          
          # fill out the next row, and continue!
          training_matrix = rbind(training_matrix, additional_datapoints)
        } # new matrix rows
      } # new day
    } # new month
  } # new year
  
  training_matrix = training_matrix[-1,] # take out the first NA row (used to start the concatenation loop)
  
  # take OUT the conversions
  # analysis_df = data.frame(training_matrix) %>% 
  #   mutate(days_in_month = case_when(Month == 1 ~ 31,
  #                                    Month == 2 ~ 28,
  #                                    Month == 3 ~ 31,
  #                                    Month == 4 ~ 30,
  #                                    Month == 5 ~ 31,
  #                                    Month == 6 ~ 30,
  #                                    Month == 7 ~ 31,
  #                                    Month == 8 ~ 31,
  #                                    Month == 9 ~ 30,
  #                                    Month == 10 ~ 31,
  #                                    Month == 11 ~ 30,
  #                                    Month == 12 ~ 31),
  #          Prior_Estimates = Prior_Estimates*area/6.023e23*14*10^4*days_in_month*24*3600*1e-12) %>% 
  #   select(-c(days_in_month, area))
  
  
  # [TgN/box] = [molec/cm2/s] * area / av * 14 * 10^(4) * days * 24 * 3600 * 1e-12
  # so Prior_Estimates are molec/cm2/s
  
  # training_matrix = training_matrix[ , apply(training_matrix, 2, function(x) !any(is.nan(x)))]
  training_matrix = training_matrix[ , apply(training_matrix, 2, function(x) var(x)!=0)]
  
  analysis_df = data.frame(training_matrix)
  
  # filter out the locations that aren't on land... looking forward: maybe do the same with population density?
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
      filter(myland_named == "land") # filter now
    
  }
  
  if(show_locations){
    
    if (!only_land){
      sf::sf_use_s2(FALSE)
      worldmap <- ne_countries(scale = "medium", returnclass = "sf")
    }
    # # map of "high_ratio_all == 1"
    # data_set_to_test = combined_AQS_sep_model %>%
    #   filter(high_ratio_all == 1)
    # coordinate_array = matrix(c(data_set_to_test$Longitude, data_set_to_test$Latitude), byrow = FALSE, ncol = 2)
    # multipoint_object = st_multipoint(coordinate_array)
    # multipoint_sfc_object = st_sfc(multipoint_object)
    # many_point_geometries = st_cast(multipoint_sfc_object, "POINT")
    # st_crs(many_point_geometries) = st_crs(worldmap) # use the same coordinate system that the world map uses
    # myland = lengths(st_intersects(many_point_geometries,worldmap)) > 0 # check which coordinates are above land
    # myland = data_set_to_test$high_ratio_tp == 1 # any kind of logical statement
    # myland_color = ifelse(myland, "red", "black") # check which coordinates are above land
    # plot(worldmap$geometry)
    # plot(many_point_geometries, pch=16, col=myland_color, add=TRUE, cex = 0.7);
    # title("locations of high ratios biomass burned. Trouble points in Red")
    # View(data_set_to_test)
    # data_set_to_test$Coordinates[data_set_to_test$high_ratio_all == 1]
    
    # map of analysis_df
    data_set_to_test = analysis_df
    coordinate_array = unique(matrix(c(analysis_df$LON, analysis_df$LAT), byrow = FALSE, ncol = 2)) # find a way to delete duplicates
    multipoint_object = st_multipoint(coordinate_array)
    multipoint_sfc_object = st_sfc(multipoint_object)
    many_point_geometries = st_cast(multipoint_sfc_object, "POINT")
    st_crs(many_point_geometries) = st_crs(worldmap) # use the same coordinate system that the world map uses
    # myland = lengths(st_intersects(many_point_geometries,worldmap)) > 0 # check which coordinates are above land
    # myland = data_set_to_test$high_ratio_tp == 1 # any kind of logical statement
    # myland_color = ifelse(myland, "red", "black") # check which coordinates are above land
    plot(worldmap$geometry)
    plot(many_point_geometries, pch=16, col="black", add=TRUE, cex = 0.7)
    title(paste("Locations we are currently using. Using SeaPoints =", !only_land))
    
  }
  
  
  # codify for oscillatory variables (location and month)
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
  
  
  # standardize everything to be between 0 and 1, except prior_estimates, which we will train against
  training_df[,-c(4)] = apply(training_df[,-c(4)], MARGIN = 2, FUN = scale_0_1) 
  # apply(training_df[,-c(4)], MARGIN = 2, FUN = scale_checking) # to check if scale_0_1 worked
  
  # save both versions: the codified/scaled version for training, and the more interpretable version
  results = list(data = training_df, interpretable_data = analysis_df) 
  return(results)
}






