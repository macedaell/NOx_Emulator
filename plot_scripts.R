# plot scripts
library(ggplot2)
library(viridis)
library(ggpointdensity)


convert_to_TgN_box = function(Prior, area){
  number_of_days = 1
  Prior = Prior*area/6.023e23*14*10^4*number_of_days*24*3600*1e-12
  return(Prior)
}

monthly_average_and_combine = function(intpretable_results_with_ML, paths_to_posterior_data, years){
  
  # intpretable_results_with_ML = results_dataset_using_CTM
  # paths_to_posterior_data = c("./cleaned_posterior_data/posterior_estimate_2010.csv",
  #                             "./cleaned_posterior_data/posterior_estimate_2011.csv")
  # years = c(2010,2011)
  
  summarized_test_results = intpretable_results_with_ML %>% 
    mutate(converted_Prior_Estimates = convert_to_TgN_box(Prior_Estimates, area),
           converted_ML_Estimates = convert_to_TgN_box(ML_Estimates, area),
           converted_residuals = convert_to_TgN_box(residuals, area))

  summarized_test_results = summarized_test_results %>% 
    group_by(Year, Month, LAT, LON) %>% 
    summarize(monthly_CTM_Concentrations = mean(CTM_Concentrations),# covariates
              monthly_Satellite_Concentrations = mean(Satellite_Concentrations),
              monthly_Solar_Radiation = mean(Solar_Radiation),
              monthly_PBL_Depth = mean(PBL_Depth),
              monthly_Surface_UWinds = mean(Surface_UWinds),
              monthly_Surface_VWinds = mean(Surface_VWinds),
              monthly_UWinds = mean(UWinds),
              monthly_VWinds = mean(VWinds),
              monthly_Temperature = mean(Temperature),
              monthly_Specific_Humidity = mean(Specific_Humidity),
              monthly_avg_pop_density = mean(avg_pop_density),
              monthly_total_pop_count = mean(total_pop_count),
              count = n(), # info for converting estimates + Prior into Posterior Units 
              added_Prior_Estimates = sum(converted_Prior_Estimates),
              added_ML_Estimates = sum(converted_ML_Estimates),
              added_residuals = sum(converted_residuals))
  
  # sum(duplicated(summarized_test_results)) # no duplicates! Nice
  
  summarized_test_results = summarized_test_results %>% 
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
           imputed_Prior_Estimates = (days_in_month/count)*added_Prior_Estimates, # impute by scaling up 
           imputed_ML_Estimates = (days_in_month/count)*added_ML_Estimates,       # (this is the same as imputing missing values by mean, then adding)
           imputed_residuals = (days_in_month/count)*added_residuals) # %>% 
    # select(-c(count, added_Prior_Estimates, added_ML_Estimates, added_residuals, days_in_month))
  
  all_posterior_data = data.frame(matrix(ncol = ncol(summarized_test_results), nrow = 0))
  colnames(all_posterior_data) <- colnames(summarized_test_results)
  
  for (i in 1:length(paths_to_posterior_data)){
    posterior_data = read_csv(paste0(paths_to_posterior_data[i]))[,-1] %>% 
      rename(Month = month) %>% 
      mutate(Year = years[i])
    all_posterior_data = rbind(all_posterior_data, posterior_data)
  }
  
  combined_data = left_join(summarized_test_results, all_posterior_data)
  combined_data$Post_ML_diff = combined_data$Posterior_Estimates - combined_data$imputed_ML_Estimates
  combined_data$Post_Prior_diff = combined_data$Posterior_Estimates - combined_data$imputed_Prior_Estimates
  combined_data$Prior_ML_diff = combined_data$imputed_Prior_Estimates - combined_data$imputed_ML_Estimates
  
  return(combined_data)
}

# takes out the residuals, we can do that ourselves!
# also accounts for the new GMAO variables
monthly_average_and_combine_v2 = function(intpretable_results_with_ML, paths_to_posterior_data, years){

  # intpretable_results_with_ML = results_dataset_using_CTM
  # 
  # paths_to_posterior_data = c("./cleaned_posterior_data/posterior_estimate_2010.csv", 
  #                             "./cleaned_posterior_data/posterior_estimate_2011.csv",
  #                             "./cleaned_posterior_data/posterior_estimate_2012.csv")
  # 
  # years = c(2010, 2011, 2012)
  
  summarized_test_results = intpretable_results_with_ML %>% 
    mutate(converted_Prior_Estimates = convert_to_TgN_box(Prior_Estimates, area),
           converted_ML_Estimates = convert_to_TgN_box(ML_Estimates, area))
  
  summarized_test_results = summarized_test_results %>% 
    group_by(Year, Month, LAT, LON) %>% 
    summarize(monthly_CTM_Concentrations = mean(CTM_Concentrations),# covariates
              monthly_Satellite_Concentrations = mean(Satellite_Concentrations),
              

              
              monthly_HFLUX = mean(HFLUX),
              monthly_RADSWG = mean(RADSWG),
              monthly_PREACC = mean(PREACC),
              monthly_PRECON = mean(PRECON),
              monthly_TS = mean(TS),
              monthly_USTAR = mean(USTAR),
              monthly_Z0 = mean(Z0),
              monthly_PBL = mean(PBL),
              monthly_CLDFRC = mean(CLDFRC),
              monthly_U10M = mean(U10M),
              monthly_V10M = mean(V10M),
              monthly_PS_PBL = mean(PS_PBL),
              monthly_ALBD = mean(ALBD),
              monthly_PHIS = mean(PHIS),
              monthly_CLDTOP = mean(CLDTOP),
              monthly_TROPP = mean(TROPP),
              monthly_SLP = mean(SLP),
              monthly_TSKIN = mean(TSKIN),
              monthly_PARDF = mean(PARDF),
              monthly_PARDR = mean(PARDR),
              monthly_GWET = mean(GWET),
              monthly_UWND = mean(UWND),
              monthly_VWND = mean(VWND),
              monthly_TMPU = mean(TMPU),
              monthly_SPHU = mean(SPHU),

              
              
              
              monthly_avg_pop_density = mean(avg_pop_density),
              monthly_total_pop_count = mean(total_pop_count),
              count = n(), # info for converting estimates + Prior into Posterior Units 
              added_Prior_Estimates = sum(converted_Prior_Estimates),
              added_ML_Estimates = sum(converted_ML_Estimates))
  
  # sum(duplicated(summarized_test_results)) # no duplicates! Nice
  
  summarized_test_results = summarized_test_results %>% 
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
           imputed_Prior_Estimates = (days_in_month/count)*added_Prior_Estimates, # impute by scaling up 
           imputed_ML_Estimates = (days_in_month/count)*added_ML_Estimates) # %>% # same as imputing by mean
  # select(-c(count, added_Prior_Estimates, added_ML_Estimates, added_residuals, days_in_month))
  
  all_posterior_data = data.frame(matrix(ncol = ncol(summarized_test_results), nrow = 0))
  colnames(all_posterior_data) <- colnames(summarized_test_results)
  
  for (i in 1:length(paths_to_posterior_data)){
    posterior_data = read_csv(paste0(paths_to_posterior_data[i]))[,-1] %>% 
      rename(Month = month) %>% 
      mutate(Year = years[i])
    all_posterior_data = rbind(all_posterior_data, posterior_data)
  }
  
  combined_data = left_join(summarized_test_results, all_posterior_data)
  
  
  return(combined_data)
}

# have this find all the variables automatically and aggregate them appropriately
monthly_average_and_combine_v3 = function(){
  return(NULL)
}



var_cor_plot = function(x,y,color_vector = NULL, alpha_val = 0.2){
  
  if (is.null(color_vector)){
    plot(x, y, col = alpha("black", alpha = alpha_val),
         xlab = as.character(substitute(x)), 
         ylab = as.character(substitute(y)))
    abline(0,1)
  }else{
    plot(x, y, col = alpha(color_vector, alpha = alpha_val),
         xlab = as.character(substitute(x)), 
         ylab = as.character(substitute(y)))
    abline(0,1)
  }
}



est_cor_plot = function(x,y, adjust = 1, line_size = 1, subset = length(x), log = FALSE){
  
  R = round(cor(x, y), 2)
  NMB = round(sum(x - y)/sum(y), 2)
  NMSE = signif(mean((x-y)^2)/(mean(x)*mean(y))*100.0, 3)
  
  picks = sample(1:length(x), subset)
  
  x_label = as.character(substitute(x))
  y_label = as.character(substitute(y))
  
  x = x[picks]
  y = y[picks]
  
  if(log){x = log(x); y = log(y)}
  
  
  ggplot(mapping = aes(x = x,y = y)) + 
    geom_pointdensity(adjust = adjust) + 
    geom_abline(slope = 1, intercept = 0, size = line_size) +
    xlab(x_label) + 
    ylab(y_label) + 
    ggtitle(paste0("R=", R, ", NMB=", NMB, ", NMSE=", NMSE)) + 
    scale_color_viridis()
  
}

