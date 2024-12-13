# plot scripts
library(ggplot2)
library(viridis)
library(ggpointdensity)

# also accounts for the new GMAO variables
monthly_average_and_combine = function(intpretable_results_with_ML, paths_to_posterior_data, years){
  
  convert_to_TgN_box = function(Prior, area){
    number_of_days = 1
    Prior = Prior*area/6.023e23*14*10^4*number_of_days*24*3600*1e-12
    return(Prior)
  }
  
  TgN_box_results = intpretable_results_with_ML %>% 
    mutate(converted_Prior_Estimates = convert_to_TgN_box(Prior_Estimates, area),
           converted_ML_Estimates = convert_to_TgN_box(ML_Estimates, area)) %>% 
    select(c(Year, Month, LAT, LON, converted_Prior_Estimates, converted_ML_Estimates))
  
  aggregate_TgN_box_results = TgN_box_results %>% 
    group_by(Year, Month, LAT, LON) %>% 
    summarize(added_Prior_Estimates = sum(converted_Prior_Estimates),
              added_ML_Estimates = sum(converted_ML_Estimates),
              count = n())
  
  imputed_TgN_box_results = aggregate_TgN_box_results %>% 
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
           imputed_Prior_Estimates = (days_in_month/count)*added_Prior_Estimates,
           imputed_ML_Estimates = (days_in_month/count)*added_ML_Estimates) %>% 
    select(-c(added_Prior_Estimates, added_ML_Estimates, count, days_in_month))
  
  imputed_predictor_results = intpretable_results_with_ML %>% 
    group_by(Year, Month, LAT, LON) %>% 
    summarize_all(mean) %>% 
    select(-c(Prior_Estimates, ML_Estimates, Day, area))
  

  summarized_results = merge(imputed_TgN_box_results, imputed_predictor_results)
  
  
  all_posterior_data = data.frame(matrix(ncol = ncol(summarized_results), nrow = 0))
  colnames(all_posterior_data) <- colnames(summarized_results)
  
  for (i in 1:length(paths_to_posterior_data)){
    posterior_data = read_csv(paste0(paths_to_posterior_data[i]))[,-1] %>% 
      rename(Month = month) %>% 
      mutate(Year = years[i])
    all_posterior_data = rbind(all_posterior_data, posterior_data)
  }
  
  combined_data = left_join(summarized_results, all_posterior_data)
  
  
  return(combined_data)
}




