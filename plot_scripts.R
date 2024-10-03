# plot scripts



monthly_average_and_combine = function(intpretable_results_with_ML, paths_to_posterior_data, years){
  
  
  summarized_test_results = intpretable_results_with_ML %>% 
    group_by(Year, Month, LON, LAT) %>% 
    summarize(ML_Monthly_Averages = mean(ML_Estimates),
              Prior_Monthly_Averages = mean(Prior_Estimates),
              residuals_Monthly_Averages = mean(residuals),
              pop_density = mean(avg_pop_density), 
              pop_count = mean(total_pop_count),
              count = n())                                                                                                            
  
  all_posterior_data = data.frame(matrix(ncol = ncol(summarized_test_results), nrow = 0))
  colnames(all_posterior_data) <- colnames(summarized_test_results)
  
  for (i in 1:length(paths_to_posterior_data)){
    posterior_data = read_csv(paste0(paths_to_posterior_data[i]))[,-1] %>% 
      rename(Month = month) %>% 
      mutate(Year = years[i])
    all_posterior_data = rbind(all_posterior_data, posterior_data)
  }
  
  combined_data = left_join(summarized_test_results, all_posterior_data)
  combined_data$estimation_diff = combined_data$Posterior_Estimates - combined_data$ML_Monthly_Averages
  
  return(combined_data)
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



est_cor_plot = function(x,y,color_vector = NULL, alpha_val = 0.2){
  
  R = round(cor(x, y), 2)
  NMB = round(sum(x - y)/sum(y), 2)
  NMSE = signif(mean((x-y)^2)/(mean(x)*mean(y))*100.0, 3)
  
  
  if (is.null(color_vector)){
    plot(x, y, col = alpha("black", alpha = alpha_val),
         main=paste0("\n \n R=", R, ", NMB=", NMB, ", NMSE=", NMSE), 
         xlab = as.character(substitute(x)), 
         ylab = as.character(substitute(y)))
    abline(0,1)
  }else{
    plot(x, y, col = alpha(color_vector, alpha = alpha_val),
         main=paste0("\n \n R=", R, ", NMB=", NMB, ", NMSE=", NMSE), 
         xlab = as.character(substitute(x)), 
         ylab = as.character(substitute(y)))
    abline(0,1)
  }
  
}

