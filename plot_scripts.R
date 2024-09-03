# plot scripts



monthly_average_and_combine = function(intpretable_results_with_ML, path_to_posterior_data){
  
  
  summarized_test_results = intpretable_results_with_ML %>% 
    group_by(Month, LON, LAT) %>% 
    summarize(ML_Monthly_Averages = mean(ML_Estimates),
              Prior_Monthly_Averages = mean(Prior_Estimates),
              residuals_Monthly_Averages = mean(residuals),
              count = n())                                                                                                            
  posterior_data = read_csv(paste0(path_to_posterior_data))[,-1] %>% 
    rename(Month = month)
  
  combined_data = left_join(summarized_test_results, posterior_data)
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

