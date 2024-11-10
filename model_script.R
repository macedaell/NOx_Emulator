# Model Scripts


library(caret)


fit_BRF = function(training_df,calculate_importance = F){
  
  # Now, just fit the data
  BRF_mean_model = train(Prior_Estimates~., 
                         data = training_df,
                         method = "gbm", 
                         trControl = trainControl("cv", 5), 
                         tuneGrid = expand.grid(n.trees = 500,
                                                interaction.depth = 9,
                                                shrinkage = 0.05,
                                                n.minobsinnode = 4))
  
  
  if (calculate_importance){
    random_forest_model = train(Prior_Estimates~ ., 
                                data = training_df, 
                                method = "rf", 
                                trControl = trainControl("oob"), 
                                tuneGrid = expand.grid(mtry = 7))
    
    variable_importance_df = varImp(random_forest_model, scale = FALSE)
    save(variable_importance_df, file = "importance_metrics.RData")
    png("importance_plot.png", width = 700, height = 700, pointsize = 20) # can also use "bmp", "jpeg", "tiff", etc.
    print(plot(varImp(random_forest_model, scale = FALSE)))
    dev.off()
  }
  
  return(BRF_mean_model)
}



predict_BRF_with_Satellite = function(predictive_model, test_df, interpretable_test_df){
  new_data_to_predict = test_df %>% 
    mutate(NOx_concentrations = Satellite_Concentrations) %>% 
    select(-c(CTM_Concentrations,Satellite_Concentrations))
  ML_Estimates = predict(predictive_model, newdata = new_data_to_predict)
  ML_Estimates[ML_Estimates < 0] = 0
  results = cbind(interpretable_test_df, ML_Estimates)
  return(results)
}

predict_BRF_with_CTM = function(predictive_model, test_df, interpretable_test_df){
  new_data_to_predict = test_df %>% 
    mutate(NOx_concentrations = CTM_Concentrations) %>% 
    select(-c(CTM_Concentrations,Satellite_Concentrations))
  ML_Estimates = predict(predictive_model, newdata = new_data_to_predict)
  ML_Estimates[ML_Estimates < 0] = 0
  results = cbind(interpretable_test_df, ML_Estimates)
  return(results)
}


