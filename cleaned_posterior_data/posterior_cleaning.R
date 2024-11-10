
# PART 2a: CLEANING THE MONTHLY DATA 
# INPUT: CTM DATA @ "./part1Output_Python_CTM_data"
#        CEDS INVENTORY DATA @ "./part0Python_data/EDGAR_2010_data.csv"
#        EDGAR INVENTORY DATA @ "./part0Python_data/inv_CEDS_2010.csv"
#        PRESSURE DATA @ "./part0Python_data/pressure.csv" (UNUSED)
# OUTPUT: TIDYVERSE DATAFRAMES @ "./part2aOutput_cleaned_tidyverse_data"
#         TOP-DOWN EMISSION VECTORS @ "./part2aOutput_cleaned_tidyverse_data"

####################### IMPORTING THE DATA #####################################

# import the packages
library(readr) # for reading in the csv
library(tidyverse) # for mutate and rename
library(data.table) # for data table and rbindlist


# all the data (year,month) from the csv files we need to read in
years = c("2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015", "2016")
months = c("01","02","03","04","05","06","07","08","09","10","11","12")

#IMPORT THE MONTHLY CSV FILES AND APPEND THE MONTHS SO EACH YEAR IS ITS OWN CSV
for (year in years){
  # read in this year's Jan
  posterior_estimate = as.data.table(read_csv(paste0("D:/posterior_monthly_csv/posterior_",year,"01.csv")) %>%
                                       mutate(month = as.character(as.numeric(months[1]))) %>% 
                                       rename(Posterior_Estimates = monthly_mean_emissions)) # number month as "1"
  
  # From Feb to Dec...
  for (month in months[-1]){
    # import the next month
    next_month = as.data.table(read_csv(paste0("D:/posterior_monthly_csv/posterior_",year,month,".csv")) %>%
                                 mutate(month = as.character(as.numeric(month))) %>% 
                                 rename(Posterior_Estimates = monthly_mean_emissions)) # number the month
    
    # append the month
    posterior_estimate = rbindlist(list(posterior_estimate, next_month))
  }
  
  # for each year, save as a csv
  write_csv(posterior_estimate, file = paste0("C:/Users/71000/Desktop/NOx_Emulator-main/cleaned_posterior_data/posterior_estimate_", year,".csv"))
  
}





