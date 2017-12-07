
      
###  Create empty dataframe for the output metrics of the ROC  #----------------
# declare field names of the dataframe
Allresults <- data.frame("Continent"=numeric(0), 
                         "StressorNb"=numeric(0), 
                         "NbObs"=numeric(0),
                         "NbPos"=numeric(0),
                         "PercPos"=numeric(0),
                         "AUC"=numeric(0),
                         "Thresh"=numeric(0),
                         "Specif"=numeric(0),
                         "Sensit"=numeric(0))


# create function calcualting ROC-AUC
source("./data_proc/fcn/roc_auc_function_v2.r")




# loop through stressors
for (s in unique(ramsites_long$stressor_nb)){

  
  # Loop thru continent  -----------------------
  for (r in unique(as.character(ramsites_long$Region))){

    
    temp_stressor_data <- ramsites_long %>%
                          filter(Region == r, stressor_nb == s)
    
    if (nrow(temp_stressor_data) > 0) {
      # Calculate the stressor specific AUC for all sites globally (NOT subsetted by continents).
      Allresults <- rbind(Allresults, auc_calc(temp_stressor_data$ram, temp_stressor_data$rt, r, s)) }
  }
  
  
  # subset only by continent
  temp_stressor_data <- ramsites_long %>% filter(stressor_nb == s)
  
  # Calculate the stressor specific AUC for all sites globally (NOT subsetted by continents). 
  if (nrow(temp_stressor_data) > 0) {
    # Append the output AUC to the result table
    Allresults <- rbind(Allresults, auc_calc(temp_stressor_data$ram, temp_stressor_data$rt, "Global", s)) }
  
}


# import function converting stressor codes to full names
source("./data_proc/fcn/function_convert_rt_to_stressor_name.r")

Allresults <- convert_rt_to_stressor_name(Allresults)


### Write ROC results dataframe to file ----------------------------------------
write.csv(Allresults, file = "../../output/continent_stressor_roc_auc_output.csv",
          row.names=FALSE)




rm(r, s, ram_cont, auc_calc)
