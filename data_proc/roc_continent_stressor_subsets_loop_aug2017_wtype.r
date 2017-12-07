

###  Create empty dataframe for the output metrics of the ROC  #----------------
# declare field names of the dataframe
auc_results <- data.frame("Stressor"=character(),
                          "Type"=character(),
                          "Continent"=character(),
                          
                          
                          "NbObs"=numeric(0),
                          "NbPos"=numeric(0),
                          "PercPos"=numeric(0),
                          "AUC"=numeric(0),
                          "AUC_permut_perc"=numeric(0),
                          "Thresh"=numeric(0),
                          "Specif"=numeric(0),
                          "Sensit"=numeric(0),
                          stringsAsFactors = F)


# create function calcualting ROC-AUC
source("./data_proc/fcn/roc_auc_function_v4_wpermut.r")


# loop through stressors
for (s in unique(ramsites_long$stressor_nb)){
  
  # loop through wetland types
  for (t in unique(ramsites_long$wettype)){
    
    # loop through continents
    for (c in unique(ramsites_long$ram_cont)){
      
      
      
      # subset: stressor x type x continent  &   calc roc-auc
      tempsubset <- ramsites_long %>% filter(stressor_nb == s, wettype == t, Region == c)
      if (nrow(tempsubset) > 0) {
        auc_results <- rbind(auc_results, auc_calc(tempsubset$ram, tempsubset$rt, s, t, c)) }
      
      # subset: stressor x continent
      tempsubset <- ramsites_long %>% filter(stressor_nb == s, Region == c)
      if (nrow(tempsubset) > 0) {
        auc_results <- rbind(auc_results, auc_calc(tempsubset$ram, tempsubset$rt, s, 'All types', c)) }
      
    }
    
    
    # subset: stressor x type
    tempsubset <- ramsites_long %>% filter(stressor_nb == s, wettype == t)
    if (nrow(tempsubset) > 0) {
      auc_results <- rbind(auc_results, auc_calc(tempsubset$ram, tempsubset$rt, s, t, 'Global')) }
    
    }
  
  
  # subset: stressor x type
  tempsubset <- ramsites_long %>% filter(stressor_nb == s)
  if (nrow(tempsubset) > 0) {
    auc_results <- rbind(auc_results, auc_calc(tempsubset$ram, tempsubset$rt, s, 'All types', 'Global')) }
    
  }



# remove duplicated rows from loops
auc_results <- unique(auc_results)

auc_results$AUC_permut_perc <- as.numeric(auc_results$AUC_permut_perc)


# import function converting stressor codes to full names
source("./data_proc/fcn/function_convert_rt_to_stressor_name.r")
auc_results <- convert_rt_to_stressor_name(auc_results)


### Write ROC results dataframe to file ----------------------------------------
write.csv(auc_results, file = "../../output/stressor_wettype_auc_output.csv",
          row.names=FALSE)




rm(t, s, c, auc_calc, tempsubset, convert_rt_to_stressor_name)
