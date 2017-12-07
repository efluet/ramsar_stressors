### ------------
# Create function that loops through the stressors and calculates all the ROC-AUC for a given input data frame
# The data has to be organized in a way that the first set of X columns must be continuous River Threat indices,
# followed by another set of X columns in the same order containing the binary Ramsar reporting.


#auc_calc <- function(input_data, subset_name1, subset_name2, temp_results){
  
  
auc_calc <- function(binary_data, continuous_data, subset_name1, subset_name2, subset_name3, temp_results){
  
  temp_auc = 0
  
  # Calculate the total and positive number of points for reporting.
  temp_nb_positive_pts = sum(binary_data)
  temp_tot_nb_pts = length(binary_data)
  
  temp_perc_positive = temp_nb_positive_pts / temp_tot_nb_pts * 100
  
  if(temp_nb_positive_pts > 0 & temp_tot_nb_pts > 5){
    
    temp_results <- data.frame("Continent"=numeric(0),
                               "WetlandType"=numeric(0),
                               "Stressor"=numeric(0),
                               "NbObs"=numeric(0),
                               "NbPos"=numeric(0),
                               "PercPos"=numeric(0),
                               "AUC"=numeric(0),
                               "Thresh"=numeric(0),
                               "Specif"=numeric(0),
                               "Sensit"=numeric(0))
    
    # input_data <- continent_stressor_subset
    # Attribute the two columns of the new subset to their own vectors.
    # rt_ram_values <- subset(input_data, select=c(4,5))
    # temp_rt <- as.double(input_data[,4])
    # temp_ram <- as.double(input_data[,5])
    
    
    # Calculate the ROC, return the AUC
    temp_auc <- (roc(binary_data~continuous_data)$auc)[1]
    
    # run the coord threshold of the ROC...
    temp_roc <- (roc(binary_data~continuous_data))
    temp_coord <- coords(temp_roc, "b", ret=c("threshold", "specificity", "sensitivity"),
                          as.list=TRUE,
                          best.method=c("youden"),
                          best.weights=c(1, 0.5))
    
    # other option for calculating threshold
    #best.method=c("closest.topleft"),
    
    #temp_coords <- coords(temp_roc, "b", 
    #               ret=c("threshold", "specificity", "sensitivity"),
    #               as.list=FALSE, drop=TRUE, best.method=c("closest.topleft"),
    #               best.weights=c(1, 0.5))
    
    
    #input=c("threshold", "specificity", "sensitivity"), 
    #if(is.na(temp_coord["threshold"])){
    
    # set to NA value -99999 when no result is returned?
    if(length(temp_coord) != 3 ){
      temp_coord["threshold"] = -99999
      temp_coord["specificity"] = -99999
      temp_coord["sensitivity"] = -99999
    }
    
    print(temp_auc)
    print(temp_coord["threshold"])
    #print(length(temp_coord))  
    
    
    
    #print (temp_tot_nb_pts)
    #print (temp_nb_positive_pts)
    
    # Filter the ROCs where positives do not represent a minimum 0f 10% of the data points
    if (temp_nb_positive_pts/temp_tot_nb_pts > 0.05){
      
      
      # Write output to the results file.
      newRow <- c(subset_name1,
                  subset_name2,
                  temp_tot_nb_pts,
                  temp_nb_positive_pts,
                  temp_perc_positive,
                  temp_auc,
                  temp_coord["threshold"],
                  temp_coord["specificity"],
                  temp_coord["sensitivity"])
      
      temp_results[nrow(temp_results)+1,] <- newRow
      
    }
    return(temp_results)
  }
}
