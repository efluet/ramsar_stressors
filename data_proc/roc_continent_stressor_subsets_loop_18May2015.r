
# Convert the Ramsar Site table from wide to long, 
# in order for the analysis by continent to be carried out. 


# create iterative file name
infile <- "../../Data/ramsar_sites_rt_ram_data_1june2015_wbdlin_mod.csv"
ramsites <- read.csv(infile, header=TRUE)
rm(infile)

ramsites_long <- ramsites %>%
  select(-one_of(#"SiteNo", "Country", "Region", 
                 #"HumanMadeCateg", "InlandCateg", "MarineCateg", 
                 #"DomWetType", "SingleDomWetType", "rt_lin_bd"
                 "Subregion",
                 "GeoPosition", "WI.Site.Reference", "Total.site.area..ha.", 
                 "Transboundary", "IUCNcateg")) %>%
  
  gather(stressor, value, ram_1:rt_23) %>%
  separate(stressor, into=c("source","stressor_nb")) %>%
  spread(source, value)


# write long table to file -----------------------------------------------------
# write.csv(ramsites_long, 
#           file = "../../Output/ramsar_sites_rt_ram_long_format.csv",
#           row.names=FALSE)

ramsites_long_inland <-ramsites_long %>%
      
      filter(InlandCateg==1 & HumanMadeCateg==0 & MarineCateg==0) %>%

      # Filter -9999 NoData data  
      filter(rt > 0)
      
      # Remove certain stressors with their numbers; these stressors we not added in the pairing 
      #stressor_nb %in% c(5, 6, 12, 13, 14, 15, 17, 18, 21))


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


# create vectors of unique regions and stressors
continent_vector = c(levels(ramsites_long_inland[,"Region"]))
stressor_nb_vector = c(levels(ramsites_long_inland[,"stressor_nb"]))


#### Loop thru continent & stressor- calculate ROC-AUC -----------------------
for (r in unique(as.character(ramsites_long_inland$Region))){

  
  # loop through stressors
  for (s in unique(ramsites_long_inland$stressor_nb)){
  
    temp_stressor_data <- ramsites_long_inland %>%
                          filter(Region == r,
                                 stressor_nb == s)
    
    print(paste(r, s, nrow(temp_stressor_data)), sep='   ')

    if (nrow(temp_stressor_data) > 0) {

      # Calculate the stressor specific AUC for all sites globally (NOT subsetted by continents).
      # Append the output AUC to the result table.
      
      #auc_calc <- function(binary_data, continuous_data, subset_name1, subset_name2, temp_results){

      Allresults <- rbind(Allresults, auc_calc(temp_stressor_data$ram, temp_stressor_data$rt, r, s))
      
      #Allresults <- rbind(Allresults, auc_calc(temp_stressor_data, continent, curr_rt_stressor))

    }
  }
}



### Write ROC results dataframe to file ----------------------------------------
write.csv(Allresults, file = "../../output/continent_stressor_roc_auc_output.csv",
          row.names=FALSE)


