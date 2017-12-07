### Write ROC results dataframe to file #--------------------------------------------------------------------------------------------------
rocauc <- read.csv("../../output/continent_stressor_roc_auc_output.csv", 
                   stringsAsFactors=F)

file <- read.csv("../../output/ramsar_sites_rt_ram_long_format.csv",
                 stringsAsFactors=F)
inland_subset <- subset(file, InlandCateg==1 & HumanMadeCateg==0 & MarineCateg==0 & riverthreat >-1)
rm(file)


# threshold 


# for each cont-stress, threshold into binary