# threshold the rt scores with continental and global threshold



### Write ROC results dataframe to file ----------------------------------------
aucroc_results <- read.csv("../../output/continent_stressor_roc_auc_output.csv",
                           stringsAsFactors = F)

# # add global threshold as a column instead of row
aucroc_results_glob <- aucroc_results %>%
                         filter(Continent == 'Global') %>%
                         mutate(glob_thresh = Thresh,
                                Stressor = as.character(Stressor)) %>%
                         select(Stressor, glob_thresh)


# 
threshs <-  aucroc_results_glob %>%
            filter(Continent != 'Global') %>%
            mutate(Stressor = as.character(Stressor),
                   cont_thresh = Thresh) %>%
            dplyr::select(Continent, Stressor, cont_thresh) %>%
            left_join(., glob_aucroc_results, by='Stressor')


# join thresholds to the ramsite long df
ramsites_long_threshd <- left_join(ramsites_long, threshs,
                                          by=c("Region"="Continent", "stressor_nb"="Stressor")) %>%
                          filter(!is.na(cont_thresh) | !is.na(glob_thresh) ) %>%
  
                          # do the thresholding
                          mutate(rt_cont_threshd = ifelse(rt >= cont_thresh, 1, 0),
                                 rt_glob_threshd = ifelse(rt >= glob_thresh, 1, 0))




# Jaccard distance between ram & rt_thresh  for each Ramsar site ----------------------
 
# create empty df
j_dist <- data.frame("SiteRef"=character(), 
                      "jd_cont"=numeric(0),
                      "jd_glob"=numeric(0),
                      "n"=numeric(0))



# loop through stressors
for (s in unique(ramsites_long_threshd$SiteRef)){
  
  # subset only by continent
  temp_thrshd <- ramsites_long_threshd %>% 
                        filter(SiteRef == s) %>%
                        select(ram, rt_cont_threshd, rt_glob_threshd)

  # calculate teh two diagonal cells in 2x2 continegncy table
  a11 <- nrow(temp_thrshd[which(temp_thrshd$ram==1 & temp_thrshd$rt_cont_threshd==1),])  
  a00 <- nrow(temp_thrshd[which(temp_thrshd$ram==0 & temp_thrshd$rt_cont_threshd==0),] )
  jd_cont <- (a11 + a00)/nrow(temp_thrshd)
  
  
  # calc jd_glob 
  # the two diagonal cells in 2x2 continegncy table
  a11 <- nrow(temp_thrshd[which(temp_thrshd$ram==1 & temp_thrshd$rt_glob_threshd==1),])  
  a00 <- nrow(temp_thrshd[which(temp_thrshd$ram==0 & temp_thrshd$rt_glob_threshd==0),] )
  jd_glob <- (a11 + a00)/nrow(temp_thrshd)
  
  
  
  # add
  j_dist <- rbind(j_dist, data.frame("SiteRef"= s,
                                     "jd_cont"= jd_cont,
                                     "jd_glob" = jd_glob, 
                                     "n"=nrow(temp_thrshd)))  }

# delete objects
rm(s, a11, a00, temp_thrshd, jd_glob, jd_cont, glob_aucroc_results, 
   aucroc_results, threshs, ramsites_long_threshd)






ramsites_cont <- ramsites %>% select(SiteRef, Region)



# filter to have minimum 4 stressors compared
j_dist_long <- j_dist %>% 
               filter(n >= 11) %>%
               gather(thresh, jd, jd_cont:jd_glob) %>%
               left_join(., ramsites_cont, by='SiteRef')


rm(ramsites_cont)


# plot -----------------------------------------------------------
ggplot(j_dist_long) +
      geom_boxplot(aes(x=Region, y=jd, fill=thresh)) +

      xlab("") + ylab("Jaccard distance") +
      coord_flip()



