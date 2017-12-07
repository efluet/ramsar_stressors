### K-S test, comparing the goodness of fit of the two distributions  #----------



# rename columns for consistency
names(ramsites)[names(ramsites) == 'Region'] <- 'ramsar_region'
names(rt_allpix)[names(rt_allpix) == 'ram_region'] <- 'ramsar_region'

# Create list of unique ramsar regions
unique_ram_region <- unique(ramsites$ramsar_region)



# create empty dataframe to store results of Kolmogorov-Smirnov Test
ks_df <- data.frame()

# test of goodness of fit to a pdf (like a khi-square but not parametric)
# but no need to divide data into bins.
# to test whether the ramsar sites pdf has the same pdf as the global pixels
# convert model output to df with 'broom' library

ks <- tidy(ks.test(ramsites$rt_lin_bd, ramsites$rt_lin_bd, alternative = c("two.sided", "less", "greater"), exact = NULL))

# add column value, then bind row to dataframe
ks <- ks %>% mutate(Test = "ks", ramsar_region = 'Global')
ks_df <- rbind(ks_df, ks)



# Run K-S test in loop through ramsar regions ----------------------------------
for (i in 1:length(as.list(unique_ram_region))) {
  
  
  t_rt <- ramsites %>% 
              filter(ramsites$ramsar_region == unique_ram_region[i]) %>%
              select(rt_lin_bd)
  
  t_pix <- rt_allpix %>% 
              filter(ramsites$ramsar_region == unique_ram_region[i]) %>%
              select(rt_lin_bd)
  
  # KS test for each continent 
  ks <- tidy(ks.test(t_rt$rt_lin_bd, t_pix$rt_lin_bd, 
                     alternative = c("two.sided", "less", "greater"), exact = NULL))
  
  # add column value, then bind row to dataframe
  ks <- ks %>% mutate(Test = "ks", ramsar_region = unique_ram_region[i])
  ks_df <- rbind(ks_df, ks)
  
}


# remove objects
rm(ks, ram_cont, pix_cont, reg_lookup)

### write output ----------------- 
write.csv(ks_df, '../../output/ks_test/ks_percont.csv')
