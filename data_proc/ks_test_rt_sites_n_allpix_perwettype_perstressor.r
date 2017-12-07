### K-S test, comparing the goodness of fit of the two distributions  #----------
# test of goodness of fit to a pdf (like a khi-square but not parametric)
# but no need to divide data into bins.
# to test whether the ramsar sites pdf has the same pdf as the global pixels
# convert model output to df with 'broom' library



# rename columns for consistency
names(ramsites)[names(ramsites) == 'Region'] <- 'ramsar_region'
names(rt_allpix)[names(rt_allpix) == 'ram_region'] <- 'ramsar_region'

# Create list of unique ramsar regions
u_ram_region <- unique(ramsites$ramsar_region)
u_wettype <- unique(ramsites$wettype)


# create empty dataframe to K-S  results
ks_df <- data.frame()


# run 
# ks <- tidy(ks.test(ramsites$rt_lin_bd, ramsites$rt_lin_bd, 
#                    alternative = c("two.sided", "less", "greater"), exact = NULL))
# 
# # add column value, then bind row to dataframe
# ks <- ks %>% mutate(Test = "ks", ramsar_region = 'Global')
# ks_df <- rbind(ks_df, ks)


for(s in c(1,2,3,4,5,7,9,10,11,15,16,19,20,22,23)) {
  c <- paste('rt_', s, sep='')
  
  
  # global comparison
  t_rt <- unlist(ramsites %>% select(c))
  t_pix <- unlist(rt_allpix %>% select(c))
  
  # KS test for each continent 
  ks <- tidy(ks.test(t_rt, t_pix, alternative = c("two.sided", "less", "greater"), exact = NULL))
  
  # add column value, then bind row to dataframe
  ks <- ks %>% mutate(Test = "ks", stressor = c, wettype='all')
  ks_df <- rbind(ks_df, ks)
  
  
  
  
  # Run K-S test in loop through ramsar regions ----------------------------------
  for (t in 1:length(u_wettype)) {
    
    
    t_rt <- unlist(ramsites %>% 
                filter(wettype == u_wettype[t]) %>% select(c))
    
    # t_pix <- unlist(rt_allpix %>% 
    #             filter(wettype == u_wettype[t]) %>% select(c))
    # 
    # KS test for each continent 
    ks <- tidy(ks.test(t_rt, t_pix, 
                       alternative = c("two.sided", "less", "greater"), exact = NULL))
    
    # add column value, then bind row to dataframe
    ks <- ks %>% mutate(Test = "ks",  stressor = c, wettype=u_wettype[t])
    ks_df <- rbind(ks_df, ks)

  }
}

# remove objects
rm(ks, t_rt, t_pix, c, n, t, s, u_wettype)


# count the 
ks_df_wettype <- ks_df %>%
                 filter(wettype != 'all')
sum(ks_df_wettype$p.value <= 0.05)

ks_df_all <- ks_df %>%
  filter(wettype == 'all')
sum(ks_df_all$p.value <= 0.05)



### write output ----------------- 
write.csv(ks_df, '../../output/ks_test/ks_perwettype_stressor.csv')
