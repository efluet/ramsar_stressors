# calc vulnerability from threshold



auc_results_glob <- auc_results %>%
                       filter(Continent == 'Global')


# filter the rt pix df
rt_allpix <- rt_allpix %>%
             filter(!is.na(Lin1_3Dec2))

rt_allpix_onlylin <- rt_allpix[5:27]



# loop through rows and calc the percenteil of threshold
for (r in seq(1, nrow(auc_results_glob))){
  
  # get the threshold of each row
  temp_thresh <- auc_results_glob[r, 'Thresh']
  temp_stress <- auc_results_glob[r, 'Stressor']
  
  
  # get vector of the rt score for that stressor
  temp_subset_ramsites_long <- ramsites_long %>%
                               filter(stressor_nb == eval(temp_stress),
                                      !is.na(rt))
  
  temp_subset_ramsites_long <- temp_subset_ramsites_long[, 'rt']
  
  
  #temp_subset_rt_allpix_onlylin <- rt_allpix_onlylin[, eval(temp_stress)]
  #temp_subset_rt_allpix_onlylin <- temp_subset_rt_allpix_onlylin[!is.na(temp_subset_rt_allpix_onlylin)]
  

  
  # get the 
  #percentile <- ecdf(temp_subset_rt_allpix_onlylin)
  percentile <- ecdf(temp_subset_ramsites_long)
  auc_results_glob[r, 'Thresh_perc'] <- percentile(temp_thresh)
  
  
  print(paste(temp_stress, temp_thresh,  percentile(temp_thresh), sep=" - "))
  #print(mean(temp_subset_rt_allpix_onlylin))
  
}



# 
auc_results_glob_filt_ranked <- auc_results_glob %>%
  filter(AUC > 0.5,  Type != 'All types') %>%
  arrange(s_name, -Thresh_perc) %>%
  group_by(s_name) %>%
  mutate(rank=row_number())





# plot it ----------------------------------------------------

ggplot(auc_results_glob_filt_ranked, 
       aes(x=Type, y=Thresh_perc, fill=as.numeric(AUC))) +
  geom_bar(stat='identity') +
  coord_flip() +
  facet_wrap(~s_name) + 
  
  ylab("Best threshold (Youden J) as percentile of stressor scores in Ramsar sites  \n (lower interpreted as more vulnerable ?)") + 
  xlab('') +
  
  
  theme(#legend.position="none",
    #legend.title=element_blank(),
    legend.key = element_blank(),
    panel.grid.minor = element_blank(), 
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.text.x=element_text(colour="black"),
    axis.text.y=element_text(colour="black"),
    plot.title = element_text(size = rel(0.9)))



### save figure to file ----------------------------------------------------------
ggsave('../../output/figures/thresh_percentile_pertype_v5.png',
       width=178, height=175, dpi=800, units="mm", type = "cairo-png")
dev.off()





# plot it ----------------------------------------------------

ggplot(auc_results_glob_filt_ranked, 
       aes(x=s_name, y=rank, fill=as.numeric(AUC))) +
  geom_bar(stat='identity') +
  coord_flip() +
  facet_wrap(~Type) + 
  
  ylab("Best threshold (Youden J) as percentile of stressor scores in Ramsar sites  \n (lower interpreted as more vulnerable ?)") + 
  xlab('') +
  
  
  theme(#legend.position="none",
    #legend.title=element_blank(),
    legend.key = element_blank(),
    legend.position = 'top',
    legend.direction = 'horizontal',
    panel.grid.minor = element_blank(), 
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.text.x=element_text(colour="black"),
    axis.text.y=element_text(colour="black"),
    plot.title = element_text(size = rel(0.9)))


### save figure to file ----------------------------------------------------------
ggsave('../../output/figures/thresh_percentile_pertype_rev_v6.png',
       width=178, height=175, dpi=800, units="mm", type = "cairo-png")
dev.off()

