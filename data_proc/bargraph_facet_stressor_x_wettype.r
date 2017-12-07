
auc_results_glob$Stressor <- as.character(auc_results_glob$Stressor)

# join the ramsites with the threshold from ROCAUC
ramsites_long_wthresh <- left_join(ramsites_long, auc_results_glob,
                                  by=c('stressor_nb'='Stressor',
                                       'wettype'='Type')) %>%
                          filter(!is.na(Thresh))



# rename the columns of auc_results to match those from other df
auc_results_glob <- auc_results_glob %>%
                    mutate(stressor_nb = Stressor,
                           wettype = Type) %>% 
                    filter(wettype != "All types")



# ---------------
f<- ggplot(ramsites_long, aes(x=rt, fill=as.factor(ram))) + 

    geom_histogram(data= subset(ramsites_long_wthresh,ram==0), 
                   aes(x=rt, fill=as.factor(ram)),
                   stat='bin', color=NA, alpha=0.5) +
  
    geom_histogram(data= subset(ramsites_long_wthresh,ram==1), 
                   aes(x=rt, fill=as.factor(ram)),
                   stat='bin', color=NA, alpha=0.5) +
    
    
    geom_vline(data= subset(auc_results_glob, !is.na(Thresh)), 
               aes(xintercept = as.numeric(Thresh)), color='black') +
  
    facet_wrap(wettype~as.factor(stressor_nb), scales="free", drop=TRUE)




### save figure to file ----------------------------------------------------------
ggsave('../../output/figures/density_plots_stressor_x_type_wettype_regroup.png', f,
       width=650, height=415, dpi=200, units="mm", type = "cairo-png")
dev.off()


  
  
  
