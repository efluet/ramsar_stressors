



# combine the 
auc_results_glob_lakriv <- auc_results_glob %>%
                           filter(Type %in% c('Flowing water', 'Lakes & ponds')) %>%
                           mutate(Type= ifelse(Type=='Flowing water', 'River', 'Lake')) %>%
                           mutate(common_stressor = s_name) %>%
                           mutate(Source = 'This study') %>%
                           select(common_stressor, Type, Thresh_perc, Source)


vul_weights_2 <- vul_weights %>%
                 select(stressor, Environment, compound_pooled_weight, source_label)


common_colnames <- c('stressor', 'type', 'weights', 'source')
names(vul_weights_2) <- common_colnames
vul_weights_2$source <- 'literature'

names(auc_results_glob_lakriv) <- common_colnames




# bind rows of lit weights and those from this study
a <- rbind(vul_weights_2, auc_results_glob_lakriv) %>%
     filter(!is.na(weights))




# compare 
ggplot(a, aes(x=stressor, y=weights, fill=source)) +
  geom_bar(stat='identity', position='dodge') +
  facet_wrap(~type, scales='free') +
  coord_flip()  + 
  theme(legend.position = 'bottom',
        legend.direction = 'vertical')

### save figure to file ----------------------------------------------------------
ggsave('../../output/figures/vulweights_compare_lit_and_thisstudy_v2.png',
       width=178, height=145, dpi=800, units="mm", type = "cairo-png")
dev.off()

