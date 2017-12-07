



# get ROCAUC vulnerabiltiy weights ----------------------------------------
auc_vuln <- read.csv("../../output/stressor_wettype_auc_vuln_weights.csv", stringsAsFactors = F)
auc_vuln <- auc_vuln %>%
  mutate(s_name = tolower(s_name), Type = tolower(Type)) %>%
  # remove confusing af colname
  dplyr::select(-rank)




# get other vulnerability weights from literature
lit_vuln = read.csv("../../data/ecosys_vulnerability_weights.csv", 
                       header=TRUE)
lit_vuln <- lit_vuln %>%
            filter(common_stressor_name != '', !is.na(pooled_weight)) %>%
            mutate(common_stressor_name = tolower(common_stressor_name),
                   Environment = tolower(Environment)) %>%
            dplyr::select(-one_of('stressor','Domain','categ_weights','method','stressor_category','source_label')) %>%
            group_by(common_stressor_name, Environment) %>%
            summarize(pooled_weight = mean(pooled_weight)) %>%
            ungroup() 

                  



# merge the literature and ramsar threshold
vuln_comb  <- lit_vuln %>%
               filter(common_stressor_name != "")  %>%
               left_join(., auc_vuln,
                         by=c('common_stressor_name'='s_name', 'Environment'='Type')) %>%
               filter(!is.na(AUC)) %>%
  
  
                # calc rank of 
                arrange(Environment, -pooled_weight) %>%
                group_by(Environment) %>%
                mutate(lit_rank= row_number()) %>%
                ungroup() %>%
                
                # calc rank of auc threshold
                arrange(Environment, -Thresh_perc) %>%
                group_by(Environment) %>%
                mutate(auc_thresh_rank = rank(Thresh_perc)) %>%
                ungroup() %>%

                mutate(rank_dif = lit_rank-auc_thresh_rank)






# spearman loop, filtering out

spearman_output <- data.frame()

for (i in seq(0, 0.7, 0.05)) {
  
  
  # Spearman correlation ---------------------------------------------------------
  l <- cor.test(~ pooled_weight + Thresh_perc, 
           data=subset(vuln_comb, Environment=='lake' & AUC > i),
           method = "spearman",
           continuity = FALSE,
           conf.level = 0.95)
  
  r <- cor.test(~ pooled_weight + Thresh_perc, 
           data=subset(vuln_comb, Environment=='river' & AUC > i),
           method = "spearman",
           continuity = FALSE,
           conf.level = 0.95)
  
  
  print(c(i, r$p.value))
  
}


vuln_comb_riv <- vuln_comb %>% 
                 filter(Environment== 'river') %>%
                 mutate(common_stressor_name = as.factor(common_stressor_name)) %>%
                 arrange(lit_rank)


# plot comparison of weight ----------------------------------------------------
ggplot(subset(vuln_comb_riv, Environment== 'river')) +
  geom_point(aes(x=as.factor(lit_rank), y=auc_thresh_rank, fill=AUC), size=2.7, color='black', shape=21) +
  
  geom_abline(intercept=0, slope = 1, color='black', size=0.3) +
  
  scale_fill_distiller(palette = "Spectral") +

  xlab('Stressor vulnerability rank from experts (Vorosmarty et al. 2010) \n  <-- more important') + 
  ylab('Stressor vulnerability rank from ramsar sites \n <-- more important')  +
  ggtitle('Rivers') +
  
  scale_y_continuous(breaks= seq(1,length(vuln_comb$lit_rank),1)) +
  # add the sorted labels to the  
  scale_x_discrete(labels=paste(vuln_comb_riv$common_stressor_name, vuln_comb_riv$lit_rank, sep=' - ')) +
  
  
  my_own_scatter_theme +
  theme(panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.85, 0.25))
  

# save to file -----------------------------------------------------------------
ggsave('../../output/figures/vuln_weight_auc_v_lit_scatter_river.png',
       width=87, height=100, dpi=600, units="mm", type = "cairo-png")
dev.off()






vuln_comb_lake <- vuln_comb %>% 
  filter(Environment== 'lake') %>%
  mutate(common_stressor_name = as.factor(common_stressor_name)) %>%
  arrange(lit_rank)

# plot comparison of weight ----------------------------------------------------
ggplot(vuln_comb_lake) +
  geom_point(aes(x=as.factor(lit_rank), y=auc_thresh_rank, fill=AUC), size=2.7, color='black', shape=21) +
  
  geom_abline(intercept=0, slope = 1, color='black', size=0.3) +
  
  scale_fill_distiller(palette = "Spectral") +
  
  xlab('Stressor vulnerability rank from experts (Allan et al. 2013) \n  <-- more important') + 
  ylab('Stressor vulnerability rank from ramsar sites \n <-- more important')  +
  ggtitle('Lakes') +
  
  scale_y_continuous(breaks= seq(1,length(vuln_comb$lit_rank),1)) +
  # add the sorted labels to the  
  scale_x_discrete(labels=paste(vuln_comb_riv$common_stressor_name, vuln_comb_riv$lit_rank, sep=' - ')) +
  
  
  my_own_scatter_theme +
  theme(panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.85, 0.25))


# save to file -----------------------------------------------------------------
ggsave('../../output/figures/vuln_weight_auc_v_lit_scatter_lake.png',
       width=87, height=100, dpi=600, units="mm", type = "cairo-png")
dev.off()















# # plot comparison of weight ----------------------------------------------------
# ggplot(vuln_comb) +
#   geom_point(aes(x=common_stressor_name, y=lit_rank), color='red', size=6) +
#   geom_point(aes(x=common_stressor_name, y=auc_thresh_rank), size=3.5) +
#   facet_wrap(~Environment, scales='free', ncol=1) +
#   coord_flip() +
#   scale_y_continuous(breaks= pretty_breaks()) +
#   xlab('') + ylab('Vulnerability rank of stressors') 
#   

# 
# # bar plot of difference in ranks
# ggplot(vuln_comb) +
#   geom_bar(aes(x=common_stressor_name, y=rank_dif), stat='identity', fill='grey75') +
#   geom_hline(yintercept=0, color='black', size=0.5)+
#   facet_wrap(~Environment, scales='free', ncol=1) +
#   coord_flip() +
#   scale_y_continuous(breaks= pretty_breaks()) +
#   xlab('') + ylab('Vulnerability rank of stressors') + my_own_theme
# 