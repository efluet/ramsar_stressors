# import library -----------
library(pacman)
pacman::p_load(qdapRegex)


# get Ramsar Sites database  --------------------------------------------
ramsites=read.csv("../../Data/ramsar_sites_rt_ram_data_1june2015_wbdlin_mod_wyear.csv", 
              stringsAsFactors = F)


# remove in domwettype list
ramsites <- ramsites %>% mutate(DomWetType = gsub(" ", "", ramsites$DomWetType)) 
  


# get wetland type reclassification --------------------------------------------
class_equiv <- read.csv("../../data/ramsar_class_equivalence.csv", stringsAsFactors = F)

# vector of types to keep
lvl1_tokeep <- c("Intertidal / Estuarine", "Flowing water", "Lakes & ponds",
                 "Brackish lake/marsh", "Marshes (inorganic soil)", "Marshes (organic soil)")

# create vector of Ramsar wetland types to keep, for filtering
dom_types_tokeep <- unlist(subset(class_equiv, lvl1 %in% lvl1_tokeep)['ramsar_class_code'])



# source('./data_proc/separate_multipledomwettypes_intorows.r')
source('./data_proc/wettype_regroup.r')


ramsites <- ramsites %>%
            left_join(., ramsites_wettypes_u, by='DomWetType') %>%
            mutate(wettype = regroup) %>%
  filter(!is.na(wettype),
         wettype != 'saline')





# count nb. of stressors per site: original and aggregated  ----------------------------

# aggregated nb of  ramsar stressors
ram_col_indices <- seq(21, 35, by=1)  # create sequence of column index wanted - referring to the 
ramsites$agg_ram_cnt <- rowSums (ramsites[,ram_col_indices], 
                                 na.rm = FALSE, dims = 1)


# count nb. original ramsar stressors 
orig_ram_stress=read.csv("../../data/all_ramsar_stressor_count_persite_14may2015.csv", 
                         stringsAsFactors = F)

# sum the binary ramsar reporting into a new column
ram_col_indices <- seq(3, 119, by=1)
orig_ram_stress$ori_ram_cnt <- rowSums (orig_ram_stress[,ram_col_indices], na.rm = FALSE, dims = 1)

# subset the table to only the two field needed
orig_ram_stress <- orig_ram_stress[c("SiteRef","ori_ram_cnt")]

# join the count of original ramsar stressors to the inland subset of sites
ramsites <- merge(x = ramsites, y = orig_ram_stress, by = "SiteRef", all.x = TRUE)



# filter sites that report no stressor
ramsites <- ramsites %>%
  
  filter(agg_ram_cnt > 0) %>%
  #filter(ori_ram_cnt != 0 & rt_lin_bd > quantile(ramsites$rt_lin_bd, probs=0.5)) %>%

  filter(rt_lin_bd > -9999) %>%
  filter(Total.site.area..ha. > 100)   


# remove duplicated row
ramsites <- unique(ramsites)

# repalce all -9999 by NA, to be filtered by stressor individually
ramsites[ ramsites == -9999 ] <- NA







# convert df to long format ----------------------------------------------------
ramsites_long <- ramsites %>%
  
  # remove some unnecessary columns
  select(-one_of("Subregion", "GeoPosition", "WI.Site.Reference",
                 "Total.site.area..ha.", "Transboundary", "IUCNcateg")) %>%
  
  
  gather(stressor, value, ram_1:rt_23) %>%
  separate(stressor, into=c("source","stressor_nb")) %>%
  spread(source, value) %>%
  filter(!is.na(rt))





# del env var
remove(ramsites_wettypes, ramsites_wettypes_u, orig_ram_stress, ram_col_indices, class_equiv)





# plot nb site per wettype -----------------------------------------------------
ggplot(ramsites) +
  geom_histogram(aes(x=regroup, fill= regroup), stat='count') +
  guides(fill=FALSE) +
  coord_flip()



# save plot
ggsave("../../output/figures/barplot_wettype_cnt.png", 
       dpi=400, width=90, height=90,units='mm')


dev.off()






# get some summary stats for paper -------------------------------------------- 
# nb.countries <- length(unique(ramsites$Country))
# nb.wet.types <- length(unique(ramsites$SingleDomWetType))
# unique(ramsites$SingleDomWetType)

# ramsites_wetclasscnt <-  ramsites %>%
#   group_by(lvl1) %>%
#   summarize(n= n())
