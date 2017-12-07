# import library -----------
library(pacman)
pacman::p_load(qdapRegex)


# get Ramsar Sites threat --------------------------------
ramsites=read.csv("../../Data/ramsar_sites_rt_ram_data_1june2015_wbdlin_mod_wyear.csv", 
              stringsAsFactors = F)





# # remove parenteses other than categories  
# ramsites$Wetland.Type <- gsub("\\(over 8 ha)", "", ramsites$Wetland.Type)
# ramsites$Wetland.Type <- gsub("\\(below 8 ha)", "", ramsites$Wetland.Type)
# ramsites$Wetland.Type <- gsub("\\(including intensively managed or grazed wet meadow or pasture)", "", ramsites$Wetland.Type)
# ramsites$Wetland.Type <- gsub("\\(generally over 8 ha)", "", ramsites$Wetland.Type)
# ramsites$Wetland.Type <- gsub("\\(generally below 8 ha)", "", ramsites$Wetland.Type)
# ramsites$Wetland.Type <- gsub("\\(e.g. fish/shrimp)", "", ramsites$Wetland.Type)
# ramsites$Wetland.Type <- sapply(rm_round(ramsites$Wetland.Type, extract=TRUE), paste, collapse=",")





# get wetland type reclassification --------------------------------------------

# read file keeping only certain column.
class_equiv <- read.csv("../../data/ramsar_class_equivalence.csv", stringsAsFactors = F)

lvl1_tokeep <- c("Intertidal / Estuarine", "Flowing water", "Lakes & ponds", 
                 "Brackish lake/marsh", "Marshes (inorganic soil)", "Marshes (organic soil)")

# create vector of Ramsar wetland types to keep
dom_types_tokeep <- unlist(subset(class_equiv, lvl1 %in% lvl1_tokeep)['ramsar_class_code'])



# filter ramsites by wet type and rt nodata ------------------------------------

ramsites<- ramsites %>%

  # remove sites without a wet type
  filter(DomWetType != "", 
         DomWetType != 'no information available', 
         !is.na(SingleDomWetType))



source('./data_proc/separate_multipledomwettypes_intorows.r')



# has to be sep from prev data manip, because lenghts of cols differ otherwise
ramsites<- ramsites %>%
  
  # keep the first dominant wetland type
  #mutate(SingleDomWetType = sapply(strsplit(gsub(" ", "", ramsites$DomWetType), ","), "[[", 1)) %>%
  
  # remove spaces
  mutate(SingleDomWetType = gsub(" ", "", ramsites$SingleDomWetType)) %>%
  
  # join the regrouped wetland categories to sites
  left_join(., class_equiv, by=c('SingleDomWetType' = 'ramsar_class_code')) %>%

  filter(SingleDomWetType %in% dom_types_tokeep) %>%
  mutate(ram_cont = Region)

  filter(rt_lin_bd > -9999) 
  #filter(InlandCateg==1 & HumanMadeCateg==0 & MarineCateg==0 & rt_lin_bd > -9999) %>%




ramsites <- unique(ramsites)



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
            filter(ori_ram_cnt != 0 & rt_lin_bd > quantile(ramsites$rt_lin_bd, probs=0.5))

# repalce all -9999 by NA, to be filtered after
ramsites[ ramsites == -9999 ] <- NA


# define wettype ---------------------------------------- 
ramsites$wettype <- ramsites$lvl1
#ramsites$wettype <- paste(ramsites$lvl1, ramsites$lvl2, sep=' - ')



# convert df to long format ----------------------------------------------------
ramsites_long <- ramsites %>%
  
  # remove some unnecessary columns
  select(-one_of("Subregion", "GeoPosition", "WI.Site.Reference",
                 "Total.site.area..ha.", "Transboundary", "IUCNcateg")) %>%
  
  
  gather(stressor, value, ram_1:rt_23) %>%
  separate(stressor, into=c("source","stressor_nb")) %>%
  spread(source, value) %>%
  filter(!is.na(rt))





# Del env var ----------------------------------------------------------------
remove(lvl1_tokeep, dom_types_tokeep, orig_ram_stress, ram_col_indices, class_equiv)




# nb.countries <- length(unique(ramsites$Country))
# nb.wet.types <- length(unique(ramsites$SingleDomWetType))
# unique(ramsites$SingleDomWetType)
# 
# 
# ramsites_wetclasscnt <-  ramsites %>%
#   group_by(lvl1) %>%
#   summarize(n= n())
