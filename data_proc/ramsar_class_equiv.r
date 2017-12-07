
# get Ramsar Sites threat --------------------------------
ramsites=read.csv("../../Data/ramsar_sites_rt_ram_data_1june2015_wbdlin_mod_wyear.csv", 
                  stringsAsFactors = F)


# read file keeping only certain column.
class_equiv <- read.csv("../../data/ramsar_class_equivalence.csv", stringsAsFactors = F)


ramsites <- filter(ramsites, DomWetType != "")

ramsites<- ramsites %>%
           filter(!is.na(DomWetType)) %>%
           mutate(first_wet_categ = sapply(strsplit(gsub(" ", "", ramsites$DomWetType), ","), "[[", 1)) %>%
           left_join(., class_equiv, by=c('first_wet_categ' = 'ramsar_class_code'))





unique(class_equiv$ramsar_class_code)
unique(ramsites$first_wet_categ)
unique(ramsites$DomWetType)



ramsites_wetclasscnt <-  ramsites %>%
                         group_by(lvl1) %>%
                         summarize(n= n())



nb.countries <- length(unique(ramsites_subset$Country))

nb.wet.types <- length(unique(ramsites_subset$SingleDomWetType))


  
  


unique(ramsites_subset$SingleDomWetType)


