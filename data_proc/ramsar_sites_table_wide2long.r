# Convert the Ramsar Site table from wide to long, 
# in order for the analysis by continent to be carried out. 


# create iterative file name
infile <- "../../Data/ramsar_sites_rt_ram_data_1june2015_wbdlin_mod.csv"
ramsites <- read.csv(infile, header=TRUE)
rm(infile)

ramsites_long <- ramsites %>%
            select(-one_of("SiteNo", "Country", "Region", "Subregion", 
                           "GeoPosition", "WI.Site.Reference", "Total.site.area..ha.", 
                           "Transboundary", "IUCNcateg", "HumanMadeCateg", "InlandCateg", 
                           "MarineCateg", "DomWetType", "SingleDomWetType", "rt_lin_bd")) %>%
  
            gather(stressor, value, ram_1:rt_23) %>%
            separate(stressor, into=c("source","stressor_nb")) %>%
            spread(source, value)


# write long table to file -----------------------------------------------------
write.csv(ramsites_long, 
          file = "../../Output/ramsar_sites_rt_ram_long_format.csv",
          row.names=FALSE)





