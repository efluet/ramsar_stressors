
### Read the input file of Ramsar Sites  #-----------------------------------------------------------------------------
filename <- "..\\..\\data\\ramsar_sites_rt_ram_data_1june2015_wbdlin.csv"
file=read.csv(filename, header=TRUE)

# subset the input file to inland, natural wetlands with a non-zero river threat.
ramsite_subset <- subset(file, InlandCateg==1 & HumanMadeCateg==0 & MarineCateg==0 & rt_lin_bd > -9999)


# count number of aggregated ramsar stressors, by summing the binary ramsar reporting into a new column
ram_col_indices <- seq(17, 31, by=1)  # create sequence of column index wanted - referring to the 
ramsite_subset$agg_ram_cnt <- rowSums (ramsite_subset[,ram_col_indices], 
                                       na.rm = FALSE, 
                                       dims = 1)

#ramsite_subset_wcnt <- ramsite_subset[c("SiteRef", "rt_lin_bd", "agg_ram_cnt")]



### Read and count number of original ramsar stressors  #-------------------------------------------------------------


# read the input file
filename <- "..\\..\\data\\all_ramsar_stressor_count_persite_14may2015.csv"
orig_ram_stress=read.csv(filename, header=TRUE)

# sum the binary ramsar reporting into a new column
ram_col_indices <- seq(3, 119, by=1)
orig_ram_stress$ori_ram_cnt <- rowSums (orig_ram_stress[,ram_col_indices], na.rm = FALSE, dims = 1)

# subset the table to only the two field needed
orig_ram_stress <- orig_ram_stress[c("SiteRef","ori_ram_cnt")]

# join the count of original ramsar stressors to the inland subset of sites
ramsite_subset <- merge(x = ramsite_subset, y = orig_ram_stress, by = "SiteRef", all.x = TRUE)




### Read the input file for all pixels  #-----------------------------------------------------------------------------

# read file keeping only certain column.
filename <- "..\\..\\data\\river_network_stn30_variables_updated24nov2014_wlin_fixedon_31may2015.csv"
rt_allpix <- read.csv(filename, header=TRUE) #[ ,c('Lin_BD_Lin')] # add this commented part, to only keep this column

names(rt_allpix)[names(rt_allpix) == 'Lin_BD_Lin'] <- 'rt_lin_bd'




### read file that has continents for the STN30 pixels  #-------------------------------------------------------------------------------------
# (unsure if other attributes are incorrect in this file)
filename <- "..\\..\\data\\River_Network_STN30_Variables_updated24Nov2014_wLin_9March2015.csv"
pix_country <- read.csv(filename, header=TRUE)[,c("ID","SubContinentName")]

rt_allpix <- merge(x = rt_allpix, y = pix_country, by = "ID", all.x = TRUE)




### Create lookup table of continents/regions  #----------------------------------------------------------------------------------------------------------------

# get the continent list from the River threat data from the allpix data file
lookup <- rt_allpix %>% 
  select(SubContinentName) %>% 
  distinct(SubContinentName) %>%
  filter(SubContinentName != "") %>%
  arrange(SubContinentName)

# Manually list a lookup column of continents matching Ramsar's
ram_region <- c("Africa","Asia","Oceania","Europe","North America","Oceania","Oceania","Oceania","North America","Oceania","Oceania","Neotropics")

# add the column to lookup table
lookup <- lookup %>% 
  mutate(ram_region = ram_region)

# merge the ramsar regions to the all pix
rt_allpix <- merge(rt_allpix, lookup, by="SubContinentName")




### Delete environment variables   #------------------------------------------------------------------------------------
remove(file, filename, orig_ram_stress, ram_col_indices, pix_country)
