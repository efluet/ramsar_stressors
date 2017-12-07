

# get RT  for all pixels  ------------------------------------------------------

# read file keeping only certain column.
filename <- "../../data/river_network_stn30_variables_updated24nov2014_wlin_fixedon_31may2015.csv"
rt_allpix <- read.csv(filename, header=TRUE) #[ ,c('Lin_BD_Lin')] # add this commented part, to only keep this column

# rename column
names(rt_allpix)[names(rt_allpix) == 'Lin_BD_Lin'] <- 'rt_lin_bd'


n <- names(rt_allpix)
n[5:27] <- c("rt_1","rt_2","rt_3","rt_4","rt_5","rt_6","rt_7","rt_8","rt_9","rt_10",
             "rt_11","rt_12","rt_13","rt_14","rt_15","rt_16","rt_17","rt_18","rt_19","rt_20",
             "rt_21","rt_22","rt_23")
names(rt_allpix) <- n

### get continent separation of allpix continents for the STN30 pixels  #-----------------------
# (unsure if other attributes are incorrect in this file)
filename <- "../../data/River_Network_STN30_Variables_updated24Nov2014_wLin_9March2015.csv"
pix_country <- read.csv(filename, header=TRUE)[,c("ID","SubContinentName")]

rt_allpix <- merge(x = rt_allpix, y = pix_country, by = "ID", all.x = TRUE)




### Create lookup table of continents/regions - translated btwn RT and RAM -----

# get the continent list from the River threat data from the allpix data file
reg_lookup <- rt_allpix %>% 
  select(SubContinentName) %>% 
  distinct(SubContinentName) %>%
  filter(SubContinentName != "") %>%
  arrange(SubContinentName)

# Manually list a lookup column of continents matching Ramsar's
ram_cont <- c("Africa","Asia","Oceania","Europe","North America","Oceania",
              "Oceania","Oceania","North America","Oceania","Oceania","Neotropics")

# add the column to lookup table
reg_lookup <- reg_lookup %>% mutate(ram_cont = ram_cont)

# merge the ramsar regions to the all pix
rt_allpix <- merge(rt_allpix, reg_lookup, by="SubContinentName")





rm(filename, ram_cont, reg_lookup)
