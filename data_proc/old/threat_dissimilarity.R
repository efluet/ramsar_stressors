
library("dplyr")
library("ecodist")

# read the data file of binary stressors
file=read.csv("../../data/ramsar_sites_rt_ram_data_1june2015_wbdlin.csv", header=TRUE)

#head(file)


# subset the data to site identifiers and ramsar reporting for each column
ram_rep <- file %>%
  select(SiteRef, contains("ram"))

ram_rep <- ram_rep[1:5,]

# convert 1st column to 
rownames(ram_rep) <- ram_rep[,1]
ram_rep[,1] <- NULL



# Calculate dissimilarity distance between sites
dist_mat <- as.matrix(distance(ram_rep, "sorensen"))

# to 
#m2 <- melt(m)[melt(upper.tri(m))$value,]
