library(ggfortify)
library('ggbiplot')
library(stats)
library(vegan)
# install.packages("eval", dependencies=TRUE, repos='http://cran.rstudio.com/')
library(MASS)







# NMDS on RT -------------------------------------------------------------
ramsites_rt <- ramsites %>%
                       dplyr::select("wettype", "rt_1","rt_2","rt_3", "rt_4","rt_5","rt_7","rt_9","rt_10",
                              "rt_11","rt_15","rt_16","rt_19","rt_20","rt_22","rt_23") %>% 
                       filter(rowSums(.[2:ncol(.)]) > 0) %>%
                       # filter(rowSums(.[31:45]) > 0) %>%
                       filter(rt_15 + rt_20 + rt_22 > 0) %>%
                       distinct() %>%
                       distinct("rt_1","rt_2","rt_3", "rt_4","rt_5","rt_7","rt_9","rt_10",
                                "rt_11","rt_15","rt_16","rt_19","rt_20","rt_22","rt_23")


ramsites_rt$rowsum <- ramsites_rt %>% mutate(rowSums(.[2:ncol(.)]))

rt_d <- dist(ramsites_rt[2:ncol(ramsites_rt)], method="euclidean")
fit <- isoMDS(rt_d, k=2)

  

names(ramsites_rt)


vare.mds <- metaMDS(ramsites_rt[2:ncol(ramsites_rt)])  #using all the defaults

data.scores <- as.data.frame(scores(vare.mds))  #Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scores$site <- rownames(data.scores)  # create a column of site names, from the rownames of data.scores
data.scores$grp <- ramsites_rt$wettype #grp  #  add the grp variable created earlier
head(data.scores)  #look at the data

species.scores <- as.data.frame(scores(vare.mds, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores$species <- rownames(species.scores)  # create a column of species, from the rownames of species.scores
head(species.scores)  #look at the data


#~~ scatter plot ----------------------------------
rt_nmds <- ggplot() + 

  geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2, colour=grp), size=3, shape=21, stroke=1.5, alpha=.8) + # add the point markers
  
  # add the species labels ?loading?
  geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species)) +
  
  #geom_text(data=data.scores,aes(x=NMDS1,y=NMDS2,label=site),size=6,vjust=0) +  # add the site labels

  coord_equal() +
  theme_bw() +
  theme(legend.position = 'top')

rt_nmds

#################################################


# NMDS on Ram -------------------------------------------------------------
ramsites_pcr <- ramsites %>%
  select("wettype","ram_1","ram_2","ram_3","ram_4","ram_5","ram_7","ram_9","ram_10",
         "ram_11","ram_15","ram_16","ram_19","ram_20","ram_22","ram_23") %>%
  filter(rowSums(.[2:ncol(.)]) > 0) 
  #filter(rt_15 + rt_20 + rt_22 > 0)


ramsites_pcr <- ramsites_pcr[1:100,]

names(ramsites_pcr)


vare.mds <- metaMDS(ramsites_pcr[2:ncol(ramsites_pcr)])  #using all the defaults

data.scores <- as.data.frame(scores(vare.mds))  #Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scores$site <- rownames(data.scores)  # create a column of site names, from the rownames of data.scores
data.scores$grp <- ramsites_pcr$wettype #grp  #  add the grp variable created earlier
head(data.scores)  #look at the data

species.scores <- as.data.frame(scores(vare.mds, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores$species <- rownames(species.scores)  # create a column of species, from the rownames of species.scores




#~~ scatter plot ----------------------------------
ram_nmds <- ggplot() + 
  
  geom_point(data=data.scores,
             aes(x=NMDS1,y=NMDS2, colour=grp), size=3, shape=21, stroke=1.5, alpha=.8) + # add the point markers
  
  # add the species labels ?loading?
  geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species)) +
  
  #geom_text(data=data.scores,aes(x=NMDS1,y=NMDS2,label=site),size=6,vjust=0) +  # add the site labels
  
  #coord_equal() +
  theme_bw() +
  xlim(-0.2, 0.15) +
  theme(legend.position = 'above')


ram_nmds



### Combine maps and plots -------------------------------------
library("gridExtra")
library("grid")
library("cowplot")

rt <- ggplotGrob(rt_nmds)
ram <- ggplotGrob(ram_nmds)

# t3 = arrangeGrob(g3, ncol=1)
# t4 = arrangeGrob(g4, ncol=1)


nmds_comb <- plot_grid(rt, ram, align = "h", nrow = 1)
nmds_comb



### Write figure to file ---------------------
ggsave(file="../../output/figures/nmds_comb_wettype.png", nmds_comb, 
       width=187, height=100, dpi=800, units="mm", type = "cairo-png")
dev.off()













# #
# m <- princomp(ramsites_pcr)
# df <- fortify(m, scale = 1)
# 
# ggplot(df, aes(x = Comp.1, y = Comp.2)) +
#   geom_point(aes(color=ramsites_subset$Region)) 
#   
#   
# wine.pca <- prcomp(wine, scale. = TRUE)
# ggbiplot(wine.pca, obs.scale = 1, var.scale = 1,
#          groups = wine.class, ellipse = TRUE, circle = TRUE) +
#   scale_color_discrete(name = '') +
#   theme(legend.direction = 'horizontal', legend.position = 'top')
