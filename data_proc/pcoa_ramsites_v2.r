library(ggfortify)
library('ggbiplot')
library(stats)
library(vegan)
# install.packages("eval", dependencies=TRUE, repos='http://cran.rstudio.com/')
library(MASS)





# Mantel test loop - on wettypes -----------------------------------------------
mantel_test <- data.frame(subset=character(),
                     statistic=numeric(),
                     signif=numeric())


for (i in unique(ramsites$wettype)){
  
  t <- filter(ramsites, wettype == i)
  
  # subet rt threat
  t_rt <- t %>%
    dplyr::select("rt_1","rt_2","rt_3", "rt_4","rt_5","rt_7","rt_9","rt_10",
                 "rt_11","rt_15","rt_16","rt_19","rt_20","rt_22","rt_23") 
  d_rt <- dist(t_rt, method='euclidian')
  
  # subset ram threat
  t_ram <- t %>% 
    dplyr::select("ram_1","ram_2","ram_3","ram_4","ram_5","ram_7","ram_9","ram_10",
                  "ram_11","ram_15","ram_16","ram_19","ram_20","ram_22","ram_23")
  d_ram <- dist(t_rt, method='binary')
  
  # run mantel test
  m <- mantel(d_rt, d_ram)
  
  
  l <- data.frame(subset=i,
                  statistic=m$statistic,
                  signif=m$signif)
  
  mantel_test <- rbind(mantel_test, l)
  
}





# run Procrustes significance test
protest(ramsites_ram[2:ncol(ramsites_ram)], 
        ramsites_rt[2:ncol(ramsites_rt)], 
        symmetric=FALSE,
        permutations = 999) #, scores = "sites", , strata




# NMDS on RT -------------------------------------------------------------

ramsites_rt <- ramsites %>%
  
  dplyr::select("wettype", "rt_1","rt_2","rt_3", "rt_4","rt_5","rt_7","rt_9","rt_10",
                "rt_11","rt_15","rt_16","rt_19","rt_20","rt_22","rt_23")
  

ramsites_rt <- ramsites %>%
                    
                       dplyr::select("wettype", "rt_1","rt_2","rt_3", "rt_4","rt_5","rt_7","rt_9","rt_10",
                              "rt_11","rt_15","rt_16","rt_19","rt_20","rt_22","rt_23") %>% 
                       filter(rowSums(.[2:ncol(.)]) > 0) %>%
                       # filter(rowSums(.[31:45]) > 0) %>%
                       filter(rt_15 + rt_20 + rt_22 > 0) %>%
                       distinct() #%>%
                       
ramsites_rt <- ramsites_rt %>%

               distinct(rt_1,rt_2,rt_3,rt_4,rt_5,rt_7,rt_9,rt_10,
                        rt_11,rt_15,rt_16,rt_19,rt_20,rt_22,rt_23)

# calc row sum
#ramsites_rt$rowsum <- rowSums(ramsites_rt[2:ncol(ramsites_rt)])




rt_d <- dist(ramsites_rt[2:ncol(ramsites_rt)], method="euclidean")
fit <- isoMDS(rt_d, k=2)
fit
  
x<-fit$points[,1]
y<-fit$points[,2]

ggplot(fit, aes(x,y)) + geom_point()



library('ggvegan')
rt_d <- vegdist(ramsites_rt[2:ncol(ramsites_rt)], method="bray", k=2)
fit <- metaMDS(rt_d)
#scrs <- as.data.frame(scores(sol, display = "sites"))
plot(fit)



fita <- fortify(fit)



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

  xlim(-0.5, 1) +
  ylim(-1, 1) +
  #coord_equal() +
  theme_bw() +
  theme(legend.position = 'top')

rt_nmds

#################################################


# NMDS on Ram -------------------------------------------------------------
ramsites_ram <- ramsites %>%
  dplyr::select("wettype","ram_1","ram_2","ram_3","ram_4","ram_5","ram_7","ram_9","ram_10",
         "ram_11","ram_15","ram_16","ram_19","ram_20","ram_22","ram_23") %>%
  filter(rowSums(.[2:ncol(.)]) > 0) 
  #filter(rt_15 + rt_20 + rt_22 > 0)


#ramsites_pcr <- ramsites_pcr[1:100,]

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
  
  #geom_text(data=data.scores, aes(x=NMDS1,y=NMDS2,label=species),size=6,vjust=0) +  # add the site labels
  
  #coord_equal() +
  theme_bw() +
  #xlim(-0.2, 0.15) +
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







# PCA plots w/ loading arrows ---------------------------------------------------
autoplot(prcomp(ramsites_rt[c(2,3,4,5,6,7,8,9,10,11,12,13,15,16)]), data = ramsites_rt, colour = 'wettype',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)



autoplot(prcomp(ramsites_ram[2:ncol(ramsites_ram)]), data = ramsites_ram, colour = 'wettype',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)








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
