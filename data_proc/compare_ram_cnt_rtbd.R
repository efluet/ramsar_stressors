
### Linear model between number of positive aggregated ramsar stressors --------


# fit Poisson regression - because the count of Ramsar stressors is a count, not continuous measurement.
# scale2 <- function(x) {
#   sdx <- sqrt(var(x))
#   meanx <- mean(x)
#   return((x - meanx)/sdx)
# }
# elev50 <- scale2(elev50[])


fit <- glm(rt_lin_bd ~ agg_ram_cnt, data=inland_subset_joined, family = "poisson")
summary(fit) # show results

# from:  http://www.r-bloggers.com/poisson-regression-fitted-by-glm-maximum-likelihood-and-mcmc/
# I will then use the fitted model to make a smooth prediction curve of   \lambda_i  :
#   elev.seq <- seq(-3, 2, by = 0.05)
# new.data <- data.frame(elev50 = elev.seq, pow.elev50 = elev.seq^2)
# new.predict <- predict(m.glm, newdata = new.data, type = "response")
# And here I plot the data and the predicted   \lambda_i  (red line):
#   
#   plot(elev50, n50, cex = 1, col = "lightgrey", pch = 19, ylab = "# of Individuals", 
#        xlab = "Scaled Mean Elevation")
# lines(elev.seq, new.predict, col = "red", lwd = 2)


### Plot the scatterplot and regression line   #------------------------------------------------------------------

inland_subset_joined <- subset(inland_subset_joined, agg_ram_cnt > 0)

# plot the scatterplot and regression line
p1 <- ggplot(data = inland_subset_joined, 
        aes(x=ori_ram_cnt, y=rt_lin_bd)) +
  geom_point(colour = "#666666", fill = "#666666", size = 2, alpha = 8/20) +    # Use hollow circles
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE)    # Don't add shaded confidence region

# add title and axis labels 
p1 <- p1 + xlab("Number of original ramsar stressors")
p1 <- p1 + ylab("Biodiversity River Threat Index (linear transform)")
p1 <- p1 + theme_classic()
p1

inland_subset_joined <- subset(inland_subset_joined, ori_ram_cnt > 0)

# plot the scatterplot and regression line
p2 <- ggplot(data = inland_subset_joined, 
             aes(x=agg_ram_cnt, y=rt_lin_bd)) +
  geom_point(colour = "#666666", fill = "#666666", size = 2, alpha = 8/20) +    # Use hollow circles
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE)    # Don't add shaded confidence region

# add title and axis labels 
p2 <- p2 + xlab("Number of aggregated ramsar stressors")+
  ylab("Biodiversity River Threat Index (linear transform)") + 
  theme_classic()
p2

# make multiplot graph
source(".\\multiplot.r")
multiplot(p1, p2, cols=2)



# create outpout file, set dimensions --------------------------------------
tiff("..\\..\\output\\fig\\scatterplot_ramcnt_bdlin.tif",
     res=300,width=8.5,height=4,units='in', compression = c("none"))



# close graphical device
dev.off()

# delete environment variables
remove(p1, p2, multiplot)
