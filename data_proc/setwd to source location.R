### run all - executes other scripts of the project

library(rstudioapi)

# Set wd  to source of script   #------------------------
dirname <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dirname)
rm(dirname)

#this.dir <- dirname(parent.frame(2)$ofile)
#setwd(this.dir)