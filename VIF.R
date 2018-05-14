
# Load libraries
require(raster)
require(rgdal)
require(dismo)
require(usdm)

# Initial Setup
g <- gc(reset = TRUE)
rm(list = ls())
setwd('D:/myDyrectory/')

# VIF Analysis
zone <- read.csv("../4_rice_swd.csv")
vif.res <- vif(x=zone[,4:31])
vif.step <- vifstep(x=zone[,4:31], th=10)
result<-vif.step@results$Variables
write.csv(vif.res, "../vif_variables.csv")
