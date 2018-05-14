
# Load libraries
library(raster)
library(rgdal)
library(dplyr)
library(dismo)
library(readr)
library(rgeos)
library(gtools)
library(rJava)

# Initial setup
options(java.parameters = '-Xmx2g') # Gigabytes to use in the ram memory

# Load data
occ <- read.csv('./presences_swd.csv') %>% rename(x = Lon, y = Lat)
bck <- read_csv('./background.csv')
gcms <- list.dirs('./_raster/_future/_2030s', full.names = TRUE)
load('./_maxent/model_maxent.rData')
yrs <- c('_2030s', '_2050s')

for(y in 1:length(yrs)){
  for(i in 1:length(gcms)){
  fls <- list.files('./_raster/_future/', yr, '/', gcms[1])
  lyr <- stack(fls)
  map.future <- predict(me, lyr, args=c("extrapolate=TRUE", "doclamp=TRUE"), progress="text")
  writeRaster(map.future, paste0('./_ maxent/map_', yr, '_', gcm, '.asc'), write.raster = FALSE)
  }
}

