
# Load libraries
require(raster)
require(rgdal)

# Initial setup
g <- gc(reset = T)
rm(list = ls())

# Load data
occFile <- "../presences.csv"
msk <- "./_raster/mask.asc" 
outBackName <- "./background.csv"
muestra <- nrow(read.csv(occFile) * 5)

selectBack <- function(occFile, outBackName, msk,muestra) {
  
  require(raster)
  require(rgdal)
  globZones <- raster(msk)
  spData <- read.csv(occFile)
  coords=coordinates(globZones)
  coord_pres=spData[,2:3]
  if(sum(is.na(extract(globZones,coord_pres)))>1){print("Valores de presencia fuera del area")
                                                  break}
  pos_pres=cellFromXY(globZones,coord_pres)
  unos=which(globZones[]==1) ##es dos debido a que mi area de estudio tiene valores iguales a 2 OJO
  todos=length(unos)-1
  
  pos_unos_pres=array(0,length(pos_pres))
  for(i in 1:length(pos_pres)){
    if(table(pos_pres[i]==unos)[1]==todos){
      pos_unos_pres[i]=which(unos==pos_pres[i])} else {pos_unos_pres[i]=0}}
  
  coords_extrac=coords[unos,]
  coords_extrac=coords_extrac[-pos_unos_pres,]
  coords_final=coords_extrac[sample(1:dim(coords_extrac)[1],muestra),]
  write.csv(coords_final, outBackName, quote=F, row.names=F)
}

selectBack(occFile, outBackName, msk,muestra)
