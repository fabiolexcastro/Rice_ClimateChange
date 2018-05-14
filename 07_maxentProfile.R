
# Load libraries
library(raster)
library(dismo)
library(usdm)
library(rgdal)

# Read occurrences and background
occ <- read.csv('./presences_swd.csv')
bck <- read.csv('./bck_swd.csv')

# Cargar rasters climáticos
current <- list.files('./_raster/_current', full.names = T, pattern='.asc$')
current <- raster::stack(current)

crossValDir <- './Profiles/current'
if(!file.exists(crossValDir)){dir.create(crossValDir)}

# Partitions y the data
tryCatch(expr={
  fit.current <- dismo::maxent(x=current,
                               p=occ[,c("Long","Lat")],
                               a=bck[,c("Long","Lat")], removeDuplicates=T,
                               args=c("nowarnings","replicates=5","linear=true","quadratic=true","product=true","threshold=false","hinge=false","pictures=false","plots=false"),
                               path=crossValDir)
},
error=function(e){
  cat("Modeling process failed\n")
  return("Done\n")
})

namPredic   <- names(occ)[-c(1:3)] # namPredic <- names(normMat)[-ncol(normMat)] Nombres de variables
dataSetStep <- occ[namPredic] # Selección de datos
nColums     <- ncol(dataSetStep)

mindss <- apply(dataSetStep,2,min) # Extraer mínimos
maxdss <- apply(dataSetStep,2,max) # Extraer máximos

matMin <- matrix(mindss,100,nColums,byrow = T)
matQ1  <- matrix(apply(dataSetStep,2,function(x){quantile(x,0.25)}), 100, nColums, byrow = T) # Extraer el primer cuartil Q1
matMed <- matrix(apply(dataSetStep,2,median),100,nColums,byrow = T) # Extraer la mediana
matQ3  <- matrix(apply(dataSetStep,2,function(x){quantile(x,0.75)}),100,nColums,byrow = T) # Extraer el tercer cuartil Q3

rangd <- maxdss - mindss
mind <- mindss

# Calcular perfiles por variable
for(j in 1:nColums)
{
  matMax <- matrix(maxdss, 100, nColums, byrow = T)
  
  xProf      <- seq(mindss[j], maxdss[j], length.out = 100)  
  xProfDesn  <- (xProf-0)*(rangd[j])/1+mind[j]
  listMats <- list(matMin, matQ1, matMed, matQ3, matMax)
  
  # Crear coordenadas ficticias
  set.seed(1235)
  coords <- sampleRandom(x=current, 100, xy=T)
  coords <- as.data.frame(coords[,c('x','y')])
  names(coords) <- c('Long','Lat')
  
  # Unión de matriz con las coordenadas ficticias
  listMats <- lapply(1:length(listMats), function(i){
    w <- listMats[[i]]
    w <- as.data.frame(w)
    names(w) <- namPredic
    w[,j] <- xProf
    z <- cbind(coords,w)
    return(z)
  })
  
  # Visualización de los datos
  plot(current[[2]])
  points(coords)
  coords$Value <- xProf
  
  # Creación de rasters con valores constantes según sea el caso: min, Q1, median, Q3, max
  template <- current[[1]]
  template[] <- NA # Raster vacio
  
  rList <- lapply(1:length(listMats), function(i){
    rCreate <- lapply(1:length(namPredic), function(k){
      z <- template
      z[cellFromXY(object=z, xy=listMats[[i]][,c('Long','Lat')])] <- unique(listMats[[i]][,namPredic[k]])
      # table(template[]); verificar valores dentro del raster
      return(z)
    })
    return(rCreate)
  })
  rm(template)
  
  rList <- lapply(1:length(rList), function(i){z <- stack(rList[[i]]); return(z)}) # Lista de stacks con valores correspondientes a las coordenadas ficticias
  
  listFitted <- apply(sapply(1:length(rList), function(x){
    
    z <- rList[[x]]
    names(z) <- names(current)
    predRaster <- predict(fit.current, z)
    predRaster <- mean(predRaster)
    
    dataTable <- cbind(xyFromCell(object=predRaster, cell=1:ncell(predRaster)), predRaster[])
    dataTable <- as.data.frame(dataTable)
    names(dataTable) <- c('Long','Lat','Idoneidad') # Cambiar nombres
    dataTable <- dataTable[complete.cases(dataTable),]
    rownames(dataTable) <- 1:nrow(dataTable)
    
    mainResults <- merge(dataTable, coords, by=c('Long','Lat')) # Como si fuera un vlookup en excel
    y <- data.frame(Value=mainResults$Value, Idoneidad=mainResults$Idoneidad)
    y <- y[order(y$Idoneidad),]
    y <- y$Idoneidad
    write.csv(mainResults, paste(crossValDir, '/fitted_values_maxent_',namPredic[j],'.csv', sep=''), row.names=F)
    return(y)}),1,median)
  
  # plot(xProf, listFitted) #este es el perfil final que es producto de la mediana para la combinacion de los 5 valores (min, q1, mediana, q3, y max)
  # listFitted <- apply(sapply(listMats,function(x){z <- as.data.frame(x); z[,j] <- xProf; colnames(z) <- namPredic; y <- predict(fit.current,z);return(y)}),1,median)
  # fitteDesn <-(listFitted+0)*(rangd[ncol(normMat)])/1+mind[ncol(normMat)]
  
  profDir <- paste(crossValDir, '/profilesPlots', sep='')
  if(!file.exists(profDir)){dir.create(profDir)} else{cat("")}
  
  png(paste(profDir,"/perfil_",namPredic[j],".png",sep=""), width=2000, height=2000, units='px', pointsize=13, res=300)
  #layout(matrix(c(1,2),ncol=2,nrow=1))
  #plot(data[,j],data[,ncol(data)],pch=21,cex=0.8,bg="azure3",col="azure3",ylab=ylabs,ylim=c(min(data[,ncol(data)]),max(data[,ncol(data)])),xlab=namPredic[j],main=namPredic[j])
  #points(xProfDesn,fitteDesn,bg="blue",col="blue",pch=21)
  #plot(xProfDesn,fitteDesn,type="l",col=0,ylab=ylabs,xlab=namPredic[j],main="Profile Zoom")
  plot(xProf, listFitted, ylim=c(0,1), ylab='Suitability', xlab=namPredic[j], type="l", col=0, main="")
  lines(supsmu(xProf,listFitted),lwd=2,col="forestgreen")
  dev.off()
  
}
