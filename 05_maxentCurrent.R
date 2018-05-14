
# Load libraries
library(raster)
library(rgdal)
library(dplyr)
library(dismo)
library(readr)
library(rgeos)
library(gtools)
library(rJava)
library(ecospat)

# Initial setup
options(java.parameters = '-Xmx2g') # Gigabytes to use in the ram memory

# Load data
occ <- read.csv('./presences_swd.csv') %>% rename(x = Lon, y = Lat)
bck <- read_csv('./background.csv')

fls <- list.files('./_raster/_climate/_current/_asc_bios', full.names = T, pattern = '.asc$') %>%
  mixedsort()
lyr <- stack(fls)

# Run Maxent - Way 1 (Normal)
dir.create('./_maxent/_models', recursive = TRUE)
me  <- maxent(lyr, occ[,1:2], remove.Duplicates = TRUE, path = './_maxent/_models')

# Run Maxent - Way 2 (SWD)
pres.covs <- extract(lyr, occ[,1:2], cellnumbers = T) %>%
  na.omit() %>%
  unique() %>%
  .[,-1] %>%
  cbind(occ[,1:2], .)

env.values <- data.frame(rbind(pres.covs, bck.covs))
y   <- c(rep(1, nrow(pres.covs)), rep(0, nrow(bck.covs)))
me  <- maxent(env.values, y, args = c('addsamplestobackground=true'), path = './_maxent/_robusta/_models/_run3')
map <- predict(me, lyr, progress = 'text')

writeRaster(map, './_maxent/_models/map.asc')
save(me, file = './_maxent/_models/mx_obj.RData')

# Evaluations using K-fold partioning
fold <- kfold(pres.covs, k = 5)
occtest <- pres.covs[fold == 1,]
occtrain <- pres.covs[fold != 1,]
y <- c(rep(1, nrow(occtrain)), rep(0, nrow(bck.covs)))
env.values <- data.frame(rbind(occtrain, bck.covs))

me <- maxent(env.values[,3:ncol(env.values)], y, args = c('addsamplestobackground=true'), path = './_maxent/_run2')
e  <- evaluate(me, p = data.frame(occtest[,3:ncol(occtest)]), a = data.frame(bck.covs[,3:ncol(bck.covs)]))

# Threshold value that maximizes Kappa
plot(e@t, e@kappa, type = 'l')
e@t[which.max(e@kappa)]

# Computing True Skill Statistic = TPR(Sensitivity)+TNR(Specificity)-1
tss <- e@TPR + e@TNR - 1
plot(e@t, tss, type = 'l')
e@t[which.max(tss)]

#AUC Plot: X=1-Specificity, Y=Sensitivity
plot((1 - e@TNR), e@TPR, type = 'l', xlab = 'Fractional Predicted Area (1 - Specificity)', ylab = 'Sensitiviy')
e@auc

# Now, for all folds
auc <- rep(NA, 5)
max.tss <- rep(NA,5)
maps <- list()
e <- list()
me <- list()
dir_out <- paste0('./_maxent/_models/_run2/_mod', 1:5)

for (i in 1:5){
  
  occtest <- pres.covs[fold == i, ]
  occtrain <- pres.covs[fold != i, ]
  env.values <- data.frame(rbind(occtrain, bck.covs))
  y  <- c(rep(1, nrow(occtrain)), rep(0, nrow(bck.covs)))
  me[[i]] <- maxent(env.values[,3:ncol(env.values)], y, args = c('addsamplestobackground=true'), path = dir_out[[i]])
  maps[[i]] <- predict(me[[i]], lyr)
  e[[i]] <- evaluate(me[[i]], p = data.frame(occtest[,3:ncol(occtest)]), a = data.frame(bck.covs[,3:ncol(bck.covs)]))
  auc[i] <- e[[i]]@auc
  lines((1 - e[[i]]@TNR), e[[i]]@TPR)
  tss <- e[[i]]@TPR + e[[i]]@TNR-1
  max.tss[i] <- e[[i]]@t[which.max(tss)]
  
}

map_avg <- mean(stack(maps))

Map('writeRaster', x = maps, filename = paste0('./_maxent/_results/map_run', 1:5, '.asc'))
writeRaster(map_avg, './_maxent/_results/_raw/map_avg.asc')

th_tss <- mean(max.tss)



