
# Load libraries
require(raster)
require(rgdal)
require(dismo)

# Initial setup
g <- gc(reset = T)
rm(list = ls())

# Load data
points <- read.csv('./presences.csv', header=T)
fls <- list.files('./_climate/_current/_asc', full.names = T, pattern = '.asc$')

# Stack and extraction by the coordinates (presences)
data <- lapply(current, FUN = raster)
data <- stack(data)
coor <- extract(data, points[,2:3]) # COlumns 2 and 3: longitude and latitude
coordinate <- cbind(points, Coor)
write.csv(coordinate,'./presences_swd.csv', row.names = F)
