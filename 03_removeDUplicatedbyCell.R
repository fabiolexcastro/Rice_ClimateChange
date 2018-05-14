
# Load libraries
require(raster)
require(rgdal)
require(tidyverse)

# Initial setup
g <- gc(reset = TRUE)
rm(list = ls())

# Function to use
dup_cell <- function(path_mask, path_df){
  mask <- raster(path_mask)
  df <- read.csv(path_df)
  cellNum <- raster::extract(mask, df[,c('Lon', 'Lat')], cellnumbers = T) 
  cells <- xyFromCell(mask, cellNum[,'cells'])
  dupvec <- duplicated(cells[,c('x', 'y')])
  occ_rmDupCell <- tbl_df(df[!dupvec,])
  occ_DupCell <- tbl_df(df[dupvec,])
  return(list(occ_rmDupCell, occ_DupCell))
}

# Apply the function
dup_cell(path_mask = './_raster/mask.asc', path_df = './presences.csv')
