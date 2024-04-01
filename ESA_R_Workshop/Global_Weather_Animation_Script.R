### Clear environment
rm(list = ls())
invisible(lapply(paste0("package:", names(sessionInfo()$otherPkgs)), detach,character.only = T, unload = T))

### Load packages and set working directory
library(terra)
setwd('/Users/falvo/Desktop/Dissertation/intro_to_R_workshop/ESA_R_Workshop')

### Define API host and file attributes
url_name='https://psl.noaa.gov/thredds/fileServer/Datasets/20thC_ReanV3/Dailies/sfcFlxSI/'
file_name='dswrf.sfc.'
year=1980

### Download file, load file and plot animation
unlink(paste0(file_name,year,'.nc'))
system(paste0('wget ',url_name,file_name,year,'.nc'))
rast=flip(rast(paste0(file_name,year,'.nc')))
animate(rast,maxcell=1E5,pause=0.2,col=hcl.colors(50))
