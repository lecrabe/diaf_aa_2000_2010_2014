####################################################################################
####### Object:  Traitement de la BD RDC pour AA            
####### Author:  remi.dannunzio@fao.org                               
####### Update:  2019/02/05                                    
####################################################################################
options(stringsAsFactors = F)

library(foreign)
library(plyr)
library(rgeos)
library(rgdal)
library(raster)
library(ggplot2)


## Set the working directory
rootdir       <- "~/diaf_aa_2000_2010_2014/"

## Go to the root directory
setwd(rootdir)
rootdir <- paste0(getwd(),"/")

scriptdir <- paste0(rootdir,"scripts/")

datadir <- paste0(rootdir,"drc/")
dir.create(datadir,showWarnings = F)