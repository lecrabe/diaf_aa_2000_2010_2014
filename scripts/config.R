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

datadir <- paste0(rootdir,"data/")
dir.create(datadir,showWarnings = F)

provdir <- paste0(rootdir,"data/provinces/")
dir.create(provdir,showWarnings = F)

imgdir <- paste0(rootdir,"img/")
dir.create(imgdir,showWarnings = F)

occ0010dir <- paste0(datadir,"occ_sols_2000_2010/")
occ1014dir <- paste0(datadir,"occ_sols_2010_2014/")

dir.create(occ0010dir,showWarnings = F)
dir.create(occ1014dir,showWarnings = F)

drvdir <- paste0(datadir,"resultats_drivers_20200406/")
dir.create(drvdir,showWarnings = F)

setwd(datadir)

source(paste0(scriptdir,"saea_function.R"),echo = T)

