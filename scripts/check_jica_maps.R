####################################################################################
####### Object:  Verifier cartes de JICA separees        
####### Author:  remi.dannunzio@fao.org                               
####### Update:  2019/02/11                                 
####################################################################################


system(sprintf("gdal_translate -a_nodata none -co COMPRESS=LZW %s %s",
               paste0(datadir,"JICA_2000-2010_reclass_WM_TIF.tif"),
               paste0(datadir,"jica_2000_2010.tif")
))

system(sprintf("gdal_translate -a_nodata none -co COMPRESS=LZW %s %s",
               paste0(datadir,"JICA_2010_2014_WM_RECLASS.tif"),
               paste0(datadir,"jica_2010_2014.tif")
))


##################### COMBINER LES DEUX CARTES JICA
system(sprintf("gdal_calc.py -A %s -B %s  --type=Byte --co=\"COMPRESS=LZW\" --outfile=%s --calc=\"%s\"",
               paste0(datadir,"jica_2000_2010.tif"),
               paste0(datadir,"jica_2010_2014.tif"),
               paste0(datadir,"tmp_jica_2000_2010_2014.tif"),
               "A*10+B"
))

##################### CALCULER LE COMPTAGE DE PIXEL
system(sprintf("oft-stat %s %s %s",
               paste0(datadir,"tmp_jica_2000_2010_2014.tif"),
               paste0(datadir,"tmp_jica_2000_2010_2014.tif"),
               paste0(datadir,"stats_jica_2000_2010_2014.txt")
))


######################################################################################################### 
##################### PARTIE II : EXPORTER LE FICHIER DE SUPERFICIES ET NETTOYER LES TRANSITIONS
######################################################################################################### 

##################### EXTRAIRE LA RESOLUTION
pix <- res(raster(paste0(datadir,"tmp_jica_2000_2010_2014.tif")))[1]

##################### LIRE LA TABLE ET CALCULER LES SUPERFICIES
df <- read.table(paste0(datadir,"stats_jica_2000_2010_2014.txt"))[,1:2]
names(df) <- c("class","pixel")
df$area_ha <- df$pixel * pix*pix/10000

df1 <- df

##################### EXTRAIRE LES CODES PROVINCES ET DE CHANGEMENT
df1$class0010  <- as.numeric(substr(df1$class,1,1))
df1$class1014  <- as.numeric(substr(df1$class,2,2))
tapply(df1$area_ha,df1[,c("class0010","class1014")],sum)


