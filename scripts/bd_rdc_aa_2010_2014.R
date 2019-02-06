####################################################################################
####### Object:  Traitement de la BD RDC pour AA            
####### Author:  remi.dannunzio@fao.org                               
####### Update:  2019/02/05                                    
####################################################################################
library(foreign)
library(plyr)
library(rgeos)
library(rgdal)
library(raster)
library(ggplot2)


## Set the working directory
rootdir       <- "~/diaf_aa_2000_2010_2014/"
#rootdir <- "/media/dannunzio/OSDisk/Users/dannunzio/Documents/countries/congo_kinshasa/amelie/donnees_RDC_2000-2010-2014/"


## Go to the root directory
setwd(rootdir)
rootdir <- paste0(getwd(),"/")


######################################################################################################### 
##################### PARTIE I : COMBINER LES CARTES POUR 2010-2014
######################################################################################################### 

##################### RASTERISER LE SHAPEFILE DES PROVINCES SUR LA CARTE JICA DIAF
system(sprintf("oft-rasterize_attr.py -v %s -i %s -o %s  -a %s",
               paste0(rootdir,"provinces/RDC_Province_26.shp"),
               paste0(rootdir,"process_2010_2014/masque_NF_F_DEF_2010_2014_jica_diaf.tif"),
               paste0(rootdir,"provinces/rdc_provinces.tif"),
               "ID_SEPAL"
               ))

##################### PASSER LES PROVINCES EN 16Bit
system(sprintf("gdal_translate -ot Int16 -co COMPRESS=LZW  %s %s",
               paste0(rootdir,"provinces/rdc_provinces.tif"),
               paste0(rootdir,"provinces/rdc_provinces_16b.tif")
               ))

##################### PASSER LA CARTE JICA DIAF EN 16Bit
system(sprintf("gdal_translate -ot Int16 -co COMPRESS=LZW  %s %s",
               paste0(rootdir,"process_2010_2014/masque_NF_F_DEF_2010_2014_jica_diaf.tif"),
               paste0(rootdir,"process_2010_2014/diaf_1014_16b.tif")
               ))

##################### COMBINER LES DEUX CARTES
system(sprintf("gdal_calc.py -A %s -B %s --type=Int16 --co=\"COMPRESS=LZW\" --outfile=%s --calc=\"%s\"",
               paste0(rootdir,"provinces/rdc_provinces_16b.tif"),
               paste0(rootdir,"process_2010_2014/diaf_1014_16b.tif"),
               paste0(rootdir,"process_2010_2014/diaf_2010_2014_provinces.tif"),
               "A*10+B"
               ))

system(sprintf("rm %s",
               (paste0(rootdir,"process_2010_2014/diaf_1014_16b.tif"))
))


##################### IDENTIFIER TOUTES LES COMBINAISONS VALIDES
classes <- as.numeric(as.vector(outer(1:26, 1:3, paste, sep="")))
classes <- classes[order(classes)]
classes

##################### CALCULER LE COMPTAGE DE PIXEL
system(sprintf("oft-stat %s %s %s",
               paste0(rootdir,"process_2010_2014/diaf_2010_2014_provinces.tif"),
               paste0(rootdir,"process_2010_2014/diaf_2010_2014_provinces.tif"),
               paste0(rootdir,"stats_2010_2014.txt")
               ))

######################################################################################################### 
##################### PARTIE II : EXPORTER LE FICHIER DE SUPERFICIES
######################################################################################################### 


##################### EXTRAIRE LA RESOLUTION
pix <- res(raster(paste0(rootdir,"process_2010_2014/diaf_2010_2014_provinces.tif")))[1]

##################### LIRE LA TABLE ET CALCULER LES SUPERFICIES
df <- read.table(paste0(rootdir,"stats_2010_2014.txt"))[,1:2]
names(df) <- c("class","pixel")
df$area_ha <- df$pixel * pix*pix/10000

##################### SELECTIONNER UNIQUEMENT LES CLASSES VALIDES
df1 <- df[df$class %in% classes,]

##################### EXTRAIRE LES CODES PROVINCES ET DE CHANGEMENT
df1$change   <- substr(df1$class,nchar(df1$class),nchar(df1$class))
df1$province <- substr(df1$class,0,nchar(df1$class)-1)

##################### FUSIONNER AVEC LES NOMS DES PROVINCES
codes <- read.dbf(paste0(rootdir,"provinces/RDC_Province_26.dbf"))[,c("NOM","ID_SEPAL")]
df2   <- merge(df1,codes,by.y="ID_SEPAL",by.x="province",all.x=T)
df2   <- arrange(df2,class)[,c("class","NOM","province","change","pixel","area_ha")]

##################### VERIFIER LES TOTAUX
sum(df2$area_ha)
tapply(df2$area_ha,df2$NOM,sum)

##################### EXPORTER LE FICHIER
write.csv(df2,"areas_2010_2014.csv",row.names = F)


df3 <- data.frame(cbind(1:3,tapply(df2$area_ha,df2$change,sum)))
names(df3) <- c("class","area")
write.csv(df3,"areas_2010_2014_national.csv",row.names = F)

######################################################################################################### 
##################### PARTIE III : EXPORTER LE FICHIER DE POINTS
######################################################################################################### 

##################### LIRE LES POINTS
df <- read.csv(paste0(rootdir,"process_2010_2014/2010_2014_prov_V3_final_avc_set_oct_nov_dec_pa4_sans_doublons.csv"))
df$province <- df$prov/10
unique(df$province)

df$ce_prov  <- as.numeric(paste0(df$province,df$ce_change))
df$map_prov <- as.numeric(paste0(df$province,df$map_change))

##################### CHECK DOUBLONS
nrow(df) == length(unique(paste0(df$location_x,df$location_y)))

table(df$map_prov,df$ce_prov)
table(df$map_change,df$ce_change)

head(df)
write.csv(df,paste0(rootdir,"points_2010_2014_v20190206.csv"),row.names = F)

