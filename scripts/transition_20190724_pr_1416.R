####################################################################################
####### Object:  Fusion des deux cartes, nettoyage transitions         
####### Author:  remi.dannunzio@fao.org                               
####### Update:  2019/02/09                                    
####################################################################################


# ##################### PASSER LES PROVINCES EN 16Bit
# system(sprintf("gdal_translate -ot Int16 -co COMPRESS=LZW  %s %s",
#                paste0(datadir,"rdc_provinces.tif"),
#                paste0(datadir,"rdc_provinces_16b.tif")
# ))
# 
# ##################### PASSER LA CARTE JICA DIAF 2000-2010 EN 16Bit
# system(sprintf("gdal_translate -ot Int16 -co COMPRESS=LZW  %s %s",
#                paste0(datadir,"masque_NF_F_DEF_2000_2010_diaf_jica.tif"),
#                paste0(datadir,"diaf_0010_16b.tif")
# ))
# 
# ##################### PASSER LA CARTE JICA DIAF 2010-2014 EN 16Bit
# system(sprintf("gdal_translate -ot Int16 -co COMPRESS=LZW  %s %s",
#                paste0(datadir,"masque_NF_F_DEF_2010_2014_jica_diaf.tif"),
#                paste0(datadir,"diaf_1014_16b.tif")
# ))

######################################################################################################### 
##################### PARTIE I : COMBINER LES CARTES POUR 2000-2010-2014
######################################################################################################### 


##################### COMBINER LES TROIS CARTES
system(sprintf("gdal_calc.py -A %s -B %s -C %s --type=Int16 --co=\"COMPRESS=LZW\" --outfile=%s --calc=\"%s\"",
               paste0(datadir,"rdc_provinces_16b.tif"),
               paste0(datadir,"diaf_0010_16b.tif"),
               paste0(datadir,"diaf_1014_16b.tif"),
               paste0(datadir,"tmp_diaf_2000_2010_2014_provinces.tif"),
               "A*100+B*10+C"
))

################################################################################
#################### COMPRESSER LE RESULTAT
system(sprintf("gdal_translate -ot Int16 -co COMPRESS=LZW %s %s",
               paste0(datadir,"tmp_diaf_2000_2010_2014_provinces.tif"),
               paste0(datadir,"diaf_2000_2010_2014_provinces.tif")
))

system(sprintf("rm %s",
               paste0(datadir,"tmp_diaf_2000_2010_2014_provinces.tif")
))

##################### CALCULER LE COMPTAGE DE PIXEL
system(sprintf("oft-stat %s %s %s",
               paste0(datadir,"diaf_2000_2010_2014_provinces.tif"),
               paste0(datadir,"diaf_2000_2010_2014_provinces.tif"),
               paste0(datadir,"stats_2000_2010_2014.txt")
))

######################################################################################################### 
##################### PARTIE II : EXPORTER LE FICHIER DE SUPERFICIES ET NETTOYER LES TRANSITIONS
######################################################################################################### 

##################### EXTRAIRE LA RESOLUTION
pix <- res(raster(paste0(datadir,"diaf_2000_2010_2014_provinces.tif")))[1]

##################### LIRE LA TABLE ET CALCULER LES SUPERFICIES
df <- read.table(paste0(datadir,"stats_2000_2010_2014.txt"))[,1:2]
names(df) <- c("class","pixel")
df$area_ha <- df$pixel * pix*pix/10000

classes <- as.numeric(as.vector(outer(1:26, c(11,12,13,21,22,23,31,32,33), paste, sep="")))
classes <- classes[order(classes)]
classes

df$class[!(df$class %in% classes)]
df1 <- df

##################### EXTRAIRE LES CODES PROVINCES ET DE CHANGEMENT
df1$transition <- substr(df1$class,nchar(df1$class)-1,nchar(df1$class))
df1$province   <- as.numeric(substr(df1$class,0,nchar(df1$class)-2))
df1$class0010  <- as.numeric(substr(df1$transition,1,1))
df1$class1014  <- as.numeric(substr(df1$transition,2,2))

##################### FUSIONNER AVEC LES NOMS DES PROVINCES
codes <- read.dbf(paste0(datadir,"RDC_Province_26.dbf"))[,c("NOM","ID_SEPAL")]
df2   <- merge(df1,codes,by.y="ID_SEPAL",by.x="province",all.x=T)
df2   <- arrange(df2,class)[,c("class","NOM","province","pixel","transition","class0010","class1014","area_ha")]

tapply(df2$area_ha,df2[,c("class0010","class1014","NOM")],sum)
tapply(df2$area_ha,df2[,c("class0010","class1014")],sum)
summary(df2$province)

##################### CREER UNE NOUVELLE CLASSE 2010-2014 NETTOYEE
df2$clean1014 <- df2$class1014

############### "NON-FORET en 2010" et "FORET ou PERTE en 2014" --> "NON-FORET en 2014"
df2[df2$class0010 == 1 & df2$class1014 == 2,]$clean1014 <- 1
df2[df2$class0010 == 1 & df2$class1014 == 3,]$clean1014 <- 1

############### "FORET en 2010" et "NON-FORET en 2014" --> "DEFORESTATION en 2014"
df2[df2$class0010 == 2 & df2$class1014 == 1,]$clean1014 <- 3

############### "PERTE en 2010" et "FORET en 2014 ou PERTE en 2014" --> "NON-FORET en 2014"
df2[df2$class0010 == 3 & df2$class1014 == 2,]$clean1014 <- 1
df2[df2$class0010 == 3 & df2$class1014 == 3,]$clean1014 <- 1

##################### VERIFIER QUE TOUTES LES TRANSITIONS SONT MAINTENANT NETTOYEES
tapply(df2$area_ha,df2[,c("class0010","clean1014")],sum)

##################### COMBINER EN UN CODE DE RECLASSEMENT FINAL
df2[is.na(df2$province),]$province <- ""
df2$reclass <- paste0(df2$province,df2$class0010,df2$clean1014)
df2$transition_clean <- paste0(df2$class0010,df2$clean1014)

write.csv(df2,paste0(datadir,"areas_transitions_2000_2010_2014_20190724.csv"),row.names = F)
df2$reclass <- as.numeric(df2$reclass)
df2$transition_clean <- as.numeric(df2$transition_clean)

write.table(df2[,c("class","reclass","transition_clean")],
            paste0(datadir,"reclass.txt"),
            row.names = F,col.names = F)

################################################################################
#################### RECLASSIFIER 
system(sprintf("(echo %s; echo 1; echo 1; echo 2; echo 0) | oft-reclass  -oi %s  -um %s %s",
               paste0(datadir,"reclass.txt"),
               paste0(datadir,"tmp_diaf_2000_2010_2014_provinces_clean.tif"),
               paste0(datadir,"diaf_2000_2010_2014_provinces.tif"),
               paste0(datadir,"diaf_2000_2010_2014_provinces.tif")
))

################################################################################
#################### COMPRESSER LE RESULTAT
system(sprintf("gdal_translate -ot Int16 -co COMPRESS=LZW %s %s",
               paste0(datadir,"tmp_diaf_2000_2010_2014_provinces_clean.tif"),
               paste0(datadir,"diaf_2000_2010_2014_provinces_clean_20190724.tif")
))

system(sprintf("rm %s",
               paste0(datadir,"tmp_diaf_2000_2010_2014_provinces_clean.tif")
               ))

################################################################################
#################### RECLASSIFIER 
system(sprintf("(echo %s; echo 1; echo 1; echo 3; echo 0) | oft-reclass  -oi %s  -um %s %s",
               paste0(datadir,"reclass.txt"),
               paste0(datadir,"tmp_diaf_2000_2010_2014_clean.tif"),
               paste0(datadir,"diaf_2000_2010_2014_provinces.tif"),
               paste0(datadir,"diaf_2000_2010_2014_provinces.tif")
))

################################################################################
#################### COMPRESSER LE RESULTAT
system(sprintf("gdal_translate -ot Byte -co COMPRESS=LZW %s %s",
               paste0(datadir,"tmp_diaf_2000_2010_2014_clean.tif"),
               paste0(datadir,"tmp_byte_diaf_2000_2010_2014_clean.tif")
))


#################### CREATE A COLOR TABLE FOR THE OUTPUT MAP
my_classes <- c(0,11,22,23,31)
my_colors  <- col2rgb(c("black","grey","darkgreen","red","orange"))

pct <- data.frame(cbind(my_classes,
                        my_colors[1,],
                        my_colors[2,],
                        my_colors[3,]))

write.table(pct,paste0(datadir,"color_table.txt"),row.names = F,col.names = F,quote = F)


################################################################################
#################### Add pseudo color table to result
################################################################################
system(sprintf("(echo %s) | oft-addpct.py %s %s",
               paste0(datadir,"color_table.txt"),
               paste0(datadir,"tmp_byte_diaf_2000_2010_2014_clean.tif"),
               paste0(datadir,"tmp_byte_pct_diaf_2000_2010_2014_clean.tif")
))

################################################################################
#################### COMPRESS
################################################################################
system(sprintf("gdal_translate -ot Byte -co COMPRESS=LZW %s %s",
               paste0(datadir,"tmp_byte_pct_diaf_2000_2010_2014_clean.tif"),
               paste0(datadir,"diaf_2000_2010_2014_clean_20190724.tif")
))


system(sprintf("rm -f %s",
               paste0(datadir,"tmp_*.tif")
))

##################### CALCULER LE COMPTAGE DE PIXEL
system(sprintf("oft-stat %s %s %s",
               paste0(datadir,"diaf_2000_2010_2014_provinces_clean_20190724.tif"),
               paste0(datadir,"diaf_2000_2010_2014_provinces_clean_20190724.tif"),
               paste0(datadir,"stats_2000_2010_2014_clean_20190724.txt")
))

######################################################################################################### 
##################### PARTIE III : LIRE LES POINTS, REGENERER UNE BASE COMBINEE, SAEA FINAL
######################################################################################################### 
bd <- read.csv(paste0(datadir,"bd_2000_2010_2014_v20190531.csv"))
names(bd)
bd[bd$ce_change == 3 & bd$primary_land_cover1 == 1,]$ce_change <- 1
bd[bd$ce_change == 1,]$ce_transition <- 11
bd[bd$ce_change == 2,]$ce_transition <- 22
bd$ce_prov_transition <- paste0(bd$map_province,bd$ce_transition)

##################### TRANSFORMER LES POINTS EN FICHIER SPATIAL EXTRAIRE LES VALEURS DE LA CARTE DE TRANSITION
spdf_geo <- SpatialPointsDataFrame(
  coords = bd[,c("location_x","location_y")],
  data   = bd,
  proj4string=CRS("+init=epsg:4326")
)

map <- paste0(datadir,"diaf_2000_2010_2014_provinces_clean_20190724.tif")
spdf <- spTransform(spdf_geo,proj4string(raster(map)))

spdf@data$map_20190724 <- raster::extract(raster(map),spdf)
df <- spdf@data
table(df$map_20190724,df$prov_transition)

nrow(df[df$map_20190724 != df$prov_transition,])

all(df$map_20190724 == df$prov_transition)


write.csv(df,paste0(datadir,"bd_2000_2010_2014_v20190725.csv"),row.names = F)
