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
               paste0(datadir,"diaf_2000_2010_2014_provinces.tif"),
               "A*100+B*10+C"
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

############### "FORET en 2010" et "NON-FORET en 2014" --> "FORET en 2014"
df2[df2$class0010 == 2 & df2$class1014 == 1,]$clean1014 <- 2

############### "PERTE en 2010" et "FORET en 2014 ou PERTE en 2014" --> "NON-FORET en 2014"
df2[df2$class0010 == 3 & df2$class1014 == 2,]$clean1014 <- 1
df2[df2$class0010 == 3 & df2$class1014 == 3,]$clean1014 <- 1

##################### VERIFIER QUE TOUTES LES TRANSITIONS SONT MAINTENANT NETTOYEES
tapply(df2$area_ha,df2[,c("class0010","clean1014")],sum)

##################### COMBINER EN UN CODE DE RECLASSEMENT FINAL
df2[is.na(df2$province),]$province <- ""
df2$reclass <- paste0(df2$province,df2$class0010,df2$clean1014)
df2$transition_clean <- paste0(df2$class0010,df2$clean1014)

write.csv(df2,paste0(datadir,"areas_transitions_2000_2010_2014.csv"),row.names = F)
df2$reclass <- as.numeric(df2$reclass)

write.table(df2[,c("class","reclass")],
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
               paste0(datadir,"diaf_2000_2010_2014_provinces_clean.tif")
))

system(sprintf("rm %s",
               paste0(datadir,"tmp_diaf_2000_2010_2014_provinces_clean.tif")
               ))


##################### CALCULER LE COMPTAGE DE PIXEL
system(sprintf("oft-stat %s %s %s",
               paste0(datadir,"diaf_2000_2010_2014_provinces_clean.tif"),
               paste0(datadir,"diaf_2000_2010_2014_provinces_clean.tif"),
               paste0(datadir,"stats_2000_2010_2014_clean.txt")
))

######################################################################################################### 
##################### PARTIE III : LIRE LES POINTS, REGENERER UNE BASE COMBINEE, SAEA FINAL
######################################################################################################### 

##################### LIRE LES POINTS
bd0010 <- read.csv(paste0(datadir,"bd_2000_2010_v20190207.csv"))
bd1014 <- read.csv(paste0(datadir,"bd_2010_2014_v20190209.csv"))

head(bd1014)
bd0010$period <- "p0010"
bd1014$period <- "p1014"

##################### FUSIONNER LES BDD
bd <- rbind(bd0010,bd1014)
table(bd$operator)

##################### VERIFIER ET SUPPRIMER LES DOUBLONS ENTRE PERIODES
nrow(bd0010) == length(unique(paste0(bd0010$location_x,bd0010$location_y)))
nrow(bd1014) == length(unique(paste0(bd1014$location_x,bd1014$location_y)))

bd$loc <- paste0(bd$location_x,bd$location_y)
bd$doublon <- duplicated(bd$loc)
table(bd$doublon,bd$period)

bd1 <- bd[bd$doublon == FALSE,1:11]
nrow(bd1) == length(unique(paste0(bd1$location_x,bd1$location_y)))

##################### TRANSFORMER LES POINTS EN FICHIER SPATIAL EXTRAIRE LES VALEURS DE LA CARTE DE TRANSITION
spdf_geo <- SpatialPointsDataFrame(
  coords = bd1[,c("location_x","location_y")],
  data   = bd1,
  proj4string=CRS("+init=epsg:4326")
)

map <- paste0(datadir,"diaf_2000_2010_2014_provinces_clean.tif")
spdf <- spTransform(spdf_geo,proj4string(raster(map)))

spdf$prov_transition <- extract(raster(map),spdf)
df <- spdf@data


##################### HARMONISER LES NOMS DE PROVINCE
names(df)[4]
names(df)[4] <- "old_name"
df1 <- merge(df,codes,by.x="province",by.y="ID_SEPAL",all.x=T)
names(df1)
names(df1)[13] <- "nom"
table(df1$nom,df1$old_name)


##################### RE-EXTRAIRE LES CODES PROVINCES ET DE CHANGEMENT DE LA CARTE
df1$map_transition <- substr(df1$prov_transition,nchar(df1$prov_transition)-1,nchar(df1$prov_transition))
df1$map_province   <- as.numeric(substr(df1$prov_transition,0,nchar(df1$prov_transition)-2))
df1$map_class0010  <- as.numeric(substr(df1$map_transition,1,1))
df1$map_class1014  <- as.numeric(substr(df1$map_transition,2,2))

table(df1$map_class0010,df1$map_class1014)
all(df1$map_province==df1$province)

df1$ce_transition <- 0
df1[df1$period == "p0010" & df1$ce_change == 3,]$ce_transition <- 31
df1[df1$period == "p1014" & df1$ce_change == 3,]$ce_transition <- 23
df1[df1$ce_change == 1,]$ce_transition <- 11
df1[df1$ce_change == 2,]$ce_transition <- 22

table(df1$map_transition,df1$ce_transition)
df1$ce_prov_transition <- paste0(df1$province,df1$ce_transition)
head(df1)
df2 <- df1[,c("id","operator","location_x","location_y","province","nom","period","map_change","ce_change",
              "prov_transition","ce_prov_transition","map_transition","ce_transition","map_class0010","map_class1014")]
names(df2) <- c("id","operator","location_x","location_y","province_code","province_nom","periode","map_change","ce_change",
                "map_prov_transition","ce_prov_transition","map_transition","ce_transition","map_class0010","map_class1014")
write.csv(df2,paste0(datadir,"bd_2000_2010_2014_v20190210.csv"),row.names = F)
