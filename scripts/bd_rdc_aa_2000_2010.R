######################################################################################################### 
##################### PARTIE I : COMBINER LES CARTES POUR 2000-2010
######################################################################################################### 

########## EXERCICE POUR AMELIE


######################################################################################################### 
##################### PARTIE II : EXPORTER LE FICHIER DE SUPERFICIES
######################################################################################################### 


##################### EXTRAIRE LA RESOLUTION
pix <- res(raster(paste0(rootdir,"process_2000_2010/diaf_2000_2010_provinces.tif")))[1]

##################### LIRE LA TABLE ET CALCULER LES SUPERFICIES
df <- read.table(paste0(rootdir,"stats_2000_2010.txt"))[,1:2]
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
write.csv(df2,"areas_2000_2010.csv",row.names = F)

df3 <- data.frame(cbind(1:3,tapply(df2$area_ha,df2$change,sum)))
names(df3) <- c("class","area")
write.csv(df3,"areas_2000_2010_national.csv",row.names = F)

######################################################################################################### 
##################### PARTIE III : EXPORTER LE FICHIER DE POINTS
######################################################################################################### 

##################### LIRE LES POINTS
df <- read.csv(paste0(rootdir,"process_2000_2010/BD_2000_2010_nerf_et_renforcement_pr_sepal_sans_doublons.csv"))
df$province <- df$prov/10
unique(df$province)

df$ce_prov  <- as.numeric(paste0(df$province,df$ce_change))
df$map_prov <- as.numeric(paste0(df$province,df$map_change))

##################### CHECK DOUBLONS
nrow(df) == length(unique(paste0(df$location_x,df$location_y)))

table(df$map_prov,df$ce_prov)
table(df$map_change,df$ce_change)

head(df)
write.csv(df,paste0(rootdir,"points_2000_2010_v20190206.csv"),row.names = F)

