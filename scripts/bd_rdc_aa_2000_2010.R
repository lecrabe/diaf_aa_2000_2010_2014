######################################################################################################### 
##################### PARTIE I : COMBINER LES CARTES POUR 2000-2010
######################################################################################################### 

######################################################################################################### 
##################### PARTIE I : COMBINER LES CARTES POUR 2000-2010
######################################################################################################### 

##################### RASTERISER LE SHAPEFILE DES PROVINCES SUR LA CARTE JICA DIAF
system(sprintf("python %s/oft-rasterize_attr.py -v %s -i %s -o %s  -a %s",
               scriptdir,
               paste0(datadir,"RDC_Province_26.shp"),
               paste0(datadir,"masque_NF_F_DEF_2000_2010_diaf_jica.tif"),
               paste0(datadir,"rdc_provinces.tif"),
               "ID_SEPAL"
))

##################### PASSER LES PROVINCES EN 16Bit
system(sprintf("gdal_translate -ot Int16 -co COMPRESS=LZW  %s %s",
               paste0(datadir,"rdc_provinces.tif"),
               paste0(datadir,"rdc_provinces_16b.tif")
))

##################### PASSER LA CARTE JICA DIAF EN 16Bit
system(sprintf("gdal_translate -ot Int16 -co COMPRESS=LZW  %s %s",
               paste0(datadir,"masque_NF_F_DEF_2000_2010_diaf_jica.tif"),
               paste0(datadir,"diaf_0010_16b.tif")
))

##################### COMBINER LES DEUX CARTES
system(sprintf("gdal_calc.py -A %s -B %s --type=Int16 --co=\"COMPRESS=LZW\" --outfile=%s --calc=\"%s\"",
               paste0(datadir,"rdc_provinces_16b.tif"),
               paste0(datadir,"diaf_0010_16b.tif"),
               paste0(datadir,"diaf_2000_2010_provinces.tif"),
               "A*10+B"
))

system(sprintf("rm %s",
               (paste0(datadir,"diaf_0010_16b.tif"))
))

##################### IDENTIFIER TOUTES LES COMBINAISONS VALIDES
classes <- as.numeric(as.vector(outer(1:26, 1:3, paste, sep="")))
classes <- classes[order(classes)]
classes

##################### CALCULER LE COMPTAGE DE PIXEL
system(sprintf("oft-stat %s %s %s",
               paste0(datadir,"diaf_2000_2010_provinces.tif"),
               paste0(datadir,"diaf_2000_2010_provinces.tif"),
               paste0(datadir,"stats_2000_2010.txt")
))
######################################################################################################### 
##################### PARTIE II : EXPORTER LE FICHIER DE SUPERFICIES
######################################################################################################### 


##################### EXTRAIRE LA RESOLUTION
pix <- res(raster(paste0(datadir,"diaf_2000_2010_provinces.tif")))[1]

##################### LIRE LA TABLE ET CALCULER LES SUPERFICIES
df <- read.table(paste0(datadir,"stats_2000_2010.txt"))[,1:2]
names(df) <- c("class","pixel")
df$area_ha <- df$pixel * pix*pix/10000

##################### SELECTIONNER UNIQUEMENT LES CLASSES VALIDES
df1 <- df[df$class %in% classes,]

##################### EXTRAIRE LES CODES PROVINCES ET DE CHANGEMENT
df1$change   <- substr(df1$class,nchar(df1$class),nchar(df1$class))
df1$province <- substr(df1$class,0,nchar(df1$class)-1)

##################### FUSIONNER AVEC LES NOMS DES PROVINCES
codes <- read.dbf(paste0(datadir,"RDC_Province_26.dbf"))[,c("NOM","ID_SEPAL")]
df2   <- merge(df1,codes,by.y="ID_SEPAL",by.x="province",all.x=T)
df2   <- arrange(df2,class)[,c("class","NOM","province","change","pixel","area_ha")]

##################### VERIFIER LES TOTAUX
sum(df2$area_ha)
tapply(df2$area_ha,df2$NOM,sum)

##################### EXPORTER LE FICHIER
write.csv(df2,paste0(datadir,"areas_2000_2010.csv"),row.names = F)

df3 <- data.frame(cbind(1:3,tapply(df2$area_ha,df2$change,sum)))
names(df3) <- c("class","area")
write.csv(df3,paste0(datadir,"areas_2000_2010_national.csv"),row.names = F)

######################################################################################################### 
##################### PARTIE III : EXPORTER LE FICHIER DE POINTS
######################################################################################################### 

##################### LIRE LES POINTS
df <- read.csv(paste0(datadir,"BD_2000_2010_nerf_et_renforcement_pr_sepal_sans_doublons.csv"))
df$province <- df$prov/10
unique(df$province)

df$ce_prov  <- as.numeric(paste0(df$province,df$ce_change))
df$map_prov <- as.numeric(paste0(df$province,df$map_change))

##################### TRANSFORMER LES POINTS EN FICHIER SPATIAL
spdf_geo <- SpatialPointsDataFrame(
  coords = df[,c("location_x","location_y")],
  data   = df,
  proj4string=CRS("+init=epsg:4326")
)

map_org <- paste0(datadir,"masque_NF_F_DEF_2000_2010_diaf_jica.tif")
map_prv <- paste0(datadir,"diaf_2000_2010_provinces.tif")

spdf <- spTransform(spdf_geo,proj4string(raster(map_prv)))

##################### EXTRAIRE LES VALEURS DES POINTS
spdf$map_org_change <- extract(raster(map_org),spdf)
spdf$map_prv_change <- extract(raster(map_prv),spdf)

spdf$check_change   <- substr(spdf$map_prv_change,nchar(spdf$map_prv_change),nchar(spdf$map_prv_change))

table(spdf$map_org_change,spdf$check_change)
table(spdf$map_org_change,spdf$map_change)

df <- spdf@data

##################### CHECK DOUBLONS
nrow(df) == length(unique(paste0(df$location_x,df$location_y)))
names(df)
table(df$map_prv_change,df$ce_prov)
table(df$map_org_change,df$ce_change)
table(df$map_org_change,df$map_change)
head(df)
write.csv(df,paste0(datadir,"points_2000_2010_v20190207.csv"),row.names = F)

##################### FICHIER AMELIE
am <- read.csv(paste0(datadir,"bd_extract_value_00_10_pr_sepal.csv"))
table(df$map_org_change,am$RASTERVALU)
head(am)

out <- df[,c("operator","id","province","nom","location_x","location_y","ce_change","map_org_change","ce_prov","map_prv_change")]
names(out) <- c("operator","id","province","nom","location_x","location_y","ce_change","map_change","ce_prov","map_prov")

write.csv(out,paste0(datadir,"bd_2000_2010_v20190207.csv"),row.names = F)
