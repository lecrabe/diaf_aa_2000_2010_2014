####################################################################################
####### Object:  EVALUATION PRECISION BDD TRANSITION        
####### Author:  remi.dannunzio@fao.org                               
####### Update:  2019/02/05                                    
####################################################################################

##### LIRE LA BDD
df0 <- read.csv2(paste0(datadir,"bd_00_10_14_20190425_aa.csv"))

##### CREER UN VRAI IDENTIFIANT UNIQUE
df0$unique_id <- row(df0)[,1]

##### REMPLACER LA VALEUR NA pour l'année destination par 2010 (seulement point avec 2000 en year)
df0[is.na(df0$year1),]$year1 <- 2010

##### CREER UN IDENTIFIANT PERIODE
df0$periode <- paste0(df0$year,"_",df0$year1)

##### CONVERTIR LES COORDONNEES EN NUMERIQUE
df0$location_x <- as.numeric(df0$location_x)
df0$location_y <- as.numeric(df0$location_y)

##### CREER UN IDENTIFIANT SUR BASE DES COORDONNEES
df0$loc <- paste0(df0$location_x,df0$location_y)
length(unique(df0$loc))

##### VERIFIER QUE LES COORDONNEES NE SONT PAS STANDARDISÉES
df0$nchar_loc <- nchar(df0$loc)
table(df0$periode)
hist(df0$nchar_loc)

##### SPATIALISER LES POINTS
spdf <- SpatialPointsDataFrame(
  coords = df0[,c("location_x","location_y")],
  data   = df0,
  proj4string=CRS("+init=epsg:4326")
)

##### CREER UN BUFFER DE 1 METRE AUTOUR DES POINTS
buffer    <- buffer(spdf,1,dissolve=F)
buf       <- SpatialPolygonsDataFrame(buffer,
                                   spdf@data,
                                   match.ID = F)

##### COMPTER LE NOMBRE DE POINTS QUI TOMBE DANS CHAQUE POLYGONE
over      <- aggregate(x = spdf["unique_id"],by = buf,FUN = length)

##### AJOUTER LE COMPTE DES POINTS COMME ATTRIBUT DE CHAQUE POLYGONE
buf$count <- over@data$unique_id

##### VOIR LE NOMBRE DE DOUBLONS
table(buf$periode,buf$count)
table(buf$count)/1:10

##### VERIFIER QUE LES POINTS ET LES POLYGONES SONT DANS LE MEME ORDRE
summary(buf$unique_id - spdf$unique_id)

##### AJOUTER LE COMPTE DES POINTS COMME ATTRIBUT DE CHAQUE POINT
spdf$count <- buf$count

##### CONVERTIR LES POINTS EN TABLE
df <- spdf@data

##### VERIFIER QUE LES TRANSITIONS COLLENT AVEC LES CATEGORIES
table(df$primary_land_cover_label,df$primary_land_cover1_label,df$ce_change)

##### APPELER LA CARTE
map <- paste0(datadir,"diaf_2000_2010_2014_provinces_clean.tif")

##### REPROJETER DANS LE SYSTEM DE LA CARTE
spdf_merc <- spTransform(spdf,proj4string(raster(map)))

##### EXTRAIRE POUR CHAQUE POINT DE LA BDD LE CODE DE LA CARTE PROVINCIALE
spdf_merc$prov_transition <- extract(raster(map),spdf_merc)

df <- spdf_merc@data

##################### EXTRAIRE LES CODES PROVINCES ET DE CHANGEMENT DE LA CARTE
df$map_transition <- as.numeric(substr(df$prov_transition,nchar(df$prov_transition)-1,nchar(df$prov_transition)))
df$map_province   <- as.numeric(substr(df$prov_transition,0,nchar(df$prov_transition)-2))
df$map_class0010  <- as.numeric(substr(df$map_transition,1,1))
df$map_class1014  <- as.numeric(substr(df$map_transition,2,2))

table(df$map_province)
table(df$map_transition)

##################### FUSIONNER AVEC LES NOMS DES PROVINCES
codes <- read.dbf(paste0(datadir,"RDC_Province_26.dbf"))[,c("NOM","ID_SEPAL")]
names(codes) <- c("province_name","ID_SEPAL")
df1   <- merge(df,codes,by.y="ID_SEPAL",by.x="map_province",all.x=T)
df1   <- arrange(df1,unique_id)

table(df1$map_province,df1$province_name)
table(df1$map_class0010,df1$map_class1014)

df1$ce_transition <- 0
df1[df1$periode == "2000_2010" & df1$ce_change == 3,]$ce_transition <- 31
df1[df1$periode == "2010_2014" & df1$ce_change == 3,]$ce_transition <- 23
df1[df1$periode == "2000_2010" & df1$ce_change == 1,]$ce_transition <- 11
df1[df1$periode == "2010_2014" & df1$ce_change == 2,]$ce_transition <- 22

table(df1$map_transition,df1$ce_transition)

df1$ce_prov_transition <- paste0(df1$map_province,df1$ce_transition)

table(df1$prov_transition,df1$ce_prov_transition)

write.csv(df1,paste0(datadir,"bd_2000_2010_2014_v20190426.csv"),row.names = F)



