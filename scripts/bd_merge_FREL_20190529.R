####################################################################################
####### Object:  EVALUATION PRECISION BDD TRANSITION        
####### Author:  remi.dannunzio@fao.org                               
####### Update:  2019/02/05                                    
####################################################################################

nerf0010 <- read.csv(paste0(datadir,"database_2000_2010_avc_bandunduu.csv"))
nerf1014 <- read.csv(paste0(datadir,"2010_2014_prov_V3_final.csv"))

head(nerf0010)
head(nerf1014)

##### LIRE LA BDD
df0 <- read.csv(paste0(datadir,"bd_2000_2010_2014_v20190426.csv"))

nerf0010$unique_id <- paste0("id_0010_",row(nerf0010)[,1])
nerf1014$unique_id <- paste0("id_1014_",row(nerf1014)[,1])
df0$unique_id <- paste0("id_bdd_",row(df0)[,1])

##### SPATIALISER LES POINTS
spdf <- SpatialPointsDataFrame(
  coords = df0[,c("location_x","location_y")],
  data   = df0,
  proj4string=CRS("+init=epsg:4326")
)

##### SPATIALISER LES POINTS NERF 2000-2010
spdf_n0010 <- SpatialPointsDataFrame(
  coords = nerf0010[,c("location_x","location_y")],
  data   = nerf0010,
  proj4string=CRS("+init=epsg:4326")
)

##### SPATIALISER LES POINTS NERF 2010-2014
spdf_n1014 <- SpatialPointsDataFrame(
  coords = nerf1014[,c("location_x","location_y")],
  data   = nerf1014,
  proj4string=CRS("+init=epsg:4326")
)

##### CREER UN BUFFER DE 10 CENTIMETRE AUTOUR DES POINTS
buffer0010    <- SpatialPolygonsDataFrame(buffer(spdf_n0010,0.1,dissolve=F),spdf_n0010@data,match.ID = F)
buffer1014    <- SpatialPolygonsDataFrame(buffer(spdf_n1014,0.1,dissolve=F),spdf_n1014@data,match.ID = F)


##### INTERSECTION BUFFER BDD AVEC POINTS NERF
all_0010  <- aggregate(x = spdf["unique_id"],by = buffer0010,FUN = length)
all_1014  <- aggregate(x = spdf["unique_id"],by = buffer1014,FUN = length)

doublon_0010  <- aggregate(x = spdf_n0010["unique_id"],by = buffer0010,FUN = length)
doublon_1014  <- aggregate(x = spdf_n1014["unique_id"],by = buffer1014,FUN = length)

nerf0010$self_count <- doublon_0010@data$unique_id
nerf1014$self_count <- doublon_1014@data$unique_id

nerf0010$all_count <- all_0010@data$unique_id
nerf1014$all_count <- all_1014@data$unique_id

table(nerf0010$all_count,nerf0010$self_count)
table(nerf1014$all_count,nerf1014$self_count)

table(nerf0010$self_count)
table(nerf1014$self_count)

over0010    <- over(spdf,buffer0010)
over1014    <- over(spdf,buffer1014)

df0$bdd0010 <- !is.na(over0010$id)
df0$bdd1014 <- !is.na(over1014$id)

dfd <- df0[df0$count > 1 ,]
df1 <- df0[df0$count == 1 ,]
table(df0$bdd0010,df0$bdd1014,df0$count)
table(df0$bdd0010,df0$bdd1014)
table(dfd$bdd0010,dfd$bdd1014)
table(df1$bdd0010,df1$bdd1014)
table(df0$bdd0010,df0$count)
table(df0$bdd1014,df0$count,df0$bdd0010)

nrow(nerf1014[nerf1014$all_count >1,])
##### INTERSECTION BUFFER BDD AVEC POINTS NERF 2010-2014
test1014      <- over(buf,spdf_n1014)
table(test$prov,useNA = "always")

summary(test)
df0$bdd <- "none"
df0[!is.na(test$prov),]$bdd <- "nerf1014"
table(df0$bdd)

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



