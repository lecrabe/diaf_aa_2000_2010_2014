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
df0 <- df0[df0$plot_file != 'test_plots.ced',]
df0 <- df0[!is.na(df0$map_class1014),]

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
buffer0010    <- SpatialPolygonsDataFrame(buffer(spdf_n0010,1,dissolve=F),spdf_n0010@data,match.ID = F)
buffer1014    <- SpatialPolygonsDataFrame(buffer(spdf_n1014,1,dissolve=F),spdf_n1014@data,match.ID = F)
bufferrenf    <- SpatialPolygonsDataFrame(buffer(spdf,1,dissolve=F),spdf@data,match.ID = F)

##### INTERSECTION BUFFER BDD AVEC POINTS NERF
all_0010  <- aggregate(x = spdf["unique_id"],by = buffer0010,FUN = length)
all_1014  <- aggregate(x = spdf["unique_id"],by = buffer1014,FUN = length)

doublon_renf  <- aggregate(x = spdf["unique_id"],by = bufferrenf,FUN = length)
doublon_0010  <- aggregate(x = spdf_n0010["unique_id"],by = buffer0010,FUN = length)
doublon_1014  <- aggregate(x = spdf_n1014["unique_id"],by = buffer1014,FUN = length)

df0$recount         <- doublon_renf@data$unique_id

nerf0010$self_count <- doublon_0010@data$unique_id
nerf1014$self_count <- doublon_1014@data$unique_id

nerf0010$all_count <- all_0010@data$unique_id
nerf1014$all_count <- all_1014@data$unique_id

table(nerf0010$all_count,nerf0010$self_count)
table(nerf1014$all_count,nerf1014$self_count)

table(df0$count,df0$recount)
summary(df0)

df0[is.na(df0$map_class1014),]
table(nerf0010$self_count)
table(nerf1014$self_count)

over0010    <- over(spdf,buffer0010)
over1014    <- over(spdf,buffer1014)

df0$bdd0010 <- !is.na(over0010$id)
df0$bdd1014 <- !is.na(over1014$id)

dfd <- df0[df0$count > 1 ,]
dfs <- df0[df0$count == 1 ,]

table(df0$bdd0010,df0$bdd1014,df0$count)

table(df0$bdd0010,df0$bdd1014)
table(dfs$periode)

table(dfd$bdd0010,dfd$bdd1014)
table(dfs$bdd0010,dfs$bdd1014,dfs$periode)
table(df0$bdd0010,df0$count)
table(df0$count,df0$periode)

nrow(nerf1014[nerf1014$all_count >1,])

table(dfd$period,dfd$map_transition)
table(dfs$period,dfs$map_transition)

write.csv(df0,paste0(datadir,"bd_2000_2010_2014_v20190531.csv"),row.names = F)
write.csv(nerf0010,paste0(datadir,"nerf_2000_2010_v20190531.csv"),row.names = F)
write.csv(nerf1014,paste0(datadir,"nerf_2010_2014_v20190531.csv"),row.names = F)
