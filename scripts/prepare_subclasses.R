####################################################################################
####### Object:  SOUS-CLASSES POUR LES TRANSITIONS       
####### Author:  remi.dannunzio@fao.org                               
####### Update:  2019/02/05                                    
####################################################################################
setwd(datadir)


#### LISTE DES FICHIERS LIRE LA PREMIERE LIGNE
occdir <- occ0010dir

list <-  list.files(paste0(occdir),pattern = ".csv")
for(file in list){
  print(readLines(paste0(occdir,file))[1])
  print(" ")
}

####  PRECISER LES SEPARATEURS POUR CHAQUE
s <- list()
list <- cbind(list,c(",",",",";",";"))

#### RECUPERER LES NOMS DES COLONNES DANS CHAQUE FICHIER
for(i in 1:nrow(list)){
  tmp <- read.csv(paste0(occdir,list[i,1]),sep=list[i,2])
  s   <- append(s,list(names(tmp)))
}

#### PRENDRE LES ELEMENTS COMMUNS A CHAQUE FICHIER
sapply(s,length)
classes <- Reduce(intersect, s)

#### CREER UN DATA.FRAME VIDE
d <- data.frame(matrix(nrow=0,ncol=length(classes)+1))
names(d) <- c(classes,"unique_id")

#### AJOUTER LES LIGNES DES FICHIERS POUR LES ELEMENTS COMMUNS UNIQUEMENT
for(i in 1:nrow(list)){
  tmp <- read.csv(paste0(occdir,list[i,1]),sep=list[i,2])
  tmp$unique_id <- paste0("p0010_",i,"id_",row(tmp)[,1])
  d <- rbind(d,tmp[,c(classes,"unique_id")])
}


d$location_x <- as.numeric(gsub(",",".",d$location_x))
d$location_y <- as.numeric(gsub(",",".",d$location_y))

d <- d[!is.na(d$location_x) & d$location_x >0 & d$location_x < 50,]

#plot(d$location_x,d$location_y)

d0010 <- d



#### LISTE DES FICHIERS LIRE LA PREMIERE LIGNE
occdir <- occ1014dir

list <-  list.files(paste0(occdir),pattern = ".csv")
for(file in list){
  print(readLines(paste0(occdir,file))[1])
  print(" ")
}

####  PRECISER LES SEPARATEURS POUR CHAQUE
s <- list()
list <- cbind(list,c(",",rep(";",6)))

#### RECUPERER LES NOMS DES COLONNES DANS CHAQUE FICHIER
for(i in 1:nrow(list)){
  tmp <- read.csv(paste0(occdir,list[i,1]),sep=list[i,2]
  )
  s   <- append(s,list(names(tmp)))
}
s
#### PRENDRE LES ELEMENTS COMMUNS A CHAQUE FICHIER
sapply(s,length)
classes <- Reduce(intersect, s)

#### CREER UN DATA.FRAME VIDE
d <- data.frame(matrix(nrow=0,ncol=length(classes)+1))
names(d) <- c(classes,"unique_id")

#### AJOUTER LES LIGNES DES FICHIERS POUR LES ELEMENTS COMMUNS UNIQUEMENT
for(i in 1:nrow(list)){
  tmp <- read.csv(paste0(occdir,list[i,1]),sep=list[i,2])
  tmp$unique_id <- paste0("p1014_",i,"id_",row(tmp)[,1])
  d <- rbind(d,tmp[,c(classes,"unique_id")])
}

d$location_x <- as.numeric(gsub(",",".",d$location_x))
d$location_y <- as.numeric(gsub(",",".",d$location_y))

d <- d[!is.na(d$location_x) & d$location_x >0 & d$location_x < 50,]

#plot(d$location_x,d$location_y)

d1014 <- d


##################### VERIFIER ET SUPPRIMER LES DOUBLONS ENTRE PERIODES
nrow(d0010) == length(unique(paste0(d0010$location_x,d0010$location_y)))
nrow(d1014) == length(unique(paste0(d1014$location_x,d1014$location_y)))

d0010$period <- "p0010"
d1014$period <- "p1014"
d0010$loc <- paste0(d0010$location_x,d0010$location_y)
d0010$dbl_intra <- duplicated(d0010$loc)

d1014$loc <- paste0(d1014$location_x,d1014$location_y)
d1014$dbl_intra <- duplicated(d1014$loc)

table(d0010$dbl_intra,d0010$period)
table(d1014$dbl_intra)

d1014 <- d1014[,names(d1014)%in% names(d0010)]

all(names(d0010) == names(d1014))
d <- rbind(d0010,d1014)

d$dbl_inter <- duplicated(paste0(d$loc,d$period))

table(d$dbl_inter,d$dbl_intra)

bd <- read.csv(paste0(datadir,"bd_2000_2010_2014_v20190210.csv"))
d1 <- d[!(d$loc %in% paste0(bd$location_x,bd$location_y)),]

bd$loc <- paste0(bd$location_x,bd$location_y)
table(d$loc %in% paste0(bd$location_x,bd$location_y))
length(unique(d$unique_id))
table(d$period,d$dbl_intra)

head(d)
head(bd)

d$nchar_loc <- nchar(d$loc)
hist(d$nchar_loc)

bd$nchar_loc <- nchar(bd$loc)
hist(bd$nchar_loc,add=T,col="blue")

bd$id <- row(bd)[,1]
#length(unique(df$cause_id))

spdf_d <- SpatialPointsDataFrame(
  coords = d[,c("location_x","location_y")],
  data   = d,
  proj4string=CRS("+init=epsg:4326")
)

spdf_bd <- SpatialPointsDataFrame(
  coords = bd[,c("location_x","location_y")],
  data   = bd,
  proj4string=CRS("+init=epsg:4326")
)

buffer <- buffer(spdf_d,1,dissolve=F)

buf <- SpatialPolygonsDataFrame(buffer,
                                spdf_d@data,
                                match.ID = F)

over <- over(spdf_bd,buf)
names(over)<-c("id","location_srs","location_x","location_y","operator","elevation","slope","aspect",
               "actively_saved","actively_saved_on_year","actively_saved_on_month","actively_saved_on_day","plot_file",
               "primary_land_cover","primary_land_cover_label",
               "secondary_land_cover","secondary_land_cover_label",
               "tertiary_land_cover","tertiary_land_cover_label",
               "quarternary_land_cover","quarternary_land_cover_label",
               "year","year_label","year1","year1_label",
               "primary_land_cover1","primary_land_cover1_label",
               "secondary_land_cover1","secondary_land_cover1_label",
               "tertiary_land_cover1","tertiary_land_cover1_label",
               "quarternary_land_cover1","quarternary_land_cover1_label",
               "cc","cc1","unique_id","period","loc","dbl_intra","dbl_inter","nchar_loc")
table(over$cc,over$cc1)
causes <- over[,c("unique_id","loc",
                  "primary_land_cover","primary_land_cover_label",
                  "secondary_land_cover","secondary_land_cover_label",
                  "tertiary_land_cover","tertiary_land_cover_label",
                  "quarternary_land_cover","quarternary_land_cover_label",
                  "year","year_label","year1","year1_label",
                  "primary_land_cover1","primary_land_cover1_label",
                  "secondary_land_cover1","secondary_land_cover1_label",
                  "tertiary_land_cover1","tertiary_land_cover1_label",
                  "quarternary_land_cover1","quarternary_land_cover1_label")]

names(causes) <- c("cause_id","cause_loc",
                   "primary_land_cover","primary_land_cover_label",
                   "secondary_land_cover","secondary_land_cover_label",
                   "tertiary_land_cover","tertiary_land_cover_label",
                   "quarternary_land_cover","quarternary_land_cover_label",
                   "year","year_label","year1","year1_label",
                   "primary_land_cover1","primary_land_cover1_label",
                   "secondary_land_cover1","secondary_land_cover1_label",
                   "tertiary_land_cover1","tertiary_land_cover1_label",
                   "quarternary_land_cover1","quarternary_land_cover1_label")

df <- cbind(bd,causes)
head(df)


write.csv(df,paste0(datadir,"bd_2000_2010_2014_v20190215.csv"),row.names = F)

