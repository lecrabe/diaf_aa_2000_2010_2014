
############
############ Produire le tableau des proportions (DRC FREL)
############

# Preciser le chemin du dossier vers le tableaux de données
root = "D:/FAO_2018/Congo_RDC/activities/FONAREDD/test_nerf_pr_jp/"

# Preciser le nom du fichier CSV de données (on le charge dans l'objet "CONV")                                             
CONV = read.table(paste(root,"bd_transition.csv", sep=""), dec=".", sep=";", header=T)

####
####  FROM     ( = la classe d'occupation du sol initiale, le "_2000" n'a pas d'importance)
####
                                                             
CONV$FROM = NA
CONV$FROM[which(CONV$quarternary_land_cover_2000 == "Forest Dense on Hydromorphic Soil")] = "FDHSH"
CONV$FROM[which(CONV$quarternary_land_cover_2000 == "Forest Dense on Terra Firma")] = "FDHTF"
CONV$FROM[which(CONV$quarternary_land_cover_2000 == "Gallery Forest")] = "FDHTF"
CONV$FROM[which(CONV$tertiary_land_cover_2000 == "Forest, Dry or Sparse")] = "FSFC"
CONV$FROM[which(CONV$tertiary_land_cover_2000 == "Secondary Forest")] = "FSc"

####
####  TO      ( = la classe d'occupation du sol finale)
####

CONV$TO = NA
CONV$TO[which(CONV$tertiary_land_cover_2010 == "RCA")] = "RCA"
CONV$TO[which(CONV$tertiary_land_cover_2010 == "Perennial Cultivation" & CONV$quarternary_land_cover_2010 == "Oil Palm")] = "RCA"
CONV$TO[which(CONV$secondary_land_cover_2010 == "Savanna")] = "Savanna"
CONV$TO[which(CONV$secondary_land_cover_2010 == "Other")] = "Other"

####
####  CLEAN the DATA
####

CONV_fin = CONV[which(!is.na(CONV$FROM)),]
CONV_fin = CONV_fin[which(!is.na(CONV_fin$TO)),]

# enregistrement (interméddiaire) du fichier "prop" sur lequel on calcul les proportion 
write.table(CONV_fin, paste(root,"CONV_fin_R.csv", sep=""), dec=".", sep=";", row.names=F)

####
####  Build summary statistics for JP
####

# On génère un SHP pour tiré les provinces
library(raster)
library(rgdal)

# on lie le fichier des provinces
PROVINCES <- shapefile("F:/BACK_UP/GIS/SHP_DRC/Administrative_boundaries/RDC_Province_26.shp")

CONV_fin$ID_temp = seq(1,nrow(CONV_fin),1)

temp = CONV_fin[,c("X", "Y", "ID_temp")] 
coordinates(temp)=~X+Y 
proj4string(temp)=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
proj_new <- "+proj=merc +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
temp2 = spTransform(temp,CRS(proj_new))

RES = as.data.frame(matrix(NA,1,8))

list_FROM = c("FDHSH", "FDHTF", "FSFC", "FSc")
list_TO = c("RCA", "Savanna", "Other")

a=1
for (i in 1:length(Liste_provinces_total)) {

provincei = Liste_provinces_total[i]
PROVINCES.sub <- PROVINCES[as.character(PROVINCES@data$NOM) %in% provincei, ]
pointprov = temp2[PROVINCES.sub,]
ID_provi = attributes(pointprov)$data$ID_temp

CONV_temp = CONV_fin[ID_provi,]
samp_size = nrow(CONV_temp)

          for (from in 1:length(list_FROM)) {
          FROM = list_FROM[from]
          
               for (to in 1:length(list_TO))  {

TO = list_TO[to]
sampleij = length(which(CONV_temp$FROM == FROM & CONV_temp$TO == TO))
p = sampleij/samp_size
RES[a,1] = provincei
RES[a,2] = FROM     
RES[a,3] = TO  
RES[a,4] = sampleij
RES[a,5] = p
RES[a,6] = SE = sqrt(p * ( 1 - p ) / samp_size )
t_value = NA
RES[a,7] = t_value = qt(0.05, (samp_size-1), lower.tail = FALSE)
RES[a,8] = CI = SE * t_value 
a = a+1  
#
                                           }
                                             }
}
names(RES)[1] = "Province"
names(RES)[2] = "FROM"
names(RES)[3] = "TO"
names(RES)[4] = "n"
names(RES)[5] = "prop"
names(RES)[6] = "SE"
names(RES)[7] = "t-value"
names(RES)[8] = "CI (90%)"

write.table(RES, paste(root,"RES_CONV_fin_R.csv", sep=""), dec=".", sep=";", row.names=F)






















