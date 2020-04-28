####################################################################################
####### Object:  EVALUATION PRECISION BDD TRANSITION        
####### Author:  remi.dannunzio@fao.org                               
####### Update:  2019/02/05                                    
####################################################################################

##### LIRE LES DEUX BASES (POINTS ET SUPERFICIES)
bd <- read.csv(paste0(datadir,"bd_2000_2010_2014_v20190426.csv"))
bd[bd$ce_change == 3 & bd$primary_land_cover1 == 1,]$ce_change <- 1
bd[ bd$ce_change == 1,]$ce_transition <- 11
bd[ bd$ce_change == 2,]$ce_transition <- 22
bd$ce_prov_transition <- paste0(bd$map_province,bd$ce_transition)
ar <- read.csv(paste0(datadir,"areas_transitions_2000_2010_2014.csv"))

table(bd$ce_transition,bd$count)

##### SELECTIONNER SEULEMENT LES SINGLETS
df0 <- bd[bd$count == 1,]

##### SELECTIONNER SEULEMENT LES TRANSITIONS SURES
df0 <- df0[df0$ce_transition >0,]

##### SELECTIONNER SEULEMENT LES POINTS QUI TOMBENT SUR LA CARTE
df0 <- df0[df0$map_transition >0,]

df0$ce_prov_transition <- as.numeric(df0$ce_prov_transition)

##### NIVEAU NATIONAL
# map_code <- "map_transition"
# ref_code <- "ce_transition"
# ar_code  <- "transition_clean"

##### NIVEAU PROVINCIAL
map_code <- "prov_transition"
ref_code <- "ce_prov_transition"
ar_code  <- "reclass"

##### RESTRICT THE AREAS TO ONLY EXISTING PROVINCES
ar_area <- "area_ha"
ar      <- ar[ar$transition_clean != 00 & !is.na(ar$province),]

##### CREER LA LEGENDE ET LE FICHIER DES SUPERFICIES FINAL
legend  <- unique(ar[,ar_code])
areas   <- data.frame(cbind(legend,tapply(ar[,ar_area],ar[,ar_code],sum)))

names(areas) <- c("class","area")

s <- saea(df0,0.9,areas,legend,map_code,ref_code)

s$transition <- substr(s$class,nchar(s$class)-1,nchar(s$class))
s$province   <- as.numeric(substr(s$class,0,nchar(s$class)-2))

codes <- read.dbf(paste0(datadir,"RDC_Province_26.dbf"))[,c("NOM","ID_SEPAL")]
names(codes) <- c("province_name","ID_SEPAL")

s1   <- merge(s,codes,by.y="ID_SEPAL",by.x="province",all.x=T)

tapply(s1$strRS_area_estimate,s1[,c("transition")],sum)
tapply(s1$strRS_area_estimate,s1[,c("province_name","transition")],sum)

df     <- df0
matrix <- table(df[,map_code],df[,ref_code])

write.csv2(matrix,paste0(datadir,"matrix_provinces_20190428.csv"))
write.csv2(s1,paste0(datadir,"resultats_provinces_20190428.csv"),row.names = F)

# write.csv(matrix,paste0(datadir,"matrix_national_20190428.csv"))
# write.csv(s,paste0(datadir,"resultats_national_20190428.csv"),row.names = F)
