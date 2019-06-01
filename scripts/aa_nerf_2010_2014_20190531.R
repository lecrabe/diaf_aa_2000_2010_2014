####################################################################################
####### Object:  EVALUATION PRECISION BDD TRANSITION        
####### Author:  remi.dannunzio@fao.org                               
####### Update:  2019/02/05                                    
####################################################################################

##### LIRE LES DEUX BASES (POINTS ET SUPERFICIES)
bd <- read.csv(paste0(datadir,"nerf_2000_2010_v20190531.csv"))
bd <- read.csv(paste0(datadir,"nerf_2010_2014_v20190531.csv"))

names(bd)

bd$map_cl <- bd$map_prov/100
bd$ref_cl <- bd$ce_prov/100

bd$map_change <- substr(bd$map_cl,nchar(bd$map_cl),nchar(bd$map_cl))
bd$ce_change  <- substr(bd$ref_cl,nchar(bd$ref_cl),nchar(bd$ref_cl))

table(bd$map_change,bd$ce_change)

ar <- read.csv(paste0(datadir,"areas_transitions_2000_2010_2014.csv"))
ar$cls_cl <- paste0(ar$province,ar$clean1014)

##### SELECTIONNER SEULEMENT LES SINGLETS
df0 <- bd[bd$self_count ==1,]

##### NIVEAU NATIONAL
# map_code <- "map_transition"
# ref_code <- "ce_transition"
# ar_code  <- "transition_clean"

##### NIVEAU PROVINCIAL
map_code <- "map_cl"
ref_code <- "ref_cl"
ar_code  <- "cls_cl"

df0[,map_code] <- as.numeric(df0[,map_code])
df0[,ref_code] <- as.numeric(df0[,ref_code])
ar[,ar_code] <- as.numeric(ar[,ar_code])

##### RESTRICT THE AREAS TO ONLY EXISTING PROVINCES
ar_area <- "area_ha"
ar      <- ar[ar$transition_clean != 00 & !is.na(ar$province),]

##### CREER LA LEGENDE ET LE FICHIER DES SUPERFICIES FINAL
legend  <- unique(ar[,ar_code])
areas   <- data.frame(cbind(legend,tapply(ar[,ar_area],ar[,ar_code],sum)))

names(areas) <- c("class","area")
areas[,"area"] <- as.numeric(areas[,"area"])
sum(areas[,"area"])

df0[,map_code] <- as.numeric(df0[,map_code])
df0[,ref_code] <- as.numeric(df0[,ref_code])
areas[,"class"] <- as.numeric(areas[,"class"])

s <- saea(df0,0.9,areas,legend,map_code,ref_code)

s$class_code <- as.numeric(substr(s$class,nchar(s$class),nchar(s$class)))
s$province   <- as.numeric(substr(s$class,0,nchar(s$class)-1))

codes <- read.dbf(paste0(datadir,"RDC_Province_26.dbf"))[,c("NOM","ID_SEPAL")]
names(codes) <- c("province_name","ID_SEPAL")

s1   <- merge(s,codes,by.y="ID_SEPAL",by.x="province",all.x=T)

n2 <- tapply(s1$strRS_area_estimate,s1[,c("class_code")],sum)
c2 <- tapply(s1$map_pixel_count,s1[,c("class_code")],sum)

# tapply(s1$strRS_area_estimate,s1[,c("province_name","class_code")],sum)
# tapply(s1$map_pixel_count,s1[,c("province_name","class_code")],sum)
# 
# (matrix <- table(df0[,"map_change"],df0[,ref_code]))
# (matrix <- table(df0[,map_code],df0[,ref_code]))
# 
# write.csv(matrix,paste0(datadir,"matrix_provinces_",nrow(df0),"_20190428.csv"))
# write.csv(s1,paste0(datadir,"resultats_provinces_",nrow(df0),"_20190428.csv"),row.names = F)

# write.csv(matrix,paste0(datadir,"matrix_national_20190426.csv"))
# write.csv(s,paste0(datadir,"resultats_national_20190426.csv"),row.names = F)
