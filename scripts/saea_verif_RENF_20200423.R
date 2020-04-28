####################################################################################
####### Object:  EVALUATION PRECISION BDD TRANSITION        
####### Author:  remi.dannunzio@fao.org                               
####### Update:  2019/02/05                                    
####################################################################################

##### LIRE LES SUPERFICIES
ar        <- read.csv(paste0(datadir,"areas_transitions_2000_2010_2014_20190724.csv"))

##### SPECIFIER LA BONNE PERIODE
ar$cls_cl <- paste0(ar$province,ar$class1014)

##### LIRE LA BDD
df <- read.csv2(paste0(datadir,"bd_simulation_28_2010_2014_20200403.csv"))

##### NIVEAU PROVINCIAL
map_code <- "map_cl"
ref_code <- "ref_cl"
ar_code  <- "cls_cl"

df0[,map_code] <- as.numeric(df0[,map_code])
df0[,ref_code] <- as.numeric(df0[,ref_code])
ar[,ar_code]   <- as.numeric(ar[,ar_code])

##### RESTRICT THE AREAS TO ONLY EXISTING PROVINCES
ar_area <- "area_ha"
ar      <- ar[ar$transition_clean != 00 & !is.na(ar$province),]

##### CREER LA LEGENDE ET LE FICHIER DES SUPERFICIES FINAL
legend  <- sort(unique(ar[,ar_code]))
areas   <- data.frame(cbind(legend,tapply(ar[,ar_area],ar[,ar_code],sum)))

names(areas) <- c("class","area")

simulation <- 28

print(simulation)

s <- saea(df,0.9,areas,legend,map_code,ref_code)

s$class_code <- as.numeric(substr(s$class,nchar(s$class),nchar(s$class)))
s$province   <- as.numeric(substr(s$class,0,nchar(s$class)-1))

codes <- read.dbf(paste0(datadir,"RDC_Province_26.dbf"))[,c("NOM","ID_SEPAL")]
names(codes) <- c("province_name","ID_SEPAL")

s1  <- merge(s,codes,by.y="ID_SEPAL",by.x="province",all.x=T)

write.csv(s1,paste0(simdir,"verif_res_sim",simulation,"_2010_2014_20200423.csv"),row.names = F)  

