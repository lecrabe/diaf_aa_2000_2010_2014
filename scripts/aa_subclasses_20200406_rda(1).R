####################################################################################
####### Object:  EVALUATION PRECISION BDD TRANSITION        
####### Author:  remi.dannunzio@fao.org                               
####### Update:  2020/04/06                               
####################################################################################

##### LIRE LES DEUX BASES (POINTS ET SUPERFICIES)
bd <- read.csv2(paste0(datadir,"bd_simulation_14_2000_2010_20200403.csv"))
df0 <- bd


##### LIRE LE FICHIER DES SUPERFICIES
ar <- read.csv(paste0(datadir,"areas_transitions_2000_2010_2014_20190724.csv"))
ar$cls_cl <- paste0(ar$province,ar$class0010)


##### NIVEAU PROVINCIAL
map_code <- "map_cl"
ref_code <- "ref_cl"
ar_code  <- "cls_cl"


##### ASSURER QUE LES CODES SONT NUMERIQUES
df0[,map_code] <- as.numeric(df0[,map_code])
df0[,ref_code] <- as.numeric(df0[,ref_code])
ar[,ar_code]   <- as.numeric(ar[,ar_code])


##### RESTRICT THE AREAS TO ONLY EXISTING PROVINCES
ar      <- ar[ar$transition_clean != 00 & !is.na(ar$province),]


#### CREER UN CHAMPS LAND COVER DE DEPART QUATERNAIRE  REMPLI AVEC TERTIAIRE ou SECONDAIRE ou PRIMAIRE SI MANQUANT
df0$quarternary_land_cover_label_filled <- df0$quarternary_land_cover_label
nrow(df0[df0$quarternary_land_cover_label_filled == "" & df0$ce_change==3,])

df0[df0$quarternary_land_cover_label_filled == "" | is.na(df0$quarternary_land_cover_label_filled),"quarternary_land_cover_label_filled"] <- df0[df0$quarternary_land_cover_label_filled == "" | is.na(df0$quarternary_land_cover_label_filled),"tertiary_land_cover_label"]
df0[df0$quarternary_land_cover_label_filled == "" | is.na(df0$quarternary_land_cover_label_filled),"quarternary_land_cover_label_filled"] <- df0[df0$quarternary_land_cover_label_filled == "" | is.na(df0$quarternary_land_cover_label_filled),"secondary_land_cover_label"]
df0[df0$quarternary_land_cover_label_filled == "" | is.na(df0$quarternary_land_cover_label_filled),"quarternary_land_cover_label_filled"] <- df0[df0$quarternary_land_cover_label_filled == "" | is.na(df0$quarternary_land_cover_label_filled),"primary_land_cover_label"]

nrow(df0[df0$quarternary_land_cover_label_filled == "" & df0$ce_change==3,])


#### CREER UN CHAMPS LAND COVER D'ARRIVEE TERTIAIRE REMPLI AVEC SECONDAIRE ou PRIMAIRE SI MANQUANT
df0$tertiary_land_cover1_label_filled <- df0$tertiary_land_cover1_label
df0[df0$tertiary_land_cover1_label_filled == "" | is.na(df0$tertiary_land_cover1_label_filled),"tertiary_land_cover1_label_filled"] <- df0[df0$tertiary_land_cover1_label_filled == "" | is.na(df0$tertiary_land_cover1_label_filled),"secondary_land_cover1_label"]
df0[df0$tertiary_land_cover1_label_filled == "" | is.na(df0$tertiary_land_cover1_label_filled),"tertiary_land_cover1_label_filled"] <- df0[df0$tertiary_land_cover1_label_filled == "" | is.na(df0$tertiary_land_cover1_label_filled),"primary_land_cover1_label"]

nrow(df0[df0$tertiary_land_cover1_label_filled == "" & df0$ce_change==3,])
table(df0$tertiary_land_cover1_label_filled,df0$province_name)

##### FAIRE TOURNER POUR CHAQUE PROVINCE
province <- "Equateur"

for(province in unique(ar$NOM)){
  
  legend <- unique(ar[ar$NOM == province,ar_code])
  
  areas <- data.frame(cbind(legend,
                            tapply(ar[ar$NOM == province,ar_area],
                                   ar[ar$NOM == province,ar_code],
                                   sum)))
  
  names(areas) <- c("class","area")
  
  df <- df0[df0$province_name == province,]
  
  s <- saea(df,0.9,areas,legend,map_code,ref_code)
  
  df1 <- df[df[,ref_code] == legend[3],] # ON NE PREND QUE LA DEFORESTATION
  
  subc     <- table(df1$quarternary_land_cover_label_filled,df1$tertiary_land_cover1_label_filled) # ON PREND LES TRANSITIONS
  
  prop0010 <- as.data.frame.matrix(subc/sum(subc))
  loss0010 <- as.data.frame.matrix(subc/sum(subc)*s[3,"strRS_area_estimate"])        # Comme on a déjà restreint la BD, il n'y a que 3 lignes
  ci0010   <- as.data.frame.matrix(subc/sum(subc)*s[3,"strRS_confidence_interval"])  # Comme on a déjà restreint la BD, il n'y a que 3 lignes
  
  # prop1014 <- as.data.frame.matrix(subc/sum(subc))
  # loss1014 <- as.data.frame.matrix(subc/sum(subc)*s[3,"strRS_area_estimate"])       # Comme on a déjà restreint la BD, il n'y a que 3 lignes
  # ci1014   <- as.data.frame.matrix(subc/sum(subc)*s[3,"strRS_confidence_interval"]) # Comme on a déjà restreint la BD, il n'y a que 3 lignes
  
  prov0010        <- cbind(row.names(loss0010),prop0010,loss0010,ci0010)
  names(prov0010) <- c("cl_dep",
                       paste0("prop_",names(loss0010)),
                       paste0("area_",names(loss0010)),
                       paste0("ci_",names(ci0010)))
  
  # prov1014       <- cbind(row.names(loss1014),prop1014,loss1014,ci1014)
  # names(prov1014) <- c("cl_dep",
  #                      paste0("prop_",names(loss1014)),
  #                      paste0("area_",names(loss1014)),
  #                      paste0("ci_",names(ci1014)))
  
  write.csv2(prov0010,paste0(drvdir,province,"_0010.csv"),row.names = F)
  # write.csv2(prov1014,paste0(drvdir,province,"_1014.csv"),row.names = F)
  
  #write.csv(prov0010,paste0(drvdir,province,"_0010_r.csv"),row.names = F) # pour Rémi en CSV normal
  
}


