####################################################################################
####### Object:  ESTIMATION OF EMISSIONS FOR RENF 2010-2014        
####### Author:  remi.dannunzio@fao.org                               
####### Update:  2020/04/23                               
####################################################################################

##### LIRE LE FICHIER DES BIOMASSES DES CLASSES DE FORÊT et NON-FORÊT ISSUES DU PRÉ-INVENTAIRE
bt_foret <- data.frame(readxl::read_xlsx("ef_20200423.xlsx","bt_forest"))
bt_nofor <- data.frame(readxl::read_xlsx("ef_20200423.xlsx","bt_nonforest"))

##### LIRE LA BDD CONTENANT TOUS LES POINTS INTERPRETÉS
df0 <- read.csv2(paste0(datadir,"bd_simulation_28_2010_2014_20200403.csv"))

##### LIRE LE FICHIER DES SUPERFICIES DE LA CARTE
ar <- read.csv(paste0(datadir,"areas_transitions_2000_2010_2014_20190724.csv"))
ar$cls_cl <- paste0(ar$province,ar$class1014)


##### LES CALCULS SONT FAITS PAR PROVINCE: ON PREND LES CODES AU NIVEAU PROVINCIAL
map_code <- "map_cl"
ref_code <- "ref_cl"
ar_code  <- "cls_cl"
ar_area  <- "area_ha"

##### ASSURER QUE LES CODES SOIENT NUMERIQUES
df0[,map_code] <- as.numeric(df0[,map_code])
df0[,ref_code] <- as.numeric(df0[,ref_code])
ar[,ar_code]   <- as.numeric(ar[,ar_code])


##### RESTREINDRE LE FICHIER DES SUPERFICIES AUX PROVINCES EXISTANTES
ar      <- ar[ar$transition_clean != 00 & !is.na(ar$province),]


#### CREER UN CHAMPS DES CLASSES D'OCCUPATION DES SOLS DE DEPART QUATERNAIRE
df0$quarternary_land_cover_label_filled <- df0$quarternary_land_cover_label
nrow(df0[df0$quarternary_land_cover_label_filled == "" & df0$ce_change==3,])

#### SI LE CHAMPS EST VIDE, REMPLIR AVEC TERTIAIRE puis SECONDAIRE puis PRIMAIRE SI MANQUANT
df0[df0$quarternary_land_cover_label_filled == "" | is.na(df0$quarternary_land_cover_label_filled),"quarternary_land_cover_label_filled"] <- df0[df0$quarternary_land_cover_label_filled == "" | is.na(df0$quarternary_land_cover_label_filled),"tertiary_land_cover_label"]
df0[df0$quarternary_land_cover_label_filled == "" | is.na(df0$quarternary_land_cover_label_filled),"quarternary_land_cover_label_filled"] <- df0[df0$quarternary_land_cover_label_filled == "" | is.na(df0$quarternary_land_cover_label_filled),"secondary_land_cover_label"]
df0[df0$quarternary_land_cover_label_filled == "" | is.na(df0$quarternary_land_cover_label_filled),"quarternary_land_cover_label_filled"] <- df0[df0$quarternary_land_cover_label_filled == "" | is.na(df0$quarternary_land_cover_label_filled),"primary_land_cover_label"]

#### VERIFIER QU'IL N'Y A PLUS DE CHAMPS VIDES
nrow(df0[df0$quarternary_land_cover_label_filled == "" & df0$ce_change==3,]) == 0

####  CREER UN CHAMPS DES CLASSES D'OCCUPATION DES SOLS  D'ARRIVEE TERTIAIRE 
df0$tertiary_land_cover1_label_filled <- df0$tertiary_land_cover1_label

#### SI LE CHAMPS EST VIDE, REMPLIR AVEC SECONDAIRE puis PRIMAIRE SI MANQUANT
df0[df0$tertiary_land_cover1_label_filled == "" | is.na(df0$tertiary_land_cover1_label_filled),"tertiary_land_cover1_label_filled"] <- df0[df0$tertiary_land_cover1_label_filled == "" | is.na(df0$tertiary_land_cover1_label_filled),"secondary_land_cover1_label"]
df0[df0$tertiary_land_cover1_label_filled == "" | is.na(df0$tertiary_land_cover1_label_filled),"tertiary_land_cover1_label_filled"] <- df0[df0$tertiary_land_cover1_label_filled == "" | is.na(df0$tertiary_land_cover1_label_filled),"primary_land_cover1_label"]

nrow(df0[df0$tertiary_land_cover1_label_filled == "" & df0$ce_change==3,])
table(df0$tertiary_land_cover1_label_filled,df0$province_name)

write.csv(df0,paste0(datadir,"bd_filled_simulation_28_2010_2014_20200423.csv"))

dtot <- df0[df0[,"ce_change"] == 3,] # ON NE PREND QUE LA DEFORESTATION
dtot[dtot$province_name == "Kinshasa",]
transitions <- sort(unique(paste0(dtot$quarternary_land_cover_label_filled,"--",dtot$tertiary_land_cover1_label_filled)))

dd          <- data.frame(matrix(nrow = 0,ncol=1+length(transitions)*5))

names(dd)   <- c("province",
                 paste0("prop_",transitions),
                 sapply(transitions,function(x){paste0(c("area_","ci_area_"),x)}),
                 sapply(transitions,function(x){paste0(c("emis_","ci_emis_"),x)})
)

nrow(dd)

(subc     <- table(dtot$quarternary_land_cover_label_filled,dtot$tertiary_land_cover1_label_filled)) # ON PREND LES TRANSITIONS)
sum(subc)


##### FAIRE TOURNER POUR CHAQUE PROVINCE
province <- "Kinshasa"
dd <- dd[0,]
for(province in unique(ar$NOM)){
  
  legend <- sort(unique(ar[ar$NOM == province,ar_code]))
  
  areas <- data.frame(cbind(legend,
                            tapply(ar[ar$NOM == province,ar_area],
                                   ar[ar$NOM == province,ar_code],
                                   sum)))
  
  names(areas) <- c("class","area")
  
  df <- df0[df0$province_name == province,]
  
  s <- saea(df,0.9,areas,legend,map_code,ref_code)
  
  df1 <- df[df[,ref_code] == legend[3],] # ON NE PREND QUE LA DEFORESTATION
  
  subc     <- table(df1$quarternary_land_cover_label_filled,df1$tertiary_land_cover1_label_filled) # ON PREND LES TRANSITIONS
  
  prop1014 <- as.data.frame.matrix(subc/sum(subc))
  loss1014 <- as.data.frame.matrix(subc/sum(subc)*s[3,"strRS_area_estimate"])       # Comme on a déjà restreint la BD, il n'y a que 3 lignes
  ci1014   <- as.data.frame.matrix(subc/sum(subc)*s[3,"strRS_confidence_interval"]) # Comme on a déjà restreint la BD, il n'y a que 3 lignes
  
  prov1014       <- cbind(row.names(loss1014),prop1014,loss1014,ci1014)
  names(prov1014) <- c("cl_dep",
                       paste0("prop_",names(loss1014)),
                       paste0("area_",names(loss1014)),
                       paste0("ci_area_",names(ci1014)))
  
  d1 <- merge(prov1014,bt_foret[,c("Class","BT")],by.x="cl_dep",by.y="Class",all.x=T)
  
  calc_emission <- function(x){
    d1[,paste0("area_",x)]*(d1[,"BT"]-bt_nofor[bt_nofor$Class == x,"BT"])*0.47*44/12}
  
  calc_ci_em <- function(x){
    d1[,paste0("ci_area_",x)]*(d1[,"BT"]-bt_nofor[bt_nofor$Class == x,"BT"])*0.47*44/12}
  
  d1[,paste0("emis_",   names(loss1014))]  <- lapply(names(loss1014),calc_emission)
  d1[,paste0("ci_emis_",names(loss1014))]  <- lapply(names(loss1014),calc_ci_em)
  
  names(d1)
  dd[nrow(dd)+1,"province"] <- province
  
  for(cl_dep in d1$cl_dep){
    for(cl_arr in names(loss1014)){
      if(d1[d1$cl_dep == cl_dep,paste0("prop_",   cl_arr)] != 0){
        dd[dd$province == province,paste0("prop_",   cl_dep,"--",cl_arr)] <- d1[d1$cl_dep == cl_dep,paste0("prop_",   cl_arr)]
        dd[dd$province == province,paste0("area_",   cl_dep,"--",cl_arr)] <- d1[d1$cl_dep == cl_dep,paste0("area_",   cl_arr)]
        dd[dd$province == province,paste0("ci_area_",cl_dep,"--",cl_arr)] <- d1[d1$cl_dep == cl_dep,paste0("ci_area_",cl_arr)]
        dd[dd$province == province,paste0("emis_",   cl_dep,"--",cl_arr)] <- d1[d1$cl_dep == cl_dep,paste0("emis_",   cl_arr)]
        dd[dd$province == province,paste0("ci_emis_",cl_dep,"--",cl_arr)] <- d1[d1$cl_dep == cl_dep,paste0("ci_emis_",cl_arr)]
      }
    }}
  
  write.csv(d1,paste0(em_dir,"em_",province,"_1014.csv"),row.names = F)
  
}


dd$total_emission  <- rowSums(dd[,names(dd)[grepl(pattern = glob2rx("emis_*"),names(dd))]],na.rm = T )
sum(dd$total_emission)

dd$total_area_loss <- rowSums(dd[,names(dd)[grepl(pattern = glob2rx("area_*"),names(dd))]],na.rm = T )
sum(dd$total_area_loss)

write.csv(dd,paste0(datadir,"emissions_renf_2010_2014_20200423.csv"),row.names = F)
