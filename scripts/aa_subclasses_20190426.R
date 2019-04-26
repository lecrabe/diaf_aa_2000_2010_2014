####################################################################################
####### Object:  EVALUATION PRECISION BDD TRANSITION        
####### Author:  remi.dannunzio@fao.org                               
####### Update:  2019/04/26                                 
####################################################################################

##### LIRE LES DEUX BASES (POINTS ET SUPERFICIES)
bd <- read.csv(paste0(datadir,"bd_2000_2010_2014_v20190426.csv"))

##### PRENDRE TOUS LES POINTS DES TRANSITIONS STABLES
bd[bd$ce_change == 1,]$ce_transition <- 11
bd[bd$ce_change == 2,]$ce_transition <- 22
bd$ce_prov_transition <- paste0(bd$map_province,bd$ce_transition)

ar <- read.csv(paste0(datadir,"areas_transitions_2000_2010_2014.csv"))

table(bd$ce_transition,bd$count)

##### SELECTIONNER SEULEMENT LES SINGLETS
df0 <- bd[bd$count == 1,]

##### SELECTIONNER SEULEMENT LES TRANSITIONS SURES
df0 <- df0[df0$ce_transition  > 0,]

##### SELECTIONNER SEULEMENT LES POINTS QUI TOMBENT SUR LA CARTE
df0 <- df0[df0$map_transition > 0,]

df0$ce_prov_transition <- as.numeric(df0$ce_prov_transition)

#### NIVEAU NATIONAL
map_code <- "map_transition"
ref_code <- "ce_transition"
ar_code  <- "transition_clean"

##### RESTRICT THE AREAS TO ONLY EXISTING PROVINCES
ar_area <- "area_ha"
ar      <- ar[ar$transition_clean != 00 & !is.na(ar$province),]

#### CREER UN CHAMPS LAND COVER TERTIAIRE REMPLI AVEC SECONDAIRE SI MANQUANT
df0$tertiary_filled <- df0$tertiary_land_cover_label
nrow(df0[is.na(df0$tertiary_filled),])

df0[df0$tertiary_filled == "" | is.na(df0$tertiary_filled),]$tertiary_filled <- df0[df0$tertiary_filled == "" | is.na(df0$tertiary_filled),]$secondary_land_cover_label
df0[df0$tertiary_filled == "" | is.na(df0$tertiary_filled),]$tertiary_filled <- df0[df0$tertiary_filled == "" | is.na(df0$tertiary_filled),]$primary_land_cover_label

table(df0$tertiary_filled,df0$tertiary_land_cover_label)
head(df0[df0$tertiary_filled == "",])

##### FAIRE TOURNER POUR CHAQUE PROVINCE
province <- "Kinshasa"
for(province in unique(ar$NOM)){
  #  c("Mai-Ndombe","Kwilu","Kwango","Equateur","Sud-Ubangi","Nord-Ubangi","Mongala","Tshuapa","Tshopo","Bas-Uele","Haut-Uele","Ituri")){
  
  legend <- unique(ar[ar$NOM == province,ar_code])
  
  areas <- data.frame(cbind(legend,
                            tapply(ar[ar$NOM == province,ar_area],
                                   ar[ar$NOM == province,ar_code],
                                   sum)))
  
  names(areas) <- c("class","area")
  
  df <- df0[df0$province_name == province,]
  
  s <- saea(df,0.9,areas,legend,map_code,ref_code)
  
  subc     <- table(df[df$ce_transition == 31 & df$periode == "2000_2010",]$tertiary_filled,
                    df[df$ce_transition == 31 & df$periode == "2000_2010",]$secondary_land_cover1_label)
  
  loss0010 <- as.data.frame.matrix(subc/sum(subc)*s[4,"strRS_area_estimate"])
  ci0010   <- as.data.frame.matrix(subc/sum(subc)*s[4,"strRS_confidence_interval"])
  
  subc     <- table(df[df$ce_transition == 23 & df$periode == "2010_2014",]$tertiary_filled,
                    df[df$ce_transition == 23 & df$periode == "2010_2014",]$secondary_land_cover1_label)
  
  loss1014 <- as.data.frame.matrix(subc/sum(subc)*s[3,"strRS_area_estimate"])
  ci1014   <- as.data.frame.matrix(subc/sum(subc)*s[3,"strRS_confidence_interval"])
  
  prov0010 <- cbind(loss0010,ci0010)
  names(prov0010) <- c(paste0("area_",names(loss0010)),paste0("ci_",names(ci0010)))
  
  prov1014 <- cbind(loss1014,ci1014)
  names(prov1014) <- c(paste0("area_",names(loss1014)),paste0("ci_",names(ci1014)))
  
  write.csv(prov0010,paste0(drvdir,province,"_",nrow(df0),"_0010.csv"))
  write.csv(prov1014,paste0(drvdir,province,"_",nrow(df0),"_1014.csv"))
  
}
