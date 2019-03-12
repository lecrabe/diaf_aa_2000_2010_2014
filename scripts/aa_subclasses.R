####################################################################################
####### Object:  EVALUATION PRECISION BDD TRANSITION        
####### Author:  remi.dannunzio@fao.org                               
####### Update:  2019/02/05                                    
####################################################################################


df0 <- read.csv(paste0(datadir,"bd_2000_2010_2014_v20190215.csv"))
map_code <- "map_transition"      #"map_prov_transition"
ref_code <- "ce_transition"       #"ce_prov_transition"

df0$tertiary_land_cover_label <- gsub(pattern = "Forest Dry or Sparse",
                                      replacement = "Forest, Dry or Sparse",
                                      df0$tertiary_land_cover_label )

df0$tertiary_land_cover_label <- gsub(pattern = "Forest Dense Humid",
                                      replacement = "Forest, Dense, Humid",
                                      df0$tertiary_land_cover_label )


ar <- read.csv(paste0(datadir,"areas_transitions_2000_2010_2014.csv"))
ar_code <- "transition_clean"
ar_area <- "area_ha"

ar <- ar[ar$transition_clean != 00 & !is.na(ar$province),]

province <- "Sud-Ubangi"

for(province in unique(ar$NOM)){
  #  c("Mai-Ndombe","Kwilu","Kwango","Equateur","Sud-Ubangi","Nord-Ubangi","Mongala","Tshuapa","Tshopo","Bas-Uele","Haut-Uele","Ituri")){
  
  legend <- unique(ar[ar$NOM == province,ar_code])
  
  areas <- data.frame(cbind(legend,
                            tapply(ar[ar$NOM == province,ar_area],
                                   ar[ar$NOM == province,ar_code],
                                   sum)))
  
  names(areas) <- c("class","area")
  
  df <- df0[df0$province_nom == province
            &    (df0$map_transition == 23 
                  |  df0$map_transition == 31 
                  | (df0$map_transition == 11 & df0$period=="p0010")
                  | (df0$map_transition == 22 & df0$period=="p1014")
            ),
            ]
  
  s<-saea(df,0.9,areas,legend)
  
  subc <- table(df[df$ce_transition == 31 & df$periode == "p0010",]$tertiary_land_cover_label,
                df[df$ce_transition == 31 & df$periode == "p0010",]$secondary_land_cover1_label)
  
  loss0010 <- as.data.frame.matrix(subc/sum(subc)*s[4,"strRS_area_estimate"])
  ci0010   <- as.data.frame.matrix(subc/sum(subc)*s[4,"strRS_confidence_interval"])
  
  subc <- table(df[df$ce_transition == 23 & df$periode == "p1014",]$tertiary_land_cover_label,
                df[df$ce_transition == 23 & df$periode == "p1014",]$secondary_land_cover1_label)
  
  loss1014 <- as.data.frame.matrix(subc/sum(subc)*s[3,"strRS_area_estimate"])
  ci1014   <- as.data.frame.matrix(subc/sum(subc)*s[3,"strRS_confidence_interval"])
  
  prov0010 <- cbind(loss0010,ci0010)
  names(prov0010) <- c(paste0("area_",names(loss0010)),paste0("ci_",names(ci0010)))
  
  prov1014 <- cbind(loss1014,ci1014)
  names(prov1014) <- c(paste0("area_",names(loss1014)),paste0("ci_",names(ci1014)))
  
  write.csv2(prov0010,paste0(drvdir,province,"_0010.csv"),sep = ";")
  write.csv2(prov1014,paste0(drvdir,province,"_1014.csv"),sep=";")
  
}
