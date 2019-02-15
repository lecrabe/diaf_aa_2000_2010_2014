####################################################################################
####### Object:  EVALUATION PRECISION BDD TRANSITION        
####### Author:  remi.dannunzio@fao.org                               
####### Update:  2019/02/05                                    
####################################################################################


df0 <- read.csv(paste0(datadir,"bd_2000_2010_2014_v20190215.csv"))
map_code <- "map_transition"      #"map_prov_transition"
ref_code <- "ce_transition"       #"ce_prov_transition"

ar <- read.csv(paste0(datadir,"areas_transitions_2000_2010_2014.csv"))
ar_code <- "transition_clean"
ar_area <- "area_ha"

ar <- ar[ar$transition_clean != 00 & !is.na(ar$province),]

legend <- unique(ar[ar$NOM == "Kwango",ar_code])

areas <- data.frame(cbind(legend,tapply(ar[ar$NOM == "Kwango",ar_area],ar[ar$NOM == "Kwango",ar_code],sum)))
names(areas) <- c("class","area")

df <- df0[df0$province_nom == "Kwango" 
          & (df0$map_transition == 23 
                     | df0$map_transition == 31 
                     | (df0$map_transition == 11 & df0$period=="p0010")
                     | (df0$map_transition == 22 & df0$period=="p1014")
          ),
          ]

s<-saea(df,0.9,areas,legend)

s$transition <- substr(s$class,nchar(s$class)-1,nchar(s$class))
s$province   <- as.numeric(substr(s$class,0,nchar(s$class)-2))

codes <- read.dbf(paste0(datadir,"RDC_Province_26.dbf"))[,c("NOM","ID_SEPAL")]
s1   <- merge(s,codes,by.y="ID_SEPAL",by.x="province",all.x=T)
tapply(s1$strRS_area_estimate,s1[,c("transition")],sum)
tapply(s1$strRS_area_estimate,s1[,c("NOM","transition")],sum)

table(df[df$periode == "p0010",]$tertiary_land_cover_label,
      df[df$periode == "p0010",]$secondary_land_cover1_label,
      df[df$periode == "p0010",]$ce_transition)

table(df[df$periode == "p0010",]$tertiary_land_cover_label,
      df[df$periode == "p0010",]$quarternary_land_cover_label,
      
      df[df$periode == "p0010",]$ce_transition)

table(df[df$periode == "p0010",]$ce_transition,df[df$periode == "p0010",]$primary_land_cover1_label)

table(df[df$periode == "p1014",]$ce_transition,df[df$periode == "p1014",]$primary_land_cover_label)

write.csv(s1,paste0(datadir,"resultats_transitions_20190210.csv"),row.names = F)
