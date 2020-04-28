####################################################################################
####### Object:  EVALUATION PRECISION BDD TRANSITION        
####### Author:  remi.dannunzio@fao.org                               
####### Update:  2019/02/05                                    
####################################################################################

##### LIRE LES DEUX BASES (POINTS ET SUPERFICIES)
bd <- read.csv(paste0(datadir,"bd_2000_2010_2014_v20190725.csv"))

##### PRENDRE TOUS LES POINTS DES TRANSITIONS STABLES
bd$ce_prov_transition <- paste0(bd$map_province,bd$ce_transition)

bd$map_cl <- paste0(bd$map_province,bd$map_class1014)
bd$ref_cl <- paste0(bd$map_province,bd$ce_change)

bd$map_change <- substr(bd$map_cl,nchar(bd$map_cl),nchar(bd$map_cl))
bd$ce_change  <- substr(bd$ref_cl,nchar(bd$ref_cl),nchar(bd$ref_cl))

table(bd$map_change,bd$ce_change)

ar <- read.csv(paste0(datadir,"areas_transitions_2000_2010_2014_20190724.csv"))
ar$cls_cl <- paste0(ar$province,ar$class1014)

table(ar$transition_clean,ar$class1014)

##### SELECTIONNER SEULEMENT LES SINGLETS
df0 <- bd[bd$count ==1 & bd$periode == '2010_2014',]

table(df0$map_cl)

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
legend  <- unique(ar[,ar_code])
#legend <- levels(as.factor(df0[, ref_code]))
areas   <- data.frame(cbind(legend,tapply(ar[,ar_area],ar[,ar_code],sum)))

names(areas) <- c("prov_class","area")
areas[,"area"] <- as.numeric(areas[,"area"])
sum(areas[,"area"])

a0 <- areas
names(a0)   <- c("prov_class","area")
a0$province <- substr(a0$prov_class,1,nchar(a0$prov_class)-1) 
a0$class    <- substr(a0$prov_class,nchar(a0$prov_class),nchar(a0$prov_class)) 

a0$eua <- 0.9
a0[a0$class == 3 ,]$eua <- 0.5

############### Add a column for Standard Error and Weighted SE
a0$si <- sqrt(a0$eua * (1 - a0$eua))

area_prov <- data.frame(tapply(a0$area,a0$province,sum))

area_prov$province <- row.names(area_prov)
names(area_prov) <- c("area_province","province")

a1      <- merge(a0,area_prov,by.x="province",by.y="province",all.x=T)
a1$wi   <- a1$area / a1$area_province
a1$wisi <- a1$wi * a1$si

############### Compute overall sampling size

wisi_prov <- data.frame(tapply(a1$wisi,a1$province,sum))
wisi_prov$province <- row.names(wisi_prov)
names(wisi_prov) <- c("wisi_province","province")

a2 <- merge(a1,wisi_prov,by.x="province",by.y="province",all.x=T)

a2$overallsample <- (a2$wisi_province / 0.01) ^ 2

############### Compute sampling repartition
a2$proportional  <- floor(a2$wi * a2$overallsample)

a2$max   <- apply(cbind(a2$proportional,120), 1, max)
a2$diff  <- a2$max - a2$proportional
a2$final <- 0

a4       <- a2[0,]

for(province in unique(a2$province)){
  
  a3            <- a2[a2$province == province,]
  a3$adjusted   <- a3$area/sum(a3$area)*(a3$overallsample-3*120)+120
  # a3$adjusted <- a3$area / (sum(a3$area[a3$proportional >= 120])) * (a3$overallsample - sum(a3$max[a3$proportional < 120]))
  
  # a3$adjusted[a3$adjusted < 120] <- 120
  a3$adjusted <- floor(a3$adjusted)
  
  a4 <- rbind(a4,a3)
}

a4


df0[,map_code] <- as.numeric(df0[,map_code])
df0[,ref_code] <- as.numeric(df0[,ref_code])

areas[,"prov_class"] <- as.numeric(areas[,"prov_class"])
names(areas) <- c("class","area")

resultat <- data.frame(matrix(ncol=6,nrow=0))
names(resultat) <- c("area_NF","area_F","area_DEF","ci_NF","ci_F","ci_DEF")

for(simulation in 1:30){
  print(simulation)

    ids <- list()

  for(class in unique(df0$map_cl)){
    tmp <- sample(df0[df0$map_cl == class,"unique_id"],
                  min(a4[a4$prov_class == class,"adjusted"],nrow(df0[df0$map_cl == class,])))
    
    ids <- c(ids,tmp)
  }
  
  df <- df0[df0$unique_id %in% ids,]
  
  s <- saea(df,0.9,areas,legend,map_code,ref_code)
  
  s$class_code <- as.numeric(substr(s$class,nchar(s$class),nchar(s$class)))
  s$province   <- as.numeric(substr(s$class,0,nchar(s$class)-1))
  
  codes <- read.dbf(paste0(datadir,"RDC_Province_26.dbf"))[,c("NOM","ID_SEPAL")]
  names(codes) <- c("province_name","ID_SEPAL")
  
  s1  <- merge(s,codes,by.y="ID_SEPAL",by.x="province",all.x=T)
  r1  <- tapply(s1$strRS_area_estimate,s1[,c("class_code")],sum)
  i1  <- sqrt(tapply(s1$strRS_confidence_interval*s1$strRS_confidence_interval,s1[,c("class_code")],sum))
  c1  <- tapply(s1$map_pixel_count,s1[,c("class_code")],sum)
  resultat <- rbind(resultat,c(r1,i1))
  
  write.csv(s1,paste0(datadir,"res_sim",simulation,"_2010_2014.csv"),row.names = F)  
  
}

names(resultat) <- c("area_NF","area_F","area_DEF","ci_NF","ci_F","ci_DEF")

resultat

write.csv(resultat,paste0(datadir,"res_sim_2010_2014.csv"),row.names = F)

