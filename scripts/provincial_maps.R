####################################################################################
####### Object:  Produire la carte de transition + points pour chaque province    
####### Author:  remi.dannunzio@fao.org                               
####### Update:  2019/02/09                                    
####################################################################################



rdc <- readOGR(paste0(datadir,"RDC_Province_26.shp"))



spdf_geo <- SpatialPointsDataFrame(
  coords = df[,c("location_x","location_y")],
  data   = df,
  proj4string=CRS("+init=epsg:4326")
)

spdf <- spTransform(spdf_geo,proj4string(rdc))
dev.off()


#################### CREATE A COLOR TABLE FOR THE OUTPUT MAP
my_classes <- c(0,11,22,23,31)
my_colors  <- col2rgb(c("black","grey","darkgreen","red","orange"))

pct <- data.frame(cbind(my_classes,
                        my_colors[1,],
                        my_colors[2,],
                        my_colors[3,]))

write.table(pct,paste0(datadir,"color_table.txt"),row.names = F,col.names = F,quote = F)



par(mfrow = c(3,9))
#par(mar=c(0,0,0,0))
map <- paste0(datadir,"diaf_2000_2010_2014_clean.tif")
i=1

for(i in 1:nrow(rdc)){

  province <- rdc[i,]
  pts <- spdf[province,]
  name <- gsub(" ","_",paste0(province$NOM))
  
  png(file=  paste0(imgdir,"map_",name,".png"),
      width= 800,
      height=800)
  
  par(mar=c(0,0,1,0))
  
  # writeOGR(province,paste0(provdir,"prov_",name,".shp"),
  #          paste0("prov_",name),
  #          "ESRI Shapefile",
  #          overwrite_layer = T)
  # 
  # head(province)
  # extent(province)
  # 
  # system(sprintf("gdal_translate -co COMPRESS=LZW  -projwin %s %s %s %s %s \"%s\" ",
  #                extent(province)@xmin,
  #                extent(province)@ymax,
  #                extent(province)@xmax,
  #                extent(province)@ymin,
  #                map,
  #                paste0(provdir,"bb_",name,".tif")
  # ))
  # 
  # ##################### RASTERISER LE SHAPEFILE DES PROVINCES SUR LA CARTE JICA DIAF
  # system(sprintf("python %s/oft-cutline_crop.py -v \"%s\" -i \"%s\" -o \"%s\" -a %s ",
  #                scriptdir,
  #                paste0(provdir,"prov_",name,".shp"),
  #                paste0(provdir,"bb_",name,".tif"),
  #                paste0(provdir,"tmp_prov_",name,".tif"),
  #                "ID_SEPAL"
  # ))
  # 
  # ################################################################################
  # #################### Add pseudo color table to result
  # ################################################################################
  # system(sprintf("(echo %s) | oft-addpct.py \"%s\" \"%s\" ",
  #                paste0(datadir,"color_table.txt"),
  #                paste0(provdir,"tmp_prov_",name,".tif"),
  #                paste0(provdir,"tmp_prov_pct_",name,".tif")
  # ))
  # 
  # ################################################################################
  # #################### COMPRESS
  # ################################################################################
  # system(sprintf("gdal_translate -ot Byte -co COMPRESS=LZW \"%s\" \"%s\" ",
  #                paste0(provdir,"tmp_prov_pct_",name,".tif"),
  #                paste0(provdir,"pct_",name,".tif")
  # ))
  # system(sprintf("rm %s",
  #                paste0(provdir,"tmp_*.tif")
  # ))
  
  plot(province,main=province$NOM,font.main=1)
  plot(raster(paste0(provdir,"pct_",name,".tif")),add=T)
  points(pts[pts$ce_transition ==11,],pch=20,col="black")
  points(pts[pts$ce_transition ==22,],pch=20,col="green")
  points(pts[pts$ce_transition ==23,],pch=20,col="red")
  points(pts[pts$ce_transition ==31,],pch=20,col="orange")

  dev.off()
  
  }

# system(sprintf("rm %s",
#                paste0(imgdir,"map_*.png")
# ))
