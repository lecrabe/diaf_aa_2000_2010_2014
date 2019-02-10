####################################################################################
####### Object:  SIMULATIONS AA         
####### Author:  remi.dannunzio@fao.org                               
####### Update:  2019/02/05                                    
####################################################################################


df0 <- read.csv(paste0(datadir,"bd_2000_2010_v20190207.csv"))
map_code <- "map_change"
ref_code <- "ce_change"

ar <- read.csv(paste0(datadir,"areas_2000_2010.csv"))
ar_code <- "change"
ar_area <- "area_ha"


legend <- unique(ar[,ar_code])

areas <- data.frame(cbind(legend,tapply(ar[,ar_area],ar[,ar_code],sum)))
names(areas) <- c("class","area")

saea <- function(df,the_ci){
  tmp <- table(df[,map_code],df[,ref_code])
  tmp[is.na(tmp)] <- 0
  
  matrix <- matrix(0, nrow = length(legend), ncol = length(legend))
  
  for (i in 1:length(legend)) {
    tryCatch({
      #cat(paste(legend[i], "\n"))
      matrix[, i] <- tmp[, legend[i]]
    }, error = function(e) {
      cat("Not relevant\n")
    })
  }
  
  matrix[is.na(matrix)] <- 0
  
  matrix_w <- matrix
  for (i in 1:length(legend)) {
    for (j in 1:length(legend)) {
      tryCatch({
        matrix_w[i, j] <-
          matrix[i, j] / sum(matrix[i, ]) * areas[areas[,"class"] == legend[i],"area" ] / sum(areas[,"area"])
      }, error = function(e) {
        cat("Not relevant\n")
      })
    }
  }
  
  matrix_se <- matrix
  for (i in 1:length(legend)) {
    for (j in 1:length(legend)) {
      tryCatch({
        matrix_se[i, j] <-
          areas[areas[, "class"] == legend[i],"area" ] / sum(areas[,"area"]) *
          areas[areas[, "class"] == legend[i], "area"] /
          sum(areas[,"area"]) *
          matrix[i, j] /
          sum(matrix[i, ]) *
          (1 - matrix[i, j] / sum(matrix[i, ])) /
          (sum(matrix[i, ]) - 1)
      }, error = function(e) {
        cat("Not relevant\n")
        #print(legend[i])
      })
    }
  }
  
  matrix_se[is.na(matrix_se)] <- 0
  
  confusion <- data.frame(matrix(nrow = length(legend), ncol = 10))
  names(confusion) <-
    c(
      "class",
      "code",
      "producers_accuracy",
      "weighted_producers_accuracy",
      "users_accuracy",
      "map_pixel_count",
      "strRS_area_estimate",
      "strRS_standard_error",
      "strRS_confidence_interval",
      "number_samples"
    )
  
  
  ## the zscores for the confidence intervals
  ci <- c(0.9, 0.95, 0.99)
  z <- c(1.645, 1.96, 2.576)
  citable <- data.frame(ci, z)
  civalue <- citable$z[citable$ci %in% as.numeric(the_ci)]
  
  ### Integration of all elements into one dataframe
  for (i in 1:length(legend)) {
    confusion[i, ]$class <- areas[areas[, "class"] == legend[i], "class"]
    confusion[i, ]$code  <- areas[areas[, "class"] == legend[i], "class"]
    confusion[i, ]$strRS_area_estimate <- sum(matrix_w[, i]) * sum(areas[,"area"])
    confusion[i, ]$producers_accuracy  <- matrix[i, i] / sum(matrix[, i])
    confusion[i, ]$users_accuracy      <- matrix[i, i] / sum(matrix[i, ])
    confusion[i, ]$weighted_producers_accuracy    <-  matrix_w[i, i] / sum(matrix_w[, i])
    confusion[i, ]$map_pixel_count                <-  areas[areas[, "class"] == legend[i],"area" ]
    confusion[i, ]$strRS_standard_error           <-  sqrt(sum(matrix_se[, i])) * sum(areas[,"area"])
    confusion[i, ]$strRS_confidence_interval      <-  confusion[i, ]$strRS_standard_error * civalue
    confusion[i, ]$number_samples                 <-  sum(matrix[,i])
  }
  confusion
}

saea(df0,0.9)

# prop13 <- table(df0$map_change)[1]*areas[3,"area"]/areas[1,"area"]
# prop23 <- table(df0$map_change)[2]*areas[3,"area"]/areas[2,"area"]
# prop21 <- table(df0$map_change)[2]*areas[1,"area"]/areas[2,"area"]

areas$ratio <- areas[,"area"]/sum(areas[,"area"])

niter <- 100
sims <- data.frame(matrix(nrow = niter, ncol = 9))
names(sims) <- c("area_nf","ci_nf","area_f","ci_f","area_def","ci_def","total_f","total_nf","total_def")

###################### BALANCED SAMPLING, INCREASING TOTAL NUMBER OF POINTS
for(iter in 1:niter){
  sampling <- 1000 + (iter-1)*floor(nrow(df0)-1000)/niter

  sampling_nf  <- min(table(df0$map_change)[1],sampling*areas$ratio[1])
  sampling_f   <- min(table(df0$map_change)[2],sampling*areas$ratio[2])
  sampling_def <- min(table(df0$map_change)[3],sampling*areas$ratio[3])
  
  df1 <- df0[df0[,map_code] == 1,]
  df1 <- df1[sample(1:nrow(df1),sampling_nf),]
  
  df3 <- df0[df0[,map_code] == 3,]
  df3 <- df3[sample(1:nrow(df3),sampling_def),]
  
  df2 <- df0[df0[,map_code] == 2,]
  df2 <- df2[sample(1:nrow(df2),sampling_f),]
  
  df <- rbind(df1,df2,df3)
  
  confusion <- saea(df,0.9)
  
  sims[iter,1] <- confusion[1,"strRS_area_estimate"]
  sims[iter,2] <- confusion[1,"strRS_confidence_interval"]
  sims[iter,3] <- confusion[2,"strRS_area_estimate"]
  sims[iter,4] <- confusion[2,"strRS_confidence_interval"]
  sims[iter,5] <- confusion[3,"strRS_area_estimate"]
  sims[iter,6] <- confusion[3,"strRS_confidence_interval"]
  sims[iter,7] <- sampling_f
  sims[iter,8] <- sampling_nf
  sims[iter,9] <- sampling_def
}

sims[,1:6] <- sims[,1:6]/1000

png(file= paste0(rootdir,"balanced.png") ,
    width= 1200*1,
    height=500*3)

par(mfrow = c(3,1))
#par(mar=c(0,0,0,0))

plot(sims$total_def,
     sims$area_def,
     ylim=range(c(sims$area_def-2*sims$ci_def, sims$area_def+2*sims$ci_def)),
     xlab="Points sampled in the map class",
     ylab="Deforestation area (1000 ha)"
     )
arrows(sims$total_def,sims$area_def-sims$ci_def,sims$total_def,sims$area_def+sims$ci_def, length=0.05, angle=90, code=3)

plot(sims$total_f,
     sims$area_f,
     ylim=range(c(sims$area_f-2*sims$ci_f, sims$area_f+2*sims$ci_f)),
     xlab="Points sampled in the map class",
     ylab="Forest area (1000 ha)"
)
arrows(sims$total_f,sims$area_f-sims$ci_f,sims$total_f,sims$area_f+sims$ci_f, length=0.05, angle=90, code=3)

plot(sims$total_nf,
     sims$area_nf,
     ylim=range(c(sims$area_nf-2*sims$ci_nf, sims$area_nf+2*sims$ci_nf)),
     xlab="Points sampled in the map class",
     ylab="Non forest area (1000 ha)"
)
arrows(sims$total_nf,sims$area_nf-sims$ci_nf,sims$total_nf,sims$area_nf+sims$ci_nf, length=0.05, angle=90, code=3)
dev.off()

###################### EQUAL SAMPLING, INCREASING TOTAL NUMBER OF POINTS
for(iter in 1:niter){
  sampling <- 1000 + (iter-1)*floor(nrow(df0)-1000)/niter
  
  sampling_nf  <- min(table(df0$map_change)[1],sampling/3)
  sampling_f   <- min(table(df0$map_change)[2],sampling/3)
  sampling_def <- min(table(df0$map_change)[3],sampling/3)
  
  df1 <- df0[df0[,map_code] == 1,]
  df1 <- df1[sample(1:nrow(df1),sampling_nf),]
  
  df3 <- df0[df0[,map_code] == 3,]
  df3 <- df3[sample(1:nrow(df3),sampling_def),]
  
  df2 <- df0[df0[,map_code] == 2,]
  df2 <- df2[sample(1:nrow(df2),sampling_f),]
  
  df <- rbind(df1,df2,df3)
  
  confusion <- saea(df,0.9)
  
  sims[iter,1] <- confusion[1,"strRS_area_estimate"]
  sims[iter,2] <- confusion[1,"strRS_confidence_interval"]
  sims[iter,3] <- confusion[2,"strRS_area_estimate"]
  sims[iter,4] <- confusion[2,"strRS_confidence_interval"]
  sims[iter,5] <- confusion[3,"strRS_area_estimate"]
  sims[iter,6] <- confusion[3,"strRS_confidence_interval"]
  sims[iter,7] <- sampling_f
  sims[iter,8] <- sampling_nf
  sims[iter,9] <- sampling_def
}

sims[,1:6] <- sims[,1:6]/1000

png(file= paste0(rootdir,"equal.png") ,
    width= 1200*1,
    height=500*3)

par(mfrow = c(3,1))
plot(sims$total_def,
     sims$area_def,
     ylim=range(c(sims$area_def-2*sims$ci_def, sims$area_def+2*sims$ci_def)),
     xlab="Points sampled in the map class",
     ylab="Deforestation area (1000 ha)"
)
arrows(sims$total_def,sims$area_def-sims$ci_def,sims$total_def,sims$area_def+sims$ci_def, length=0.05, angle=90, code=3)

plot(sims$total_f,
     sims$area_f,
     ylim=range(c(sims$area_f-2*sims$ci_f, sims$area_f+2*sims$ci_f)),
     xlab="Points sampled in the map class",
     ylab="Forest area (1000 ha)"
)
arrows(sims$total_f,sims$area_f-sims$ci_f,sims$total_f,sims$area_f+sims$ci_f, length=0.05, angle=90, code=3)

plot(sims$total_nf,
     sims$area_nf,
     ylim=range(c(sims$area_nf-2*sims$ci_nf, sims$area_nf+2*sims$ci_nf)),
     xlab="Points sampled in the map class",
     ylab="Non forest area (1000 ha)"
)
arrows(sims$total_nf,sims$area_nf-sims$ci_nf,sims$total_nf,sims$area_nf+sims$ci_nf, length=0.05, angle=90, code=3)

dev.off()

###################### SKEWED SAMPLING TOWARDS DEFORESTATION, INCREASING TOTAL NUMBER OF POINTS
for(iter in 1:niter){
  sampling <- 1000 + (iter-1)*floor(nrow(df0)-1000)/niter
  
  sampling_nf  <- min(table(df0$map_change)[1],sampling*.2)
  sampling_f   <- min(table(df0$map_change)[2],sampling*.2)
  sampling_def <- min(table(df0$map_change)[3],sampling*.6)
  
  df1 <- df0[df0[,map_code] == 1,]
  df1 <- df1[sample(1:nrow(df1),sampling_nf),]
  
  df3 <- df0[df0[,map_code] == 3,]
  df3 <- df3[sample(1:nrow(df3),sampling_def),]
  
  df2 <- df0[df0[,map_code] == 2,]
  df2 <- df2[sample(1:nrow(df2),sampling_f),]
  
  df <- rbind(df1,df2,df3)
  
  confusion <- saea(df,0.9)
  
  sims[iter,1] <- confusion[1,"strRS_area_estimate"]
  sims[iter,2] <- confusion[1,"strRS_confidence_interval"]
  sims[iter,3] <- confusion[2,"strRS_area_estimate"]
  sims[iter,4] <- confusion[2,"strRS_confidence_interval"]
  sims[iter,5] <- confusion[3,"strRS_area_estimate"]
  sims[iter,6] <- confusion[3,"strRS_confidence_interval"]
  sims[iter,7] <- sampling_f
  sims[iter,8] <- sampling_nf
  sims[iter,9] <- sampling_def
}

sims[,1:6] <- sims[,1:6]/1000

png(file= paste0(rootdir,"skewed.png") ,
    width= 1200*1,
    height=500*3)

par(mfrow = c(3,1))
plot(sims$total_def,
     sims$area_def,
     ylim=range(c(sims$area_def-2*sims$ci_def, sims$area_def+2*sims$ci_def)),
     xlab="Points sampled in the map class",
     ylab="Deforestation area (1000 ha)"
)
arrows(sims$total_def,sims$area_def-sims$ci_def,sims$total_def,sims$area_def+sims$ci_def, length=0.05, angle=90, code=3)

plot(sims$total_f,
     sims$area_f,
     ylim=range(c(sims$area_f-2*sims$ci_f, sims$area_f+2*sims$ci_f)),
     xlab="Points sampled in the map class",
     ylab="Forest area (1000 ha)"
)
arrows(sims$total_f,sims$area_f-sims$ci_f,sims$total_f,sims$area_f+sims$ci_f, length=0.05, angle=90, code=3)

plot(sims$total_nf,
     sims$area_nf,
     ylim=range(c(sims$area_nf-2*sims$ci_nf, sims$area_nf+2*sims$ci_nf)),
     xlab="Points sampled in the map class",
     ylab="Non forest area (1000 ha)"
)
arrows(sims$total_nf,sims$area_nf-sims$ci_nf,sims$total_nf,sims$area_nf+sims$ci_nf, length=0.05, angle=90, code=3)

dev.off()
saea(df0,0.9)
