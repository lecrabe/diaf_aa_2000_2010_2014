####################################################################################
####### Object:  EVALUATION PRECISION BDD TRANSITION        
####### Author:  remi.dannunzio@fao.org                               
####### Update:  2019/02/05                                    
####################################################################################


df0 <- read.csv(paste0(datadir,"bd_2000_2010_2014_v20190210.csv"))
map_code <- "map_prov_transition"
ref_code <- "ce_prov_transition"

ar <- read.csv(paste0(datadir,"areas_transitions_2000_2010_2014.csv"))
ar_code <- "reclass"
ar_area <- "area_ha"

ar <- ar[ar$transition_clean != 00 & !is.na(ar$province),]

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

df <- df0[df0$map_transition == 23 
                     | df0$map_transition == 31 
                     | (df0$map_transition == 11 & df0$period=="p0010")
                     | (df0$map_transition == 22 & df0$period=="p1014"),
                       ]

s<-saea(df0,0.9)

s<-saea(df,0.9)

s$transition <- substr(s$class,nchar(s$class)-1,nchar(s$class))
s$province   <- as.numeric(substr(s$class,0,nchar(s$class)-2))

codes <- read.dbf(paste0(datadir,"RDC_Province_26.dbf"))[,c("NOM","ID_SEPAL")]
s1   <- merge(s,codes,by.y="ID_SEPAL",by.x="province",all.x=T)
tapply(s1$strRS_area_estimate,s1[,c("transition")],sum)
tapply(s1$strRS_area_estimate,s1[,c("NOM","transition")],sum)

write.csv(s1,paste0(datadir,"resultats_transitions_20190210.csv"),row.names = F)
