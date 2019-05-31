
####################################################################################
####### Object:  SAEA FUNCTION        
####### Author:  remi.dannunzio@fao.org                               
####### Update:  2019/02/15                                    
####################################################################################
saea <- function(my_df,the_ci,my_areas,my_legend,my_map_code,my_ref_code){
  df <- my_df
  areas <- my_areas
  legend <- my_legend
  map_code <- my_map_code
  ref_code <- my_ref_code
  
  ref_legend <- as.numeric(levels(as.factor(df[,ref_code])))
  
  tmp <- table(df[,map_code],df[,ref_code])
  tmp[is.na(tmp)] <- 0

  matrix <- matrix(0, nrow = length(legend), ncol = length(legend))
  
  for (i in 1:length(legend)) {
    tryCatch({
      #cat(paste(legend[i], "\n"))
      
      matrix[, i] <- tmp[, which(colnames(tmp) == row.names(tmp)[i])]
      
    }, error = function(e) {
      cat(paste0(legend[i]," not collected \n"))
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
  
  confusion <- data.frame(matrix(0,nrow = length(legend), ncol = 10))
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
    tryCatch({
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
    }, error=function(e){cat(paste0(legend[i]," tried \n"))})
  }
  confusion
}