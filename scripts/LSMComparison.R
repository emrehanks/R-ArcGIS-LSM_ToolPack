#####################################################################################################  
### Article Name: A Novel Feature Selection Tool Based on Integrating R with ArcMap For Producing Landslide Susceptibility Mapping
### Author(s): Emrehan Kutlug SAHýN ----- emrehansahin@ibu.edu.tr
###            Ismail COLKESEN -----  icolkesen@gtu.edu.tr
###            Aykut AKGUN  ----- aykutakgun@ktu.edu.tr
###            Arif Cagdas AYDINOGLU ----- aaydinoglu@gtu.edu.tr
###            Suheda Semih ACMALI  ---- suhedasemihacmali@gmail.com
#####################################################################################################  
###########   PURPOSE   ##############
#####################################################################################################
#########################
###  
### Comparison of LSM produced
###
##################################################################################################### 

tool_exec <- function(in_params, out_params)
{
  
  #####################################################################################################  
  ### Check/Load required packages
  #####################################################################################################   
  
  round(memory.limit()/2^20, 2) 
  set.seed(24)
  
  library(arcgisbinding)
  arc.check_product()
  arc.progress_label("Loading Packages...")
  arc.progress_pos(0)
  
  if (!requireNamespace("rgdal", quietly = TRUE))
    install.packages("rgdal")
  if (!requireNamespace("raster", quietly = TRUE))
    install.packages("raster")
  if (!requireNamespace("sp", quietly = TRUE))
    install.packages("sp")
  if (!requireNamespace("classInt", quietly = TRUE))
    install.packages("classInt")
  if (!requireNamespace("dplyr", quietly = TRUE))
    install.packages("dplyr")
  if (!requireNamespace("stats", quietly = TRUE))
    install.packages("stats")
  if (!requireNamespace("Metrics", quietly = TRUE))
    install.packages("Metrics")
  if (!requireNamespace("irr", quietly = TRUE))
    install.packages("irr")
  if (!requireNamespace("xlsx", quietly = TRUE))
    install.packages("xlsx")
  if (!requireNamespace("svDialogs", quietly = TRUE))
    install.packages("svDialogs")
  if (!requireNamespace("pROC", quietly = TRUE))
    install.packages("pROC")

  require(rgdal)
  require(raster)
  require(sp)
  require(classInt)
  require(dplyr)
  require(stats)
  require(Metrics)
  require(irr)
  require(xlsx)
  require(svDialogs)
  require(pROC)
  
  ##################################################################################################### 
  ### Define functions
  ##################################################################################################### 
  
  #Raster to Data Frame
  FeatureData <- function(features,train){
    train <- resample(train,features, resample='bilinear')
    
    predictors<-stack(features,train)
    names(predictors)[length(names(predictors))]<-"train"
    names(predictors)
    
    value_table=getValues(predictors)
    value_table=na.omit(value_table)
    value_table=as.data.frame(value_table)
    value_table$train <- rounded_value(value_table$train)
    return(value_table)
    
  }
  
  rounded_value <- function(value) {
    value <- round(value,digits = 0)
    return (value)
  }
  
  #---- raster Normalization ------
  normalizationraster <- function(r){
    
    r.min = cellStats(r, "min")
    r.max = cellStats(r, "max")
    
    r.normal <- ((r - r.min) / (r.max - r.min) )
    return(r.normal)
  }
  
  
  #Raster Classifier
  funclasifier <- function(x,y = "quantile",n = 5){
    if(y == "fisher"){
      breaks <- classIntervals(sampleRandom(x,1000), n=n,style=y,warnLargeN = FALSE)$brks
      breaks <- unique(breaks)
    }
    else{
      breaks <- classIntervals(values(x), n=n,style=y,warnLargeN = FALSE)$brks
      breaks <- unique(breaks)
    }
    with(x, cut(x,breaks=breaks, na.rm=TRUE),include.lowest=TRUE)
  }
  
  #raster manual classifier
  funmanual = function(x){
    a <-min(values(x),na.rm = TRUE)
    b <-max(values(x),na.rm = TRUE)
    kirilmalar <- as.numeric(c(a,((b-a)*0.2),((b-a)*0.4),((b-a)*0.6),((b-a)*0.8),b))
    
    with(x, cut(x,breaks=kirilmalar, na.rm=TRUE),include.lowest=TRUE)
  }
  
  getFileNameExtension <- function (filePath) {
    # remove a path
    splitted    <- strsplit(x=filePath, split='/')[[1]]   
    # or use .Platform$file.sep in stead of '/'
    filePath          <- splitted [length(splitted)]
    ext         <- ''
    splitted    <- strsplit(x=filePath, split='\\.')[[1]]
    l           <-length (splitted)
    if (l > 1 && sum(splitted[1:(l-1)] != ''))  ext <-splitted [l] 
    # the extention must be the suffix of a non-empty name    
    ext
  }
  
  ##################################################################################################### 
  ### Define input/output parameters
  #####################################################################################################  
  
  arc.progress_label("Reading Data...")
  arc.progress_pos(20)
  
  rfiles1 <- in_params[[1]]
  classifierName <- in_params[[2]]
  cutoff <- in_params[[3]]
  testPath <- in_params[[4]]
  kayitPath <- out_params[[1]]
  rocPath <- out_params[[2]]
   
  ##################################################################################################### 
  ### Load Data
  #####################################################################################################
  
  #Read Test Raster Data
  # train <- arc.raster(arc.open(testPath))
  # train <- arc.data2sp(train)
  train <- raster(testPath)
  
  #HDH haritalarýn okunmasý
  tryCatch({
    s <- stack(rfiles1)
  }, warning = function(w) {
    print("bir hata olustu.")
  }, error = function(e) {
    msg_box("There are Xmin and Ymin Dimension Problems in the data you upload. \n
           Please make sure your data is in the same projection system and the corner coordinates are the same. \n
            You can troubleshoot this error using \"project raster\".")
    return(out_params)
  }, finally = {
  })

  ##################################################################################################### 
  ### Classification process
  #####################################################################################################
  
  arc.progress_label("Dividing Data by Selected Classifier..")
  arc.progress_pos(30)
  
  if(classifierName == "fisher"){
    normalraster <- stack(lapply(1:nlayers(s), function(x){normalizationraster(s[[x]])}))
    classout <-  suppressWarnings({
    stack(lapply(1:nlayers(normalraster), function(x){funclasifier(normalraster[[x]],"fisher")}))
  })
  }else if(classifierName == "quantile"){
    normalraster <- stack(lapply(1:nlayers(s), function(x){normalizationraster(s[[x]])}))
    classout <- suppressWarnings({
    stack(lapply(1:nlayers(normalraster), function(x){funclasifier(normalraster[[x]],"quantile")}))
  })
  }else if(classifierName == "manual"){
    normalraster <- stack(lapply(1:nlayers(s), function(x){normalizationraster(s[[x]])}))
    classout <- stack(lapply(1:nlayers(normalraster), function(x){funmanual(normalraster[[x]])}))
  }else if(classifierName == "equal"){
    normalraster <- stack(lapply(1:nlayers(s), function(x){normalizationraster(s[[x]])}))
    classout <- suppressWarnings({
    stack(lapply(1:nlayers(normalraster), function(x){funclasifier(normalraster[[x]],"equal")}))
  })
  }else if(classifierName == "Non-Classifier"){
    classout <- s
  }
  
  names(classout) <- names(s)
  
  ##################################################################################################### 
  ### Create training and testing dataset
  #####################################################################################################
  
  arc.progress_label("Preparing Data Set...")
  arc.progress_pos(40)
  
  traindata <- FeatureData(classout,train)
  
  Rocdata <- FeatureData(s,train)
  trainMatrix <- traindata[,-ncol(traindata)]
  trainMatrix <- ifelse(trainMatrix >= cutoff,1,0)

  ##################################################################################################### 
  ### Calculating LSM metrics(Accuracy, AUC(Classified), MSE, MAE, RMSE, AUC(Raw), Kappa, Precision, Recall, F1)
  #####################################################################################################
  
  arc.progress_label("Calculating LSM metrics(Accuracy, AUC(Classified), MSE, MAE, RMSE, AUC(Raw), Kappa, Precision, Recall, F1)")
  arc.progress_pos(70)
  
  n <- length(traindata)
  metricdata <- ifelse(traindata[c(1:(n-1))] >= cutoff,1,0)
  metricdata <- data.frame(metricdata)
  metricdata$train <- traindata$train
  metricdata <- as.matrix(metricdata)
  rocdata <- as.matrix(data.frame(Rocdata))
  
  n <- (ncol(metricdata) - 1)
  resultAUC <- matrix(nrow = 1, ncol = n)
  resultRMSE <- matrix(nrow = 1, ncol = n)
  resultMSE <- matrix(nrow = 1, ncol = n)
  resultMAE <- matrix(nrow = 1, ncol = n)
  resultF1 <- matrix(nrow = 1, ncol = n)
  resultPrecision <- matrix(nrow = 1, ncol = n)
  resultRecall <- matrix(nrow = 1, ncol = n)
  resultAccuracy <- matrix(nrow = 1, ncol = n)
  resultKappaTest <- matrix(nrow = 1, ncol = n)
  resultAUCRaw <- matrix(nrow = 1, ncol = n)
  
  for(i in 1:n){
    for(j in i:n){
      x1 <- metricdata[,i]
      x2 <- metricdata[,ncol(metricdata)]
      x3 <- rocdata[,i]
      x4 <- rocdata[,ncol(rocdata)]
      
      resultRMSE[i] <- rmse(x2,x1)
      resultMSE[i] <- mse(x2,x1)
      resultAUC[i] <- auc(x2,x1)
      resultAUCRaw[i] <- auc(x4,x3)
      resultAccuracy[i] <- accuracy(x2,x1)
      resultMAE[i] <- mae(x2,x1)
      resultPrecision[i] <- precision(x2,x1)
      resultRecall[i] <- recall(x2,x1)
      resultF1[i] <- (2*(resultPrecision[i]*resultRecall[i])/(resultPrecision[i]+resultRecall[i]))
      resultKappaTest[i]<-kappa2(data.frame(x2,x1))$value
      
    }
  }
  
  colnames(resultAUCRaw) <- colnames(resultAccuracy) <- colnames(resultF1) <- colnames(resultAUC) <- colnames(resultMAE) <- colnames(resultMSE) <- colnames(resultRMSE) <- colnames(metricdata)[1:n]
  
  ##################################################################################################### 
  ### Write LSM metric results
  #####################################################################################################
  
  arc.progress_label("Write LSM metric results")
  arc.progress_pos(90)
  
  result <- matrix(data = c(resultAccuracy,resultAUC,resultAUCRaw,resultMAE,resultMSE,resultRMSE,resultKappaTest,resultPrecision, resultRecall,resultF1), nrow = n)
  colnames(result) <- c("Accuracy","AUC(Classified)","AUC(RAW)","MAE","MSE","RMSE","Kappa","Precision","Recall","F1")
  rownames(result) <- colnames(resultAccuracy)
  
  fileFormat <- getFileNameExtension(kayitPath)
  
  if(fileFormat == "txt"){
    
    write.table(result,file = kayitPath,append = FALSE, sep = " ", dec = ".",
                row.names = TRUE, col.names = TRUE)
  }else if(fileFormat == "xls"){
    
    write.xlsx(result,file = kayitPath,col.names = T, row.names = T)
  }else if(fileFormat == "xlsx"){
    
    write.xlsx(result,file = kayitPath,col.names = T, row.names = T)
  }else{
    cat("Please Select a File Format in The List!")
  }
  
  #--- Write Out ROC ----------
  tiff(filename = rocPath,width = 2400, height = 2400,res = 300)
  par(pty = "s")
  color <- rainbow(ncol(trainMatrix))
  r1 <- lapply(1:ncol(trainMatrix), function(x) NULL)
  listofAUC <- round(as.numeric(resultAUC),digits = 3)
  legendnames <- names(s)
 
  for(i in 1:ncol(trainMatrix)){
    r1[[i]] <-   roc(response = Rocdata$train, predictor = Rocdata[[i]], plot=F,legacy.axes = TRUE,percent = TRUE)
    listofAUC[[i]] <- round(r1[[i]]$auc, digits = 3)
    if(i == 1){
      plot(r1[[i]],col = color[i])
    }else{ 
      lines(r1[[i]],col = color[i])
    }
  }
  a <- paste(legendnames,"  AUC : ",as.character(listofAUC),sep = "")
  legend("bottomright",a, col = color,cex = 1,lwd = 1:2)
  dev.off()
  
  arc.progress_pos(100)
  return(out_params)
  
}















