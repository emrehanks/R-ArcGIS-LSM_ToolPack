#####################################################################################################  
### Article Name: A Novel Feature Selection Tool Based on Integrating R with ArcMap For Producing Landslide Susceptibility Mapping
### Author(s): Emrehan Kutlug SAHIN ----- emrehansahin@ibu.edu.tr
###            Ismail COLKESEN -----  icolkesen@gtu.edu.tr
###            Suheda Semih ACMALI  ---- suhedasemihacmali@gmail.com
###            Aykut AKGUN  ----- aykutakgun@ktu.edu.tr
###            Arif Cagdas AYDINOGLU ----- aaydinoglu@gtu.edu.tr
#####################################################################################################  
###########   PURPOSE   ##############
#####################################################################################################
#########################
###  
### Attribute selection using attriute ranking algoritm
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
  if (!requireNamespace("FSelector", quietly = TRUE))
    install.packages("FSelector")
  if (!requireNamespace("data.table", quietly = TRUE))
    install.packages("data.table")
  if (!requireNamespace("xlsx", quietly = TRUE))
    install.packages("xlsx")
  if (!requireNamespace("svDialogs", quietly = TRUE))
    install.packages("svDialogs")
  if (!requireNamespace("caret", quietly = TRUE))
    install.packages("caret")
  if (!requireNamespace("doParallel", quietly = TRUE))
    install.packages("doParallel")
  
  require(rgdal)
  require(raster)
  require(sp)
  require(FSelector)
  require(data.table)
  require(svDialogs)
  require(xlsx)
  require(caret)
  require(doParallel)
  
  ##################################################################################################### 
  ### Define functions
  ##################################################################################################### 
  
  ###### ------ Raster to data frame  ------  ###### 
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
  
  ###### ------ Train/Test split  ------  ###### 
  TrainTestSplit <- function(value_table,type = "Percentage",value = 70){
    
    if(type == "Percentage"){
      if(value > 95){
        msg_box("The percentage value cannot be more than 95 .... \n
           Your process will continue over 95% ...")
        value <- 95
      }else if(value < 5){
        msg_box("The percentage value cannot be less than 5 .... \n
           Your process will continue over 5% ...")
        value <- 5
      }
      
      #selecting the smallest numerical value
      maxDataNumber <- min(table(value_table$train)) * 2
      trainValue <- as.integer(maxDataNumber*value/100) 
      testValue <- maxDataNumber - trainValue
      trainid <- createSets(value_table,value_table$train,trainValue)
      testid <- createSets(value_table,value_table$train,testValue)
      
      traindata <- value_table[trainid,]
      testdata <- value_table[testid,]
      traintest <-list(train = traindata,test = testdata)
      return(traintest)
    }
    else if(type == "Numerical"){
      #selecting the smallest numerical value
      maxDataNumber <- min(table(value_table$train)) * 2
      maxValue <- as.integer(maxDataNumber * 0.95)
      minValue <- as.integer(maxDataNumber * 0.05)
      if(value > maxValue){
        msg_box("The percentage value cannot be more than the highest value.... \n
                Your process will continue from the highest value")
        value <- maxValue
      }else if(value < minValue){
        msg_box("The percentage value cannot be less than the lowest value.... \n
                Your process will continue from the lowest value")
        value <- minValue
      } 
      
      testValue <- maxDataNumber - value
      trainid <- createSets(value_table,value_table$train,value)
      testid <- createSets(value_table,value_table$train,testValue)
        
      traindata <- value_table[trainid,]
      testdata <- value_table[testid,]
      traintest <-list(train = traindata,test = testdata)
      return(traintest)
      
    }
    else cat("You must type 'numerical' or 'percentage' as type .... \ n
            if you do not, train test data set will be created according to 70%")
    
  }
  
  ###### ------ Create random number set  ------  ###### 
  createSets <- function(x, y, p){
    nr <- NROW(x)
    size <- (p) %/% length(unique(y))
    idx <- lapply(split(seq_len(nr), y), function(.x) sample(.x, size))
    unlist(idx)
    
  }
  
  ###### ------ Raster normalization  ------  ###### 
  normalizationraster <- function(r){
    
    r.min = cellStats(r, "min")
    r.max = cellStats(r, "max")
    
    r.normal <- ((r - r.min) / (r.max - r.min) )
    return(r.normal)
  }
  
  upper.diag<-function(x){
    m<-(-1+sqrt(1+8*length(x)))/2
    X<-lower.tri(matrix(NA,m,m),diag=TRUE)
    X[X==TRUE]<-x
    X[X==FALSE]<-NA
    t(X)
  }
  
  findOptiFea <- function(resultMatrix){
    nX <- ncol(resultMatrix)
    listOfTrue <- lapply(1:nX, function(x) NULL)
    for(i in 1:nX){
      correntValueCount = 0
      if((i+1) <= nX){
        for(j in (i+1):nX){
          if(resultMatrix[i,j] < 0.05 & !is.nan(resultMatrix[i,j]) & !is.na(resultMatrix[i,j])) {
            correntValueCount = correntValueCount + 1
          }
        }
        listOfTrue[[i]] <- correntValueCount
      }
      else{
        listOfTrue[[nX]] <- nX
      }
    }
    #Finding the smallest value and getting the result
    minimum <- min(unlist(listOfTrue))
    minimumValue <- nX + 100
    for(i in 1:( nX- 1)){
      if(listOfTrue[[i]] == minimum){
        minimumValue <- i
        break
      }
    }
    return(minimumValue)
  }
  
  ##################################################################################################### 
  ### Define input/output parameters
  #####################################################################################################  

  arc.progress_label("Reading Data...")
  arc.progress_pos(20)
  
  rasterPath <- in_params[[1]]
  csvPath <- in_params[[2]]
  trainPath <- in_params[[3]]
  algoritm <- in_params[[4]]
  resultName <- in_params[[5]]
  featureFoldNumber <- as.integer(in_params[[6]])
  modelFoldNumber <- in_params[[7]]
  type <- "Percentage"
  value <- as.integer(in_params[[8]])
  excelPath <- out_params[[1]]
  stackPath <- in_params[[9]]

  ##################################################################################################### 
  ### Load data
  #####################################################################################################
  
  #Read Raster Stack
  rasters1 <- brick(rasterPath)
  
  #Reading factor names file
  if(length(csvPath)){
    stackNames<-read.csv(csvPath)
    if(nlayers(rasters1) == nrow(stackNames)){
      stackNames <- as.character(stackNames[[2]])
      names(rasters1) <- stackNames
    }else{
      msg_box("Factor names will be organized as Band1, Band2...")
      featureName <- unlist(lapply(1:nlayers(rasters1),function(x) paste0("Band",x)))
      names(rasters1) <- featureName
    }
  }else{
    nameRas <- lapply(1:nlayers(rasters1), function(x)  paste0("Band",x))
    namesRas <- unlist(nameRas)
    names(rasters1) <- namesRas
  }
  
  #Read Train
  train <- raster(trainPath)
  
  ##################################################################################################### 
  ### Create training and testing dataset
  #####################################################################################################
  
  arc.progress_label("Preparing Data Set...")
  arc.progress_pos(40)
  
  #raster to dataframe
  valueDF <- FeatureData(rasters1,train)
  
  #Train/Test split
  listoftrain <- lapply(1:featureFoldNumber, function(x) NULL)
  listoftest <- lapply(1:featureFoldNumber, function(x) NULL)
  for(i in 1:featureFoldNumber){
    traintest <- TrainTestSplit(valueDF,type,value)
    listoftrain[[i]] <- traintest$train
    listoftest[[i]] <- traintest$test
  }
  
  arc.progress_label("Select Optimum Feature Sort...")
  arc.progress_pos(40)
  
  ###### ------ Feature Weight  ------  ###### 
  listofweights <- lapply(1:featureFoldNumber, function(x) NULL)
  arc.progress_pos(0)
  if(algoritm == "Chi Square"){
    for(i in 1:featureFoldNumber){
      arc.progress_pos(as.integer(i * (100/featureFoldNumber)))
      listofweights[[i]] <- chi.squared(train ~. , listoftrain[[i]])
    }
  }else if(algoritm == "Information Gain"){
    for(i in 1:featureFoldNumber){
      arc.progress_pos(as.integer(i * (100/featureFoldNumber)))
      listofweights[[i]] <- information.gain(train ~. , listoftrain[[i]])
    }
  }else if(algoritm == "Spearmans Rank Correlation Coefficient"){
    for(i in 1:featureFoldNumber){
      arc.progress_pos(as.integer(i * (100/featureFoldNumber)))
      listofweights[[i]] <- rank.correlation(train ~. , listoftrain[[i]])
    }
  }else if(algoritm == "Random Forest Importance"){
    for(i in 1:featureFoldNumber){
      arc.progress_pos(as.integer(i * (100/featureFoldNumber)))
      listofweights[[i]] <- random.forest.importance(train ~. , listoftrain[[i]])
    }
  }else cat("Please Select an Algoritm in The List...")
  
  ###### ------ Sort features by Importance  ------  ###### 
  listofsubset <- lapply(1:featureFoldNumber, function(x) NULL)
  for(i in 1:featureFoldNumber){
    listofsubset[[i]] <- cutoff.k(listofweights[[i]], nrow(listofweights[[i]]))
    #listofsubset[[i]] <- append(listofsubset[[i]], "train")
  }
  
  ###### ------ Find most repeat Feature list  ------  ###### 
  df <- data.frame(listofsubset)
  colnames(df) <- c(1:length(df))
  df <- t(df)
  df <- as.data.frame(df)
  rownames(df) <- c(1:featureFoldNumber)
  
  output <- data.frame(sample = NA, duplicate = NA, matches = NA)
  dfrow <- 1
  for(i in 1:nrow(df)) {
    sample <- df[i, ]
    for(j in (i+1):nrow(df)) if(i+1 <= nrow(df)) {
      matches <- 0
      for(V in 1:ncol(df)) {
        if(df[j,V] == sample[,V]) {       
          matches <- matches + 1
        }
      }
      if(matches > 3) {
        duplicate <- df[j, ]
        pair <- cbind(rownames(sample), rownames(duplicate), matches)
        output[dfrow, ] <- pair
        dfrow <- dfrow + 1
      }
    }
  }
  
  optimum <- which.max(output$matches)
  optimumnumber <- as.numeric(output[optimum,1])
  optiResult <- data.frame("FeatureNames" = listofsubset[[optimumnumber]],"Importance" = sort(listofweights[[optimumnumber]]$attr_importance,decreasing = T))
  optimumsort <- c(listofsubset[[optimumnumber]],"train")
  optimumTrain <- listoftrain[[optimumnumber]][optimumsort]
  optimumTest <- listoftest[[optimumnumber]][optimumsort]
  nX <- length(optimumTrain) - 2 
  listofdf <- lapply(1:nX, function(x) NULL)
  
  for(j in 1:nX){
    listofdf[[j]] <- data.frame(optimumTrain[1:(j + 1)])
    listofdf[[j]]$train <- optimumTrain$train 
  }
  
  #####################################################################################################
  ### Fit model
  #####################################################################################################
  
  arc.progress_label("Fit Models...")
  arc.progress_pos(60)
  
  listofPredict <- lapply(1:nX, function(x) NULL)
  listofModel <- lapply(1:nX, function(x) NULL)
 
  #define training control
  train_control <- trainControl(method="cv", number=modelFoldNumber)
  
  arc.progress_pos(0)
    for(j in 1:nX){
      arc.progress_pos(as.integer(j * (100/nX)))
      listofModel[[j]] <-  train(train ~., data=listofdf[[j]], trControl=train_control, method="glm",family=binomial(link='logit'))
      listofPredict[[j]] <- predict(listofModel[[j]],optimumTest)
    }
  

  #####################################################################################################
  ### Analyze the predicted model
  #####################################################################################################
  
  arc.progress_label("Analize Models by Binary Classifier...")
  arc.progress_pos(80)
  
  predict <- as.data.frame(matrix(unlist(listofPredict), nrow=length(unlist(listofPredict[1]))))

  if(resultName == "Wilcoxon"){
    arc.progress_label("Analize Models by Wilcoxon Test")
    resultNumeric <- matrix(nrow = length(listofModel), ncol = length(listofModel))
    resultTF <- matrix(nrow = length(listofModel), ncol = length(listofModel))
    for(j in 1:(nX-1)){
      for(k in (j+1):nX){
        resultNumeric[j,k] <- round(wilcox.test(predict[,j], predict[,k], paired = TRUE)$p.value, digits = 5)
        resultTF[j,k] <- ifelse(resultNumeric[j,k] < 0.05,"Sig","Insig")
      }
    }
  }else if( resultName == "Variance"){
    arc.progress_label("Analize Models by Variance Test")
    resultNumeric <- matrix(nrow = length(listofModel), ncol = length(listofModel))
    resultTF <- matrix(nrow = length(listofModel), ncol = length(listofModel))
    for(j in 1:(nX-1)){
      for(k in (j+1):nX){
        resultNumeric[j,k] <- round(var.test(predict[,j], predict[,k])$p.value, digits = 5)
        resultTF[j,k] <- ifelse(resultNumeric[j,k] < 0.05,"Sig","Insig")
      }
    }
  }else if( resultName == "Kolmogorov-Smirnov"){
    arc.progress_label("Analize Models by Kolmogorov-Smirnov Test")
    resultNumeric <- matrix(nrow = length(listofModel), ncol = length(listofModel))
    resultTF <- matrix(nrow = length(listofModel), ncol = length(listofModel))
    for(j in 1:(nX-1)){
      for(k in (j+1):nX){
        resultNumeric[j,k] <- round(ks.test(predict[,j], predict[,k])$p.value, digits = 5)
        resultTF[j,k] <- ifelse(resultNumeric[j,k] < 0.05,"Sig","Insig")
      }
    }
  }else if( resultName == "OneS-T-Test"){
    arc.progress_label("Analize Models by OneS-T Test")
    results1 <- resamples((as.list(listofModel)))
    diffs <- diff(results1)
    #summarize p-values for pair-wise comparisons
    sumdiff <- summary(diffs)
    diffRMSE <- t(sumdiff$table$RMSE)
    RMSEpvalue <- diffs$statistics$RMSE
    resultsRMSE <- RMSEpvalue
    for(i in 1:length(RMSEpvalue)){
      
      resultsRMSE[[i]] <- RMSEpvalue[[i]]$p.value
      
    }
    resultsRMSE <- unlist(resultsRMSE)
    resultsRMSE <- upper.diag(resultsRMSE)
    resultNumeric <- resultsRMSE
    resultTF <- resultsRMSE
    
    resultTF <- ifelse(resultNumeric < 0.05,"Sig","Insig")
   
  }else if( resultName == "ALL"){
    arc.progress_label("Analize Models by  Wilcoxon, OneS-T, Variance, Kolmogorov-Smirnov Tests")
    
    ###### ------ One Sample T-Test REsults  ------  ######
    results1 <- resamples((as.list(listofModel)))
    
    diffs <- diff(results1)
    #summarize p-values for pair-wise comparisons
    sumdiff <- summary(diffs)
    diffRMSE <- t(sumdiff$table$RMSE)
    RMSEpvalue <- diffs$statistics$RMSE
    resultsRMSE <- RMSEpvalue
    for(i in 1:length(RMSEpvalue)){
      
      resultsRMSE[[i]] <- RMSEpvalue[[i]]$p.value
      
    }
    resultsRMSE <- unlist(resultsRMSE)
    resultsRMSE <- upper.diag(resultsRMSE)
    resultNumeric <- resultsRMSE
    resultOSTNumeric <- resultsRMSE
    
    resultOSTTF <- ifelse(resultOSTNumeric < 0.05,"Sig","Insig")
    
    resultWLC <- matrix(nrow = length(listofModel), ncol = length(listofModel))
    resultWLCTF <- matrix(nrow = length(listofModel), ncol = length(listofModel))
    resultRMSE <- matrix(nrow = length(listofModel), ncol = length(listofModel))
    resultRMSETF <- matrix(nrow = length(listofModel), ncol = length(listofModel))
    resultVAR <- matrix(nrow = length(listofModel), ncol = length(listofModel))
    resultVARTF <- matrix(nrow = length(listofModel), ncol = length(listofModel))
    resultKs<- matrix(nrow = length(listofModel), ncol = length(listofModel))
    resultKsTF<- matrix(nrow = length(listofModel), ncol = length(listofModel))
    
    for(j in 1:nX){
      for(k in j:nX){
        resultWLC[j,k] <- round(wilcox.test(predict[,j], predict[,k], paired = TRUE)$p.value, digits = 5)
        resultWLCTF[j,k] <- ifelse(resultWLC[j,k] < 0.05,"Sig","Insig")
        resultVAR[j,k] <- round(var.test(predict[,j], predict[,k])$p.value, digits = 5)
        resultVARTF[j,k] <- ifelse(resultVAR[j,k] < 0.05,"Sig","Insig")
        resultKs[j,k] <- round(ks.test(predict[,j], predict[,k])$p.value, digits = 5)
        resultKsTF[j,k] <- ifelse(resultKs[j,k] < 0.05,"Sig","Insig")
        
      }
    }
  }else{ print("Please Select a Significant Test in The List")
    return(out_params)}
  
  #####################################################################################################
  ###  List the contents of models
  #####################################################################################################
  
  arc.progress_label("List the contents of models")
  
  modelNames <- lapply(1:length(listofdf), function(x) NULL)
  nameList <- list()
 
  for(i in 1:length(listofdf)){
    modelNames[[i]] <- colnames(listofdf[[i]][-ncol(listofdf[[i]])])
    nameList <- append(nameList,paste0("Model",i))
    
  }
  
  nameList <- unlist(nameList)
  n.obs <- sapply(modelNames, length)
  seq.max <- seq_len(max(n.obs))
  modelNames <- t(sapply(modelNames, "[", i = seq.max))
  rownames(modelNames) <- nameList
  modelNames <- ifelse(is.na(modelNames),"---",modelNames)
  
  #####################################################################################################
  ### Write feature selection results
  #####################################################################################################
  
  arc.progress_label("Write statistical results...")
  arc.progress_pos(90)
  
  if(resultName != "ALL"){
    minimumValue <- findOptiFea(resultNumeric)
    optimumFeatureNames <- names(optimumTrain[1:(minimumValue + 1)])
    newRasterFS <- subset(rasters1,optimumFeatureNames)
    newRasterFS <- resample(newRasterFS,rasters1, resample='bilinear')
    stackNamesPath <- paste0(stackPath,"\\",resultName,".csv")
    stackRasterPath <- paste0(stackPath,"\\",resultName,".tif")
    if(length(csvPath)){
      writeRaster(newRasterFS, filename = stackRasterPath,overwrite = T)
      write.csv(names(newRasterFS),stackNamesPath)
    }else{
      writeRaster(newRasterFS, filename = stackRasterPath,overwrite = T)
    }
    
  }else{
    listofResult <- list(resultWLC,resultVAR,resultKs,resultOSTNumeric)
    listofNames <- list("Wilcoxon","Variance","Kolmogorov-Smirnov","OneS-T-Test")
    for(i in 1:length(listofResult)){
      minimumValue <- findOptiFea(listofResult[[i]])
      optimumFeatureNames <- names(optimumTrain[1:(minimumValue + 1)])
      newRasterFS<- subset(rasters1,optimumFeatureNames)
      stackPath1 <- paste0(stackPath,"\\",listofNames[[i]],"Stack.tif")
      stackNamesPath <- paste0(stackPath,"\\",listofNames[[i]],"_FNames.csv")
      if(length(csvPath)){
        writeRaster(newRasterFS,filename = stackPath1)
        write.csv(names(newRasterFS),stackNamesPath)
      }else{
        arc.write(data = newRasterFS, path = stackPath1
                  ,overwrite=TRUE)
      }
    }
  }
  
  if(resultName == "ALL"){
    write.xlsx(resultWLC,file = excelPath,col.names = T, row.names = T, sheetName = "WilcoxonNumeric",append = F)
    write.xlsx(resultWLCTF,file = excelPath,col.names = T, row.names = T, sheetName = "WilcoxonTF",append = T)
    write.xlsx(resultVAR,file = excelPath,col.names = T, row.names = T, sheetName = "VarianceNumeric",append = T)
    write.xlsx(resultVARTF,file = excelPath,col.names = T, row.names = T, sheetName = "VarianceTF",append = T)
    write.xlsx(resultOSTNumeric,file = excelPath,col.names = T, row.names = T, sheetName = "OneS-T-TestNumeric",append = T)
    write.xlsx(resultOSTTF,file = excelPath,col.names = T, row.names = T, sheetName = "OneS-T-TestTF",append = T)
    write.xlsx(resultKs,file = excelPath,col.names = T, row.names = T, sheetName = "Kolmogorov-SmirnovNumeric",append = T)
    write.xlsx(resultKsTF,file = excelPath,col.names = T, row.names = T, sheetName = "Kolmogorov-SmirnovTF",append = T)
    write.xlsx(modelNames,file = excelPath,col.names = T, row.names = T, sheetName = "Model_List",append = T)
    write.xlsx(optiResult,file = excelPath,col.names = T, row.names = T, sheetName = "Feature_Importance",append = T)
    
    
  }else{
    sheetname1 <- paste0(resultName,"Numeric")
    sheetname2 <- paste0(resultName,"TF")
    write.xlsx(resultNumeric,file = excelPath,col.names = T, row.names = T, sheetName = sheetname1,append = F)
    write.xlsx(resultTF,file = excelPath,col.names = T, row.names = T, sheetName = sheetname2,append = T)
    write.xlsx(modelNames,file = excelPath,col.names = T, row.names = T, sheetName = "Model_List",append = T)
    write.xlsx(optiResult,file = excelPath,col.names = T, row.names = T, sheetName = "Feature_Importance",append = T)
  }

  arc.progress_pos(100)
  return(out_params)

}

