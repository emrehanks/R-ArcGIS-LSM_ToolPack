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
### LSM generation using Random Forest algorithm
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
  if (!requireNamespace("randomForest", quietly = TRUE))
    install.packages("randomForest")
  if (!requireNamespace("pROC", quietly = TRUE))
    install.packages("pROC")
  if (!requireNamespace("grDevices", quietly = TRUE))
    install.packages("grDevices")
  if (!requireNamespace("svDialogs", quietly = TRUE))
    install.packages("svDialogs")
  if (!requireNamespace("xlsx", quietly = TRUE))
    install.packages("xlsx")
  
  require(rgdal)
  require(raster)
  require(sp)
  require(randomForest)
  require(pROC)
  require(grDevices)
  require(svDialogs)
  require(xlsx)
 
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
  TrainTestSplit <- function(value_table,type = "percantage",value = 70){
    
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
  
  ###### ------ Drawing the tree  ------  ###### 
  to.dendrogram <- function(dfrep,rownum=1,height.increment=0.1){
    
    if(dfrep[rownum,'status'] == -1){
      rval <- list()
      
      attr(rval,"members") <- 1
      attr(rval,"height") <- 0.0
      attr(rval,"label") <- dfrep[rownum,'prediction']
      attr(rval,"leaf") <- TRUE
      
    }else{##note the change "to.dendrogram" and not "to.dendogram"
      left <- to.dendrogram(dfrep,dfrep[rownum,'left daughter'],height.increment)
      right <- to.dendrogram(dfrep,dfrep[rownum,'right daughter'],height.increment)
      rval <- list(left,right)
      
      attr(rval,"members") <- attr(left,"members") + attr(right,"members")
      attr(rval,"height") <- max(attr(left,"height"),attr(right,"height")) + height.increment
      attr(rval,"leaf") <- FALSE
      attr(rval,"edgetext") <- paste(dfrep[rownum,'split var'],"\n<",round(dfrep[rownum,'split point'], digits = 2),"=>", sep = " ")
    }

    class(rval) <- "dendrogram"
    return(rval)
  }
  
  ##################################################################################################### 
  ### Define input/output parameters 
  #####################################################################################################  
  
  arc.progress_label("Reading Data...")
  arc.progress_pos(20)
  
  rasterPath <- in_params[[1]]
  csvPath <- in_params[[2]]
  type <- as.character(in_params[[3]])
  value <- as.integer(in_params[[4]])
  subsetN <- as.integer(in_params[[5]])
  treeN <- as.integer(in_params[[6]])
  nodesize <- as.integer(in_params[[7]])
  trainPath <- in_params[[8]]
  roctf <- out_params[[1]]
  treePath <- out_params[[2]]
  featPath <- out_params[[3]]
  LSMPath <- out_params[[4]]
  
  ##################################################################################################### 
  ### Load data
  #####################################################################################################
  
  #Read raster stack data
  rasters1 <- brick(rasterPath)
  
  #Read factor names file
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
  
  #Read train raster data
  train <- raster(trainPath)
  
  ##################################################################################################### 
  ### Create training and testing dataset
  #####################################################################################################
  
  arc.progress_label("Preparing Data Set...")
  arc.progress_pos(40)
  
  #Merging raster stack data and train data and after turn to DataFrame 
  valueDF <- FeatureData(rasters1,train)
  trainTestDf <- TrainTestSplit(value_table = valueDF,type = type, value = value)
  #Defination train and test data
  traindata <- trainTestDf$train
  testdata <- trainTestDf$test
  
  #####################################################################################################
  ### Fit Model
  #####################################################################################################
  
  arc.progress_label("Fit Model...")
  arc.progress_pos(60)
  
  rfFit <- randomForest(formula = train ~ ., data = traindata, ntree = treeN, mtry = subsetN, nodesize = nodesize)
  feaImp <- importance(rfFit)
  
  #####################################################################################################
  ### Predict Model
  #####################################################################################################
  
  arc.progress_label("Predict Model")
  arc.progress_pos(60)
  
  #Predict Raster data
  rfRasterPredict <- predict(rasters1, rfFit, na.rm = T)
  #Normalization predict data
  rfNormalRasterPredict <- normalizationraster(rfRasterPredict)
  
  #####################################################################################################
  ### Write LSM results
  #####################################################################################################
  
  arc.progress_label("Write the Results...")
  arc.progress_pos(90)
  
  if(length(roctf)){
      #predict Test data
      rfTestPredict <- predict(rfFit, testdata,type = "response")
      #Create ROC with test data
      rfRoc <- roc(response = testdata$train,predictor = rfTestPredict, plot=FALSE,legacy.axes = TRUE,percent = TRUE)
      auc <- round(rfRoc$auc,digit = 4)
      legendname <- paste0("Random Forest ","AUC : ",auc)
      
      tiff(roctf, width = 1920, height = 1080, res = 200)
      par(pty = "s")
      plot(rfRoc)
      legend("bottomright",legendname,cex = 1,lwd = 1:2)
      dev.off()
  }
  
  #Write Out Random Forest tree 
  if(length(treePath)){
    trees <- getTree(rfFit,labelVar = T)
    d <- to.dendrogram(trees)
    tiff(treePath, width = 5120, height = 2880, res = 200)
    plot(d,center=TRUE,edgePar=list(t.cex=.55,p.col=NA,p.lty=0), yaxt = "n")
    dev.off()
  }
  
  #Write out Feature Importance
  if(length(featPath)){
    write.xlsx(feaImp,file = featPath,col.names = T, row.names = T)
  }
  
  #Write Out LSM 
  arc.write(data = rfNormalRasterPredict, path = if(grepl("\\.tif$", LSMPath)| grepl("\\.img$", LSMPath)) LSMPath
            else paste0(normalizePath(dirname(LSMPath)),"\\", sub('\\..*$', '', basename(LSMPath)),".tif")
            ,overwrite=TRUE)
  
  arc.progress_pos(100)
  return(out_params)
  
}