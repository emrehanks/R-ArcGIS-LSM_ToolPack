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
### LSM generation using Logistic Regression algorithm
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
  if (!requireNamespace("pROC", quietly = TRUE))
    install.packages("pROC")
  if (!requireNamespace("grDevices", quietly = TRUE))
    install.packages("grDevices")
  if (!requireNamespace("svDialogs", quietly = TRUE))
    install.packages("svDialogs")
  
  require(rgdal)
  require(raster)
  require(sp)
  require(pROC)
  require(grDevices)
  require(svDialogs)
    
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
      
  #train test split 
  TrainTestSplit <- function(value_table,type = "percantage",value = 70){
    
    if(type == "percantage"){
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
      maxverisayisi <- min(table(value_table$train)) * 2
      trainsayisi <- as.integer(maxverisayisi*value/100) 
      testsayisi <- maxverisayisi - trainsayisi
      trainid <- createSets(value_table,value_table$train,trainsayisi)
      testid <- createSets(value_table,value_table$train,testsayisi)
      
      traindata <- value_table[trainid,]
      testdata <- value_table[testid,]
      traintest <-list(train = traindata,test = testdata)
      return(traintest)
      
      
    }
    else if(type == "numerical"){
      #selecting the smallest numerical value
      maxverisayisi <- min(table(value_table$train)) * 2
      enfazladeger <- as.integer(maxverisayisi * 0.95)
      enazdeger <- as.integer(maxverisayisi * 0.05)
      if(value > enfazladeger){
        msg_box("The percentage value cannot be more than the highest value.... \n
                Your process will continue from the highest value")
        value <- enfazladeger
      }else if(value < enazdeger){
        msg_box("The percentage value cannot be less than the lowest value.... \n
                Your process will continue from the lowest value")
        value <- enazdeger
      } 
      
      testsayisi <- maxverisayisi - value
      trainid <- createSets(value_table,value_table$train,value)
      testid <- createSets(value_table,value_table$train,testsayisi)
      
      traindata <- value_table[trainid,]
      testdata <- value_table[testid,]
      traintest <-list(train = traindata,test = testdata)
      return(traintest)
      
    }
    else cat("You must type 'numerical' or 'percentage' as type .... \ n
            if you do not, train test data set will be created according to 70%")
    
  }
    
  #create random number set
  createSets <- function(x, y, p){
    nr <- NROW(x)
    size <- (p) %/% length(unique(y))
    idx <- lapply(split(seq_len(nr), y), function(.x) sample(.x, size))
    unlist(idx)
    
  }
    
  #---- raster Normalization ------
  normalizationraster <- function(r){
    
    r.min = cellStats(r, "min")
    r.max = cellStats(r, "max")
    
    r.normal <- ((r - r.min) / (r.max - r.min) )
    return(r.normal)
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
  trainPath <- in_params[[5]]
  roctf <- out_params[[1]]
  sumPath <- out_params[[2]]
  kayitPath <- out_params[[3]]
  
  ##################################################################################################### 
  ### Load Data
  #####################################################################################################
  
  #Read Raster Stack Data
  # rasters1 <-  arc.raster(arc.open(rasterPath))
  # rasters1 <-  arc.data2sp(rasters1)
  rasters1 <- brick(rasterPath)
  
  #If have Feature name file, read this file
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
  # train <- arc.raster(arc.open(trainPath))
  # train <- arc.data2sp(train)
  train <- raster(trainPath)
  
  ##################################################################################################### 
  ### Create training and testing dataset
  #####################################################################################################
  
  arc.progress_label("Preparing Data Set...")
  arc.progress_pos(40)
  
  #Merge Raster stack and Train data and Turn Data frame format
  valueDF <- FeatureData(rasters1,train)
  
  #train test split
  trainTestDf <- TrainTestSplit(value_table = valueDF,type = type, value = value)
  #defination train and test data
  traindata <- trainTestDf$train
  testdata <- trainTestDf$test
  
  #####################################################################################################
  ### Fit model
  #####################################################################################################
  
  arc.progress_label("Fit Model...")
  arc.progress_pos(60)
  
  lrfit <- glm(train ~. , data = traindata, family = binomial(link = "logit") )
  sumLr <- summary(lrfit)
  
  #####################################################################################################
  ### Predict model
  #####################################################################################################
  
  arc.progress_label("Predict Model")
  arc.progress_pos(60)
  
  #Predict Raster data
  lrRasterPredict <- predict(rasters1, lrfit, na.rm = T)
  #normalization predict data
  lrNormalRasterPredict <- normalizationraster(lrRasterPredict)

  #####################################################################################################
  ### Write LSM results
  #####################################################################################################
  
  arc.progress_label("Write Out the Results...")
  arc.progress_pos(90)
    
  if(length(roctf)){
    #predict Test data
      lrTestPredict <- predict(lrfit, testdata,type = "response")
      #Create ROC with test data
      lrRoc <- roc(response = testdata$train,predictor = lrTestPredict, plot=FALSE,legacy.axes = TRUE,percent = TRUE)
      auc <- round(lrRoc$auc,digit = 4)
      legendname <- paste0("Log Reg ","AUC : ",auc)
      tiff(roctf, width = 1920, height = 1080, res = 200)
      par(pty = "s")
      plot(lrRoc)
      legend("bottomright",legendname,cex = 1,lwd = 1:2)
      dev.off()
  }
  
  #Write out statistical Result
  if(length(sumPath)){
    sink(sumPath); print(sumLr); sink()
  }
  
  #write out LSM
  arc.write(data = lrNormalRasterPredict, path = if(grepl("\\.tif$", kayitPath)| grepl("\\.img$", kayitPath)) kayitPath
            else paste0(normalizePath(dirname(kayitPath)),"\\", sub('\\..*$', '', basename(kayitPath)),".tif")
            ,overwrite=TRUE)
  
  arc.progress_pos(100)
  return(out_params)
  
}
