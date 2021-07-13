#####################################################################################################  
### By: Emrehan Kutlug SAHIN ----- emrehansahin@ibu.edu.tr ; emrehans@gmail.com
#####################################################################################################  
# Please mention and cite this article in your research
#####################################################################################################
#########################
###  Emrehan Kutlug Sahin, Ismail Colkesen, Suheda Semih Acmali, Aykut Akgun, Arif Cagdas Aydinoglu,
# Developing comprehensive geocomputation tools for landslide susceptibility mapping: LSM tool pack,
# Computers & Geosciences,
# Volume 144,
# 2020,
# 104592,
# ISSN 0098-3004,
# https://doi.org/10.1016/j.cageo.2020.104592.
# (https://www.sciencedirect.com/science/article/pii/S009830042030577X)
### Producing landslide susceptibility maps by applying Support Vector Machine
###
##################################################################################################### 

##################################################################################################### 

tool_exec <- function(in_params, out_params)
{
  #####################################################################################################  
  ### Check/Load Required Packages  ####  
  #####################################################################################################   
  options(repos="https://CRAN.R-project.org")
  set.seed(24)
  round(memory.limit()/2^20, 2)
  library(arcgisbinding)
  arc.check_product()
  arc.progress_label("Loading Libraries...")
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
  if (!requireNamespace("e1071", quietly = TRUE))
    install.packages("e1071")
  if (!requireNamespace("svDialogs", quietly = TRUE))
    install.packages("svDialogs")
  if (!requireNamespace("xlsx", quietly = TRUE))
    install.packages("xlsx")

  require(rgdal)
  require(raster)
  require(sp)
  require(pROC)
  require(grDevices)
  require(e1071)
  require(svDialogs)
  require(xlsx)

  ##################################################################################################### 
  ### Define input/output parameters 
  #####################################################################################################  
  arc.progress_label("Data Reading...")
  arc.progress_pos(20)
  rasterPath <- in_params[[1]]
  csvPath <- in_params[[2]]
  type <- as.character(in_params[[3]])
  value <- as.integer(in_params[[4]])
  kernelName <- tolower(as.character(in_params[[5]]))
  degree <- as.integer(in_params[[6]])
  gamma <- as.double(in_params[[7]])
  coef0 <- as.integer(in_params[[8]])
  nu <- as.double(in_params[[9]])
  trainPath <- in_params[[10]]
  roctf <- out_params[[1]]
  kayitPath <- out_params[[2]]
  
  ###################################################################################################### 
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
    
    if(type == "Sample Ratio"){
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
    else if(type == "Sample Number"){
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
  
  #Merging raster stack data and train data and after turn to DataFrame 
  valueDF <- FeatureData(rasters1,train)
  trainTestDf <- TrainTestSplit(value_table = valueDF,type = type, value = value)
  
  #Defination train and test data
  traindata <- trainTestDf$train
  testdata <- trainTestDf$test
  
  #####################################################################################################
  ### Fit Model ### 
  #####################################################################################################
  arc.progress_label("Building Model...")
  arc.progress_pos(60)
  #Support Vector Machine Modelli oluþturuyor
  svmFit <- svm(train ~., data = traindata, kernel = kernelName, type = 'eps-regression', degree = degree, gamma = gamma, coef0 = coef0, nu = nu)
  #####################################################################################################
  ### Predict Model ### M
  #####################################################################################################
  arc.progress_label("Predicting Model...")
  arc.progress_pos(60)
  #raster verinin eðitilen veri seti ile predict edilmesi
  svmRasterPredict <- predict(rasters1, svmFit, na.rm = T)
  #raster verinin normalizasyonu
  svmNormalRasterPredict <- normalizationraster(svmRasterPredict)
  
  
  
  #####################################################################################################
  ### Write Out ### 
  #####################################################################################################
  arc.progress_label("Data Writing...")
  arc.progress_pos(90)
  #Roc eðrisi çizilmesi istenilmesi durumda yapýlacak iþlemler
  if(length(roctf)){
      #Test verisinin predict edilmesi
      svmTestPredict <- predict(svmFit, testdata)
      #Roc egrisinin cizilmesi
      desicionRoc <- roc(response = testdata$train,predictor = svmTestPredict, plot=FALSE,legacy.axes = TRUE,percent = TRUE)
      auc <- round(desicionRoc$auc,digit = 4)
      legendname <- paste0("DVM ","AUC : ",auc)
      
      tiff(roctf, width = 1920, height = 1080, res = 200)
      par(pty = "s")
      plot(desicionRoc)
      legend("bottomright",legendname,cex = 1,lwd = 1:2)
      dev.off()
  }
  arc.write(data = svmNormalRasterPredict, path = if(grepl("\\.tif$", kayitPath)| grepl("\\.img$", kayitPath)) kayitPath
            else paste0(normalizePath(dirname(kayitPath)),"\\", sub('\\..*$', '', basename(kayitPath)),".tif")
            ,overwrite=TRUE)
  arc.progress_pos(100)
  return(out_params)
}
