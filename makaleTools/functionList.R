
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

TrainTestSplit <- function(value_table,type = "percantage",value = 70){
  
  if(type == "percantage"){
    if(value > 95){
      msg_box("The percentage value cannot be more than 95 .... \ n
           Your process will continue over 95% ...")
      value = 95
    }
    else if(value < 5){
      msg_box("The percentage value cannot be less than 5 .... \ n
           Your process will continue over 5% ...")
      value =5
    }
    
    #veri setindeki 0 ve 1 lere bakarak en az olana göre ayarlanmasi
    maxverisayisi <- min(table(value_table$train)) * 2
    #percentvalue kadarinin train gerisinin test verisi olarak ayarlar ve idlerini tutar
    trainsayisi <- as.integer(maxverisayisi*value/100) 
    testsayisi <- maxverisayisi - trainsayisi
    trainid <- createSets(value_table,value_table$train,trainsayisi)
    testid <- createSets(value_table,value_table$train,testsayisi)
    
    #tutulan idler üzerinden train ve test verisini value_table dan ceker
    traindata <- value_table[trainid,]
    testdata <- value_table[testid,]
    #train ve test verisini disa aktarmak icin list'in icine atanir
    traintest <-list(train = traindata,test = testdata)
    return(traintest)
    
    
  }
  else if(type == "numerical"){
    #veri setindeki 0 ve 1 lere bakarak en az olana göre ayarlanmasi
    maxverisayisi <- min(table(value_table$train)) * 2
    enfazladeger <- as.integer(maxverisayisi * 0.95)
    if(value > enfazladeger) cat("The value you entered is greater than the number of data that can be created \ n Maximum:",enfazladeger)
    
    else{
      
      testsayisi <- maxverisayisi - value
      trainid <- createSets(value_table,value_table$train,value)
      testid <- createSets(value_table,value_table$train,testsayisi)
      
      #tutulan idler üzerinden train ve test verisini value_table dan ceker
      traindata <- value_table[trainid,]
      testdata <- value_table[testid,]
      #train ve test verisini disa aktarmak icin list'in icine atanir
      traintest <-list(train = traindata,test = testdata)
      return(traintest)
    }
    
  }
  else cat("You must type 'numerical' or 'percentage' as type .... \ n
            if you do not, train test data set will be created according to 70%")
  
}

createSets <- function(x, y, p){
  nr <- NROW(x)
  size <- (p) %/% length(unique(y))
  idx <- lapply(split(seq_len(nr), y), function(.x) sample(.x, size))
  unlist(idx)
  
}