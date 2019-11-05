
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
      msg_box("The percentage value cannot be more than 95 .... \ n
           Your process will continue over 95% ...")
      value = 95
    }
    else if(value < 5){
      msg_box("The percentage value cannot be less than 5 .... \ n
           Your process will continue over 5% ...")
      value =5
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
    if(value > enfazladeger) cat("The value you entered is greater than the number of data that can be created \ n Maximum:",enfazladeger)
    
    else{
      
      testsayisi <- maxverisayisi - value
      trainid <- createSets(value_table,value_table$train,value)
      testid <- createSets(value_table,value_table$train,testsayisi)
      
      traindata <- value_table[trainid,]
      testdata <- value_table[testid,]
      traintest <-list(train = traindata,test = testdata)
      return(traintest)
    }
    
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