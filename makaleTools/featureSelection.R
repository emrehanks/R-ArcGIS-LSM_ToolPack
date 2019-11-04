#Feature Selection
#####################################################################################################  
### TÜBÝTAK 3501 - KARÝYER GELÝÞTÝRME PROGRAMI TARAFINDAN DESTEKLENMÝÞTÝR
### Proje No: 118Y090
### Proje Adý: "Heyelan Duyarlýlýk Haritalarý Üretimi için R Programlama Dili Yardýmýyla ARCGIS Ara Yüzlerinin Geliþtirilmesi"
### Proje Yürütücüsü: Emrehan Kutluð ÞAHÝN
### Proje Araþtýrma Ekibi: Doç.Dr. Ýsmail Çölkesen
### Proje Danýþma Ekibi: Prof.Dr. Aykut AKGÜN ; Prof.Dr. Arif Çaðdaþ AYDINOÐLU
### Proje Asistaný Ekibi: Þüheda Semih AÇMALI
#####################################################################################################  
###########   KOD DETAYLARI VE EK BÝLGÝLER             ##############
#####################################################################################################
#########################
### Araç Adý:
### Araç Amacý: 
### Araç Ýçeriði: 
### Yararlanýlan Kütüphane isim ve Web sayfalarý: 
##################################################################################################### 
tool_exec <- function(in_params, out_params)
{
  #####################################################################################################  
  ### Check/Load Required Packages  ####  Kütüphanelerin Kontrol Edilmesi/Yüklenmesi
  ##################################################################################################### 
  round(memory.limit()/2^20, 2) #ARCGIS içerisindeki memory datasýný giderebilmek için eklenmiþtir
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
  #yazilan fonksiyonlarin uzantilari
  source("C:/Users/Public/functionList.R")
  
  ##################################################################################################### 
  ### Define input/output parameters #### Girdi/Çýktý Parametrelerinin Tanýmlanmasý
  #####################################################################################################  
  arc.progress_label("Reading Data...")
  arc.progress_pos(20)
  rasterPath <- in_params[[1]]
  csvPath <- in_params[[2]]
  algoritm <- in_params[[3]]
  resultName <- in_params[[4]]
  #Fold Number
  featureFoldNumber <- as.integer(in_params[[5]])
  modelFoldNumber <- in_params[[6]]
  #Train Test Split Type and Value
  type <- as.character(in_params[[7]])
  value <- as.integer(in_params[[8]])
  trainPath <- in_params[[9]]
  kayitName <- out_params[[1]]
  stackPath <- in_params[[10]]

  ##################################################################################################### 
  ### Load Data  ####  Verilerin Okunmasý
  #####################################################################################################
  #Read Raster Stack
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
  ### Check Train Data  ####  Train Datasý Kontrol Ediliyor
  #####################################################################################################
  arc.progress_label("Check Train Data Values...")
  # #Read Raster Stack
  # cevap <- "cevap"
  # cevap <- trainTableKontrol(train)
  # while(cevap != "cevap"){
  #   if(cevap == "yes"){
  #     train <- trainDataControl(train)
  #     cevap <- trainTableKontrol(train)
  #   }else{
  #     return(out_params)
  #   }
  #   
  # }
  # 
  # #CRS kodlarinin kontrol edilmesi;
  # #Uyuþmazlýk veya Boþ deðer olmasý durumunda uyari verir.
  # #Uyarý sonucunda kullanýcý devam etmesini isteyebilir
  # result <- "sonuc"
  # crsCodes <- c(proj4string(rasters1), proj4string(train))
  # result <- CrsCheck(crsCodes)
  # if(result == "cancel") return(out_params)
  # 
  # 
  # #Extentler arasýnda kesiþim noktasýnýn kontrol edilmesi iþlemi
  # #eðer kesiþim noktalarý yoksa bunlar ya parklý koordinat sistemindedirler yada
  # #farklý yerleri göstermektedir. Bu þekilde iþlem yapýlamayacaðýndan
  # #uyarý ekraný çýkartýlmýþtýr
  # arc.progress_label("Check Extent and Resolution...")
  # arc.progress_pos(30)
  # cevap <- "cevap"
  # extents <- c(extent(rasters1),extent(train))
  # resos <- c(list(res(rasters1)),list(res(train)))
  # cevap <- resoCheck(resos)
  # if(cevap == "no") return(out_params)
  # cevap <-extentCheck(extents)
  # if(cevap == "no") return(out_params)
  
  ##################################################################################################### 
  ### Create Training and Testing Datasets  ####  Eðitim Test Verisinin Oluþturulmasý 
  #####################################################################################################
  arc.progress_label("Preparing Data Set...")
  arc.progress_pos(40)
  #Merge Raster stack and Train data and Turn Data frame format
  valueDF <- FeatureData(rasters1,train)
  
  #train test split
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
  #Select an algoritm
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
  }else if(algoritm == "Gain Ratio"){
    for(i in 1:featureFoldNumber){
      arc.progress_pos(as.integer(i * (100/featureFoldNumber)))
      listofweights[[i]] <- gain.ratio(train ~. , listoftrain[[i]])
    }
  }else if(algoritm == "Symmetrical Uncertainty"){
    for(i in 1:featureFoldNumber){
      arc.progress_pos(as.integer(i * (100/featureFoldNumber)))
      listofweights[[i]] <- symmetrical.uncertainty(train ~. , listoftrain[[i]])
    }
  }else if(algoritm == "Pearson’s Correlation"){
    for(i in 1:featureFoldNumber){
      arc.progress_pos(as.integer(i * (100/featureFoldNumber)))
      listofweights[[i]] <- linear.correlation(train ~. , listoftrain[[i]])
    }
  }else if(algoritm == "Spearman’s Rank Correlation Coefficient"){
    for(i in 1:featureFoldNumber){
      arc.progress_pos(as.integer(i * (100/featureFoldNumber)))
      listofweights[[i]] <- rank.correlation(train ~. , listoftrain[[i]])
    }
  }else if(algoritm == "OneR"){
    for(i in 1:featureFoldNumber){
      arc.progress_pos(as.integer(i * (100/featureFoldNumber)))
      listofweights[[i]] <- oneR(train ~. , listoftrain[[i]])
    }
  }else if(algoritm == "Random Forest Importance"){
    for(i in 1:featureFoldNumber){
      arc.progress_pos(as.integer(i * (100/featureFoldNumber)))
      listofweights[[i]] <- random.forest.importance(train ~. , listoftrain[[i]])
    }
  }else cat("Please Select an Algoritm...")
  
  #Sort features by Importance
  listofsubset <- lapply(1:featureFoldNumber, function(x) NULL)
  for(i in 1:featureFoldNumber){
    listofsubset[[i]] <- cutoff.k(listofweights[[i]], nrow(listofweights[[i]]))
    #listofsubset[[i]] <- append(listofsubset[[i]], "train")
  }
  
  ###### ------ find most repeat Feature list  ------  ###### 
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
  ### Fit Model ### Modelin Eðitilmesi
  #####################################################################################################
  arc.progress_label("Fit Models...")
  arc.progress_pos(60)
  #uretilen df'lerden model uretilmesi ve test verisine gore predict edilmesi
  listofPredict <- lapply(1:nX, function(x) NULL)
  listofModel <- lapply(1:nX, function(x) NULL)
  # define training control
  train_control <- trainControl(method="cv", number=modelFoldNumber)
  arc.progress_pos(0)
    for(j in 1:nX){
      arc.progress_pos(as.integer(j * (100/nX)))
      listofModel[[j]] <-  train(train~., data=listofdf[[j]], trControl=train_control, method="glm",family=binomial(link='logit'))
      listofPredict[[j]] <- predict(listofModel[[j]],optimumTest)
    }
  

  #####################################################################################################
  ### Analize  Model ### Modelin Ýncelenmesi
  #####################################################################################################
  arc.progress_label("Analize Models by Binary Classifier...")
  arc.progress_pos(80)
  predict <- as.data.frame(matrix(unlist(listofPredict), nrow=length(unlist(listofPredict[1]))))

  if(resultName == "Wilcoxon"){
    arc.progress_label("Analize Models by Wilcoxon Test")
    resultNumeric <- matrix(nrow = length(listofModel), ncol = length(listofModel))
    resultTF <- matrix(nrow = length(listofModel), ncol = length(listofModel))
    for(j in 1:nX){
      for(k in j:nX){
        resultNumeric[j,k] <- round(wilcox.test(predict[,j], predict[,k], paired = TRUE)$p.value, digits = 5)
        resultTF[j,k] <- ifelse(resultNumeric[j,k] < 0.05,"Sig","Insig")
      }
    }
  }else if( resultName == "Variance"){
    arc.progress_label("Analize Models by Variance Test")
    resultNumeric <- matrix(nrow = length(listofModel), ncol = length(listofModel))
    resultTF <- matrix(nrow = length(listofModel), ncol = length(listofModel))
    for(j in 1:nX){
      for(k in j:nX){
        resultNumeric[j,k] <- round(var.test(predict[,j], predict[,k])$p.value, digits = 5)
        resultTF[j,k] <- ifelse(resultNumeric[j,k] < 0.05,"Sig","Insig")
      }
    }
  }else if( resultName == "Kolmogorov-Smirnov"){
    arc.progress_label("Analize Models by Kolmogorov-Smirnov Test")
    resultNumeric <- matrix(nrow = length(listofModel), ncol = length(listofModel))
    resultTF <- matrix(nrow = length(listofModel), ncol = length(listofModel))
    for(j in 1:nX){
      for(k in j:nX){
        resultNumeric[j,k] <- round(ks.test(predict[,j], predict[,k])$p.value, digits = 5)
        resultTF[j,k] <- ifelse(resultNumeric[j,k] < 0.05,"Sig","Insig")
      }
    }
  }else if( resultName == "OneS-T-Test"){
    arc.progress_label("Analize Models by OneS-T Test")
    results1 <- resamples((as.list(listofModel)))
    
    diffs <- diff(results1)
    # summarize p-values for pair-wise comparisons
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
    #------ One Sample T-Test REsults --------
    results1 <- resamples((as.list(listofModel)))
    
    diffs <- diff(results1)
    # summarize p-values for pair-wise comparisons
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
    #-------- ----------------
    resultWLC <- matrix(nrow = length(listofModel), ncol = length(listofModel))
    resultWLCTF <- matrix(nrow = length(listofModel), ncol = length(listofModel))
    resultRMSE <- matrix(nrow = length(listofModel), ncol = length(listofModel))
    resultRMSETF <- matrix(nrow = length(listofModel), ncol = length(listofModel))
    resultVAR <- matrix(nrow = length(listofModel), ncol = length(listofModel))
    resultVARTF <- matrix(nrow = length(listofModel), ncol = length(listofModel))
    resultKs<- matrix(nrow = length(listofModel), ncol = length(listofModel))
    resultKsTF<- matrix(nrow = length(listofModel), ncol = length(listofModel))
    
    #predict sonuclarinin wilcoxon testi sonuclarin elde edilmesi ve resultWLC e yazdirilmasi
    
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
  }else{ print("Sonuç listesinde olan bir deðer seçiniz...")
    return(out_params)}
  
  #####################################################################################################
  ###  List the contents of models ###  Modellerin içeriklerinin listelenmesi
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
  ### Write Out ### Veriler Yazdýrýlýyor
  #####################################################################################################
  arc.progress_label("Write Out the Results...")
  arc.progress_pos(90)
  #kayit yapilacak dosyanin Path ayarlamasi
  
  if(resultName != "ALL"){
    minimumdeger <- findOptiFea(resultNumeric)
    optimumFeatureNames <- names(optimumTrain[1:(minimumdeger + 1)])
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
      minimumdeger <- findOptiFea(listofResult[[i]])
      optimumFeatureNames <- names(optimumTrain[1:(minimumdeger + 1)])
      newRasterFS<- subset(rasters1,optimumFeatureNames)
      stackPath1 <- paste0(stackPath,"\\",listofNames[[i]],"Stack.tif")
      stackNamesPath <- paste0(stackPath,"\\",listofNames[[i]],"StackNames.csv")
      if(length(csvPath)){
        writeRaster(newRasterFS,filename = stackPath1)
        # arc.write(data = newRasterFS, path = stackPath1
        #           ,overwrite=TRUE)
        write.csv(names(newRasterFS),stackNamesPath)
      }else{
        arc.write(data = newRasterFS, path = stackPath1
                  ,overwrite=TRUE)
      }
    }
  }
  
  if(resultName == "ALL"){
    write.xlsx(resultWLC,file = kayitName,col.names = T, row.names = T, sheetName = "WilcoxonNumeric",append = F)
    write.xlsx(resultWLCTF,file = kayitName,col.names = T, row.names = T, sheetName = "WilcoxonTF",append = T)
    write.xlsx(resultVAR,file = kayitName,col.names = T, row.names = T, sheetName = "VarianceNumeric",append = T)
    write.xlsx(resultVARTF,file = kayitName,col.names = T, row.names = T, sheetName = "VarianceTF",append = T)
    write.xlsx(resultOSTNumeric,file = kayitName,col.names = T, row.names = T, sheetName = "OneS-T-TestNumeric",append = T)
    write.xlsx(resultOSTTF,file = kayitName,col.names = T, row.names = T, sheetName = "OneS-T-TestTF",append = T)
    write.xlsx(resultKs,file = kayitName,col.names = T, row.names = T, sheetName = "Kolmogorov-SmirnovNumeric",append = T)
    write.xlsx(resultKsTF,file = kayitName,col.names = T, row.names = T, sheetName = "Kolmogorov-SmirnovTF",append = T)
    write.xlsx(modelNames,file = kayitName,col.names = T, row.names = T, sheetName = "Model_List",append = T)
    write.xlsx(optiResult,file = kayitName,col.names = T, row.names = T, sheetName = "Feature_Importance",append = T)
    
    
  }else{
    sheetname1 <- paste0(resultName,"Numeric")
    sheetname2 <- paste0(resultName,"TF")
    write.xlsx(resultNumeric,file = kayitName,col.names = T, row.names = T, sheetName = sheetname1,append = F)
    write.xlsx(resultTF,file = kayitName,col.names = T, row.names = T, sheetName = sheetname2,append = T)
    write.xlsx(modelNames,file = kayitName,col.names = T, row.names = T, sheetName = "Model_List",append = T)
    write.xlsx(optiResult,file = kayitName,col.names = T, row.names = T, sheetName = "Feature_Importance",append = T)
  }

  arc.progress_pos(100)
  return(out_params)

}

