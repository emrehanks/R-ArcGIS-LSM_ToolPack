#Logistic Regression
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
    #Read the functions on functionList.R
    source(paste0(getwd(),"/functionList.R"))
    
    ##################################################################################################### 
    ### Define input/output parameters #### Girdi/Çýktý Parametrelerinin Tanýmlanmasý
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
    ### Load Data  ####  Verilerin Okunmasý
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
    ### Create Training and Testing Datasets  ####  Eðitim Test Verisinin Oluþturulmasý 
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
    ### Fit Model ### Modelin Eðitilmesi
    #####################################################################################################
    arc.progress_label("Fit Model...")
    arc.progress_pos(60)
    lrfit <- glm(train ~. , data = traindata, family = binomial(link = "logit") )
    sumLr <- summary(lrfit)
    #####################################################################################################
    ### Predict Model ### Model Çýktýlarýnýn Alýnmasý
    #####################################################################################################
    arc.progress_label("Predict Model")
    arc.progress_pos(60)
    #Predict Raster data
    lrRasterPredict <- predict(rasters1, lrfit, na.rm = T)
    #normalization predict data
    lrNormalRasterPredict <- normalizationraster(lrRasterPredict)

    #####################################################################################################
    ### Write Out ### Veriler Yazdýrýlýyor
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
