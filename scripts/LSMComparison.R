#dosyalarin uzantilari E:/R tutorials/winco/predictModels

#predict edilen HDH lerin Quantil, Natural Break ve Standart Devision Classifier larý kullanilarak 'cok yuksek, yuksek, orta,dusuk,cok dusuk'
#olacak sekilde 5 sýnýfa ayrilmasi
#NOT : bu siniflandirmada KAPPA, Genel dogruluk vb. dogrulama metotlari kullaniminda 4'den buyuk siniflar icin 1 digerleri icin 0 kabulu yapilmistir
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
  #Read the functions on functionList.R
  source(paste0(getwd(),"/functionList.R"))
  
  ##################################################################################################### 
  ### Define input/output parameters #### Girdi/Çýktý Parametrelerinin Tanýmlanmasý
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
  ### Load Data  ####  Verilerin Okunmasý
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
  ### Interval Progress  ####  Veriyi Bölme Ýþlemi
  #####################################################################################################
  arc.progress_label("Dividing Data by Selected Classifier..")
  arc.progress_pos(30)
  if(classifierName == "fisher"){
    #normalizasyon yapilmis veri normalraster a atanmasi
    normalraster <- stack(lapply(1:nlayers(s), function(x){normalizationraster(s[[x]])}))
    #fisher algoritmasýna göre siniflandirma
    classout <-  suppressWarnings({
    stack(lapply(1:nlayers(normalraster), function(x){funclasifier(normalraster[[x]],"fisher")}))
  })
  }else if(classifierName == "quantile"){
    #normalizasyon yapilmis veri normalraster a atanmasi
    normalraster <- stack(lapply(1:nlayers(s), function(x){normalizationraster(s[[x]])}))
    #quantil algoritmasina gore siniflandirma
    classout <- suppressWarnings({
    stack(lapply(1:nlayers(normalraster), function(x){funclasifier(normalraster[[x]],"quantile")}))
  })
  }else if(classifierName == "manual"){
    #normalizasyon yapilmis veri normalraster a atanmasi
    normalraster <- stack(lapply(1:nlayers(s), function(x){normalizationraster(s[[x]])}))
    #manual interval ile siniflandirma %20,%40,%60,%80 olarak
    classout <- stack(lapply(1:nlayers(normalraster), function(x){funmanual(normalraster[[x]])}))
  }else if(classifierName == "equal"){
    #normalizasyon yapilmis veri normalraster a atanmasi
    normalraster <- stack(lapply(1:nlayers(s), function(x){normalizationraster(s[[x]])}))
    #equal algoritmasina gore siniflandirma
    classout <- suppressWarnings({
    stack(lapply(1:nlayers(normalraster), function(x){funclasifier(normalraster[[x]],"equal")}))
  })
  }else if(classifierName == "Non-Classifier"){
    classout <- s
  }
  
  names(classout) <- names(s)
  #------------------ Siniflandirma yapilan haritalarla test verisinin birlestirip 5 sinifli haritanin 0 1 haritasina dondurulmesi ----------------
  ##################################################################################################### 
  ### Create Training and Testing Datasets  ####  Eðitim Test Verisinin Oluþturulmasý 
  #####################################################################################################
  arc.progress_label("Preparing Data Set...")
  arc.progress_pos(40)
  #featurelerin tutuldugu raster veri ile train verisinin rasterini resample yapýp egitim ve test olarak ayirmak icin hazir hale getirir
  traindata <- FeatureData(classout,train)
  
  Rocdata <- FeatureData(s,train)
  trainMatrix <- traindata[,-ncol(traindata)]
  trainMatrix <- ifelse(trainMatrix >= cutoff,1,0)

  ##################################################################################################### 
  ### Calculate ACCURACY, AUC, MSE, MAE, RMSE  #### Doðruluk, AUC, MSE, MAE, RMSE Hesaplanýyor
  #####################################################################################################
  arc.progress_label("Calculating Accuracy, AUC(Classified), MSE, MAE, RMSE, AUC(RAW), Kappa, Precision, Recall and F1")
  arc.progress_pos(70)
  #-----------   Metrics kutuphanesi ile AUC, ACCURACY, MSE, MAE, RMSE degerlerinin hesaplanmasi ------------
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
  ### Write Out Results  #### Sonuçlar Yazdýrýlýyor
  #####################################################################################################
  arc.progress_label("Write Out Results")
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
  #tiff dosyasý olarak kaydetmek icin gerekli olan fonksiyon ve dosyanin ozellikleri
  tiff(filename = rocPath,width = 2400, height = 2400,res = 300)
  #yandaki bosluklari siler
  par(pty = "s")
  #renklendirme için
  color <- rainbow(ncol(trainMatrix))
  #roc sonuclarinin ciktilarini tutacak olan liste
  r1 <- lapply(1:ncol(trainMatrix), function(x) NULL)
  #AUC degerlerindeki virgülden sonraki deger sayisini 3'e indirir
  listofAUC <- round(as.numeric(resultAUC),digits = 3)
  #olusturulacak olan legend in degerleri % kacinin train oldugu
  legendnames <- names(s)
 
  for(i in 1:ncol(trainMatrix)){
    r1[[i]] <-   roc(response = Rocdata$train, predictor = Rocdata[[i]], plot=F,legacy.axes = TRUE,percent = TRUE)
    listofAUC[[i]] <- round(r1[[i]]$auc, digits = 3)
    #her dongu sonunda plot siliniyor yeni plotun olusturulmasi 
    if(i == 1){
      plot(r1[[i]],col = color[i])
      #baslangic disindaki degerleri cizgi olarak plot a eklenmesi
    }else{ 
      lines(r1[[i]],col = color[i])
    }
  }
   #legend icin % degerleri ve AUC degerlerinin birlestirir
  a <- paste(legendnames,"  AUC : ",as.character(listofAUC),sep = "")
  #olusturulan plot a icerisindeki degerlerin ne oldugunun yazdirilmasi
  legend("bottomright",a, col = color,cex = 1,lwd = 1:2)
  
  #plot'un kapatýlmasý veya silinmesi bu islem yapilmadan dosya kaydedilemiyor
  dev.off()
  
  arc.progress_pos(100)
  return(out_params)
}















