#Random Forest
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
  arc.progress_label("Kütüphaneler Yükleniyor...")
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
  #yazilan fonksiyonlarin uzantilari
  source("C:/Users/Public/kullanilanFonksiyonlar.R")
  
  ##################################################################################################### 
  ### Define input/output parameters #### Girdi/Çýktý Parametrelerinin Tanýmlanmasý
  #####################################################################################################  
  arc.progress_label("Veriler Okunuyor...")
  arc.progress_pos(20)
  rasterPath <- in_params[[1]]
  csvPath <- in_params[[2]]
  #value type a bagli olarak ayarlanir
  type <- as.character(in_params[[3]])
  value <- as.integer(in_params[[4]])
  #degisken altkumelerinin sayisi default olarak faktor sayisi / 3
  subsetN <- as.integer(in_params[[5]])
  #olusturulacak agac sayisi islem suresini uzatir 
  treeN <- as.integer(in_params[[6]])
  #dal olusumu icin gerekli ornek sayisi agacin karmasikligini ayarlar
  nodesize <- as.integer(in_params[[7]])
  trainPath <- in_params[[8]]
  roctf <- out_params[[1]]
  treePath <- out_params[[2]]
  featPath <- out_params[[3]]
  kayitPath <- out_params[[4]]
  
  ##################################################################################################### 
  ### Load Data  ####  Verilerin Okunmasý
  #####################################################################################################
  #raster stack verinin okunmasý
  # rasters1 <-  arc.raster(arc.open(rasterPath))
  # rasters1 <-  arc.data2sp(rasters1)
  rasters1 <- brick(rasterPath)
  
  #Harita isimlerinin tutuluduðu csv dosyasý seçilmiþ ise
  if(length(csvPath)){
    stackNames<-read.csv(csvPath)
    if(nlayers(rasters1) == nrow(stackNames)){
      stackNames <- as.character(stackNames[[2]])
      names(rasters1) <- stackNames
    }else{
      msg_box("Yuklediginiz Faktör Ýsimleri Dosyasý ile Raster Verinin Uzunluklarý Tutmadýðýndan\n
              Faktor Ýsimleri Band1,Band2.... olarak duzenlenecektir...")
      featureName <- unlist(lapply(1:nlayers(rasters1),function(x) paste0("Band",x)))
      names(rasters1) <- featureName
    }
  }else{
    nameRas <- lapply(1:nlayers(rasters1), function(x)  paste0("Band",x))
    namesRas <- unlist(nameRas)
    names(rasters1) <- namesRas
  }
  
  #eðitim verisinin okunmasý
  # train <- arc.raster(arc.open(trainPath))
  # train <- arc.data2sp(train)
  train <- raster(trainPath)
  
  ##################################################################################################### 
  ### Check Train Data  ####  Train Datasý Kontrol Ediliyor
  #####################################################################################################
  arc.progress_label("Eðitim Verisi Kontrol Ediliyor...")
  #Read Raster Stack
  cevap <- "cevap"
  cevap <- trainTableKontrol(train)
  while(cevap != "cevap"){
    if(cevap == "yes"){
      train <- trainDataControl(train)
      cevap <- trainTableKontrol(train)
    }else{
      return(out_params)
    }
    
  }
  
  #CRS kodlarinin kontrol edilmesi;
  #Uyuþmazlýk veya Boþ deðer olmasý durumunda uyari verir.
  #Uyarý sonucunda kullanýcý devam etmesini isteyebilir
  result <- "sonuc"
  crsCodes <- c(proj4string(rasters1), proj4string(train))
  result <- CrsCheck(crsCodes)
  if(result == "cancel") return(out_params)
  
  
  #Extentler arasýnda kesiþim noktasýnýn kontrol edilmesi iþlemi
  #eðer kesiþim noktalarý yoksa bunlar ya parklý koordinat sistemindedirler yada
  #farklý yerleri göstermektedir. Bu þekilde iþlem yapýlamayacaðýndan
  #uyarý ekraný çýkartýlmýþtýr
  arc.progress_label("Extent ve Çözünürlük Kontrol Ediliyor...")
  arc.progress_pos(30)
  cevap <- "cevap"
  extents <- c(extent(rasters1),extent(train))
  resos <- c(list(res(rasters1)),list(res(train)))
  cevap <- resoCheck(resos)
  if(cevap == "no") return(out_params)
  cevap <-extentCheck(extents)
  if(cevap == "no") return(out_params)
  
  ##################################################################################################### 
  ### Create Training and Testing Datasets  ####  Eðitim Test Verisinin Oluþturulmasý 
  #####################################################################################################
  arc.progress_label("Veri Seti Hazýrlanýyor...")
  arc.progress_pos(40)
  #featurelerin tutuldugu raster veri ile train verisinin rasterini resample yapýp egitim ve test olarak ayirmak icin hazir hale getirir
  valueDF <- FeatureData(rasters1,train)
  
  #verilen type ve value degerine gore train ve test verisi ayrimi yapar
  trainTestDf <- TrainTestSplit(value_table = valueDF,type = type, value = value)
  #train ve test verisinin degiskenlere atanmasi
  traindata <- trainTestDf$train
  testdata <- trainTestDf$test
  
  #####################################################################################################
  ### Fit Model ### Modelin Eðitilmesi
  #####################################################################################################
  arc.progress_label("Model Oluþturuyor...")
  arc.progress_pos(60)
  rfFit <- randomForest(formula = train ~ ., data = traindata, ntree = treeN, mtry = subsetN, nodesize = nodesize)

  feaImp <- importance(rfFit)
  # print(fitreg)


  #####################################################################################################
  ### Predict Model ### Model Çýktýlarýnýn Alýnmasý
  #####################################################################################################
  arc.progress_label("Veri Tahmini Yapýlýyor(Predict)...")
  arc.progress_pos(60)
  #raster verinin eðitilen veri seti ile predict edilmesi
  rfRasterPredict <- predict(rasters1, rfFit, na.rm = T)
  #raster verinin normalizasyonu
  rfNormalRasterPredict <- normalizationraster(rfRasterPredict)
  
  
  #####################################################################################################
  ### Write Out ### Veriler Yazdýrýlýyor
  #####################################################################################################
  arc.progress_label("Veri Yazdýrýlýyor...")
  arc.progress_pos(90)
 
  if(length(roctf)){
      #Test verisinin predict edilmesi
      rfTestPredict <- predict(rfFit, testdata,type = "response")
      #Roc eðrisi çizilmesi istenilmesi durumda yapýlacak iþlemler
      rfRoc <- roc(response = testdata$train,predictor = rfTestPredict, plot=FALSE,legacy.axes = TRUE,percent = TRUE)
      auc <- round(rfRoc$auc,digit = 4)
      legendname <- paste0("Random Forest ","AUC : ",auc)
      
      tiff(roctf, width = 1920, height = 1080, res = 200)
      par(pty = "s")
      plot(rfRoc)
      legend("bottomright",legendname,cex = 1,lwd = 1:2)
      dev.off()
  }
  #4K cikti
  if(length(treePath)){
    trees <- getTree(rfFit,labelVar = T)
    d <- to.dendrogram(trees)
    tiff(treePath, width = 5120, height = 2880, res = 200)
    plot(d,center=TRUE,edgePar=list(t.cex=.55,p.col=NA,p.lty=0), yaxt = "n")
    dev.off()
  }
  if(length(featPath)){
    #Write out Feature Importance
    write.xlsx(feaImp,file = featPath,col.names = T, row.names = T)
  }
  arc.write(data = rfNormalRasterPredict, path = if(grepl("\\.tif$", kayitPath)| grepl("\\.img$", kayitPath)) kayitPath
            else paste0(normalizePath(dirname(kayitPath)),"\\", sub('\\..*$', '', basename(kayitPath)),".tif")
            ,overwrite=TRUE)
  arc.progress_pos(100)
  return(out_params)
}