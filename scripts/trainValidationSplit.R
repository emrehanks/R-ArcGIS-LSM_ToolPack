#olan ve olmayan alanlarin secili oldugu Shape dosyalarini az olan alana gore 
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
# alanlarin buyukluge degil sayisina gore %70 train %30 test
#verisi olarak raster veriye cevirip kaydeder

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
  if (!requireNamespace("rgeos", quietly = TRUE))
    install.packages("rgeos")
  if (!requireNamespace("svDialogs", quietly = TRUE))
    install.packages("svDialogs")
  
  require(sp)
  require(rgdal)
  require(raster)
  require(rgeos)
  require(svDialogs)
  #yazilan fonksiyonlarin uzantilari
  source("C:/Users/Public/kullanilanFonksiyonlar.R")

  ##################################################################################################### 
  ### Define input/output parameters #### Girdi/Çýktý Parametrelerinin Tanýmlanmasý
  ##################################################################################################### 
  #dosya uzantilari
  olanPath <- in_params[[1]]
  olmayanPath <- in_params[[2]]
  percentvalue <- as.integer(in_params[[3]])
  resolation <-  as.integer(in_params[[4]])
  #Uretilecek olan Raster verinin kaydedilecegi yer ve adi
  kayitPath <- out_params[[1]]
  kayitPath2 <- out_params[[2]]
  #heyelan Olan ve olmayan alanlarý içeren polygon datasýný okumak ve raster train datasýna dönüþtürmek.
  
  ##################################################################################################### 
  ### Load Landslide and NonLandSlide Shape Data  ####  Heyelan Olan ve Olmayan Alan  Verilerinin Yüklenmesi
  #####################################################################################################
  arc.progress_label("Veri Yükleniyor...")
  arc.progress_pos(20)
  #-------- Heyelan olan ve olmayan alanlarin shape dosyasindan okunmasi ve train, test olarak ayrilmasi -------------
  olanShp <- arc.open(olanPath)
  olanShp <- arc.data2sp(arc.select(olanShp))
  
  olmayanShp <- arc.open(olmayanPath)
  olmayanShp <- arc.data2sp(arc.select(olmayanShp))

  #CRS kodlarinin kontrol edilmesi;
  #Uyuþmazlýk veya Boþ deðer olmasý durumunda uyari verir.
  #Uyarý sonucunda kullanýcý devam etmesini isteyebilir
  result <- "sonuc"
  crsCodes <- c(proj4string(olanShp), proj4string(olmayanShp))
  result <- CrsCheck(crsCodes)
  if(result == "cancel") return(out_params)
  
  
  #Extentler arasýnda kesiþim noktasýnýn kontrol edilmesi iþlemi
  #eðer kesiþim noktalarý yoksa bunlar ya parklý koordinat sistemindedirler yada
  #farklý yerleri göstermektedir. Bu þekilde iþlem yapýlamayacaðýndan
  #uyarý ekraný çýkartýlmýþtýr
  cevap <- "cevap"
  extents <- c(extent(olanShp),extent(olmayanShp))
  cevap <-extentCheck(extents)
  if(cevap == "no") return(out_params)
  
  #heyelan olan olanlarin 1 olamyan alanlarin 0 olarak atanmasi birlestirme yapildiginda gerekli olacak veri ayirmasi icin
  
  olanShp$heyelanTur <- 1
  olmayanShp$heyelanTur <- 0


  ##################################################################################################### 
  ### Train Test Split  ####  Eðitim Test Verisinin Ayrýlmasý
  #####################################################################################################
  arc.progress_label("Verilen Yüzdeye Göre Ayrým Yapýlýyor...")
  arc.progress_pos(40)
  #train test veri sayisinin belirlenmesi
  olantrain <- as.integer(nrow(olanShp)*percentvalue/100)
  olmayantrain <- as.integer(nrow(olmayanShp)* percentvalue/100)

  #heyelan olan alanlarin polygonlarin train ve test kadarinin seçilmesi
  olantrainsample <- sample(1:nrow(olanShp), olantrain)
  
  #heyelan olmayan alanlarin polygonlarin train ve test kadarinin seçilmesi
  olmayantrainsample <- sample(1:nrow(olmayanShp), olmayantrain)
  arc.progress_label("Heyelan Olan Alanlar Eðitim ve Test Olarak Ayrýlýyor...")
  arc.progress_pos(45)
  #heyelan olan alanlarin polygonlarin train ve test olarak ayrilmasi ayrilmasi
  olantrainpolygon <- olanShp[olantrainsample,]
  olantestpolygon <- olanShp[-(olantrainsample),]
  arc.progress_label("Heyelan Olmayan Alanlar Eðitim ve Test Olarak Ayrýlýyor...")
  arc.progress_pos(50)
  #heyelan olmayan alanlarin polygonlarin train ve test olarak ayrilmasi ayrilmasi
  olmayantrainpolygon <- olmayanShp[olmayantrainsample,]
  olmayantestpolygon <- olmayanShp[-(olmayantrainsample),]
  ##################################################################################################### 
  ### Create Train, Test Shape Data  ####  Eðitim, Test Verisinin Oluþturulmas
  #####################################################################################################
  arc.progress_label("Eðitim Ve Test Verisi oluþturuluyor...")
  arc.progress_pos(60)
  #heyelan olan ve olamayan train ve test verilerinin birlestirilmesi
  trainShp <- bind(olantrainpolygon, olmayantrainpolygon)
  testShp <- bind(olantestpolygon, olmayantestpolygon)
  rm(olantestpolygon,olmayantestpolygon,olmayantrainpolygon,olantrainpolygon)
  
  ##################################################################################################### 
  ### Train, Test Shape Data Turn to Raster Data  ####  Eðitim, Test Verisinin Raster'a Dönüþtürülmesi
  #####################################################################################################
  arc.progress_label("Poligon Verisi Raster Veriye Çevriliyor...")
  arc.progress_pos(80)
  
  trainRaster <- ShapetoRaster(trainShp,resolation,"heyelanTur")
  testRaster <- ShapetoRaster(testShp,resolation,"heyelanTur")
  crs(trainRaster) <- crs(testRaster) <- crs(olanShp)
  ##################################################################################################### 
  ### Write Out Train Test Data  ###  Eðitim Test Verilerinin Yazdýrýlmasý
  #####################################################################################################
  arc.progress_label("Eðitim ve Test Verisi Yazdýrýlýyor...")
  arc.write(data = trainRaster, path = if(grepl("\\.tif$", kayitPath)| grepl("\\.img$", kayitPath)) kayitPath
            else paste0(normalizePath(dirname(kayitPath)),"\\", sub('\\..*$', '', basename(kayitPath)),".tif") 
            ,overwrite=TRUE)
  arc.write(data = testRaster, path = if(grepl("\\.tif$", kayitPath2)| grepl("\\.img$", kayitPath2)) kayitPath2
            else paste0(normalizePath(dirname(kayitPath2)),"\\", sub('\\..*$', '', basename(kayitPath2)),".tif")
            ,overwrite=TRUE)
  arc.progress_pos(100)
return(out_params)
}
