#####################################################################################################  
### Article Name: Developing Comprehensive Geocomputation Tools for Landslide Susceptibility Mapping: LSM Tool Pack
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
### Dividing a Data Set into Training and Validation Samples
###
##################################################################################################### 

tool_exec <- function(in_params, out_params)
{
  
  #####################################################################################################  
  ### Check/Load required packages
  ##################################################################################################### 
  
  options(repos="https://CRAN.R-project.org")
  set.seed(24)
  round(memory.limit()/2^20, 2)
  
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
  if (!requireNamespace("rgeos", quietly = TRUE))
    install.packages("rgeos")
  if (!requireNamespace("svDialogs", quietly = TRUE))
    install.packages("svDialogs")
  
  require(sp)
  require(rgdal)
  require(raster)
  require(rgeos)
  require(svDialogs)
  ##################################################################################################### 
  ### Define functions
  ##################################################################################################### 
  ShapetoRaster <- function(konumV, resolation,field = "Landslide"){
    RasterF <- raster(extent(konumV))
    res(RasterF) <- resolation
    RasterF <- rasterize(konumV,field = field,RasterF, updateValue= "NA")
    return(RasterF)
  }
  
  ##################################################################################################### 
  ### Define input/output parameters #### Girdi/Çýktý Parametrelerinin Tanýmlanmasý
  ##################################################################################################### 
  arc.progress_label("Reading Data...")
  arc.progress_pos(20)
  landslidePath <- in_params[[1]]
  nonlandslidePath <- in_params[[2]]
  percentvalue <- as.integer(in_params[[3]])
  resolation <-  as.integer(in_params[[4]])
  trainPath <- out_params[[1]]
  validationPath <- out_params[[2]]
  
  ##################################################################################################### 
  ### Load Landslide and NonLandSlide Shape Data  ####  Heyelan Olan ve Olmayan Alan  Verilerinin Yüklenmesi
  #####################################################################################################
  #Read Landslide and Non-Landslide Polygon data
  landslideShp <- arc.open(landslidePath)
  landslideShp <- arc.data2sp(arc.select(landslideShp))
  
  nonlandslideShp <- arc.open(nonlandslidePath)
  nonlandslideShp <- arc.data2sp(arc.select(nonlandslideShp))

  #Assing Landslide is 1 , Non-Landslide is 0
  landslideShp$Landslide <- 1
  nonlandslideShp$Landslide <- 0

  ##################################################################################################### 
  ### Train Test Split  ####  Eðitim Test Verisinin Ayrýlmasý
  #####################################################################################################
  arc.progress_label("Distinction by Percentage...")
  arc.progress_pos(40)
  
  landslidetrain <- as.integer(nrow(landslideShp)*percentvalue/100)
  nonlandslidetrain <- as.integer(nrow(nonlandslideShp)* percentvalue/100)

  landslidetrainSample <- sample(1:nrow(landslideShp), landslidetrain)
  nonlandslidetrainSample <- sample(1:nrow(nonlandslideShp), nonlandslidetrain)
  
  arc.progress_label("Landslide Areas Are Separated as Train and Validation ...")
  arc.progress_pos(45)
  
  landslideTrainPolygon <- landslideShp[landslidetrainSample,]
  landslideTestPolygon <- landslideShp[-(landslidetrainSample),]
  
  arc.progress_label("Non-Landslide Areas Are Separated as Train and Validation ...")
  arc.progress_pos(50)
  
  nonlandslideTrainPolygon <- nonlandslideShp[nonlandslidetrainSample,]
  nonlandslideTestPolygon <- nonlandslideShp[-(nonlandslidetrainSample),]
  
  ##################################################################################################### 
  ### Create Train, Test Shape Data  ####  Eðitim, Test Verisinin Oluþturulmas
  #####################################################################################################
  arc.progress_label("Generating Train and Validation Data...")
  arc.progress_pos(60)
  
  trainShp <- bind(landslideTrainPolygon, nonlandslideTrainPolygon)
  validationShp <- bind(landslideTestPolygon, nonlandslideTestPolygon)
  rm(landslideTestPolygon,nonlandslideTestPolygon,nonlandslideTrainPolygon,landslideTrainPolygon)
  
  ##################################################################################################### 
  ### Train, Test Shape Data Turn to Raster Data  ####  Eðitim, Test Verisinin Raster'a Dönüþtürülmesi
  #####################################################################################################
  arc.progress_label("Polygon to Raster")
  arc.progress_pos(80)
  
  trainRaster <- ShapetoRaster(trainShp,resolation,"Landslide")
  validationRaster <- ShapetoRaster(validationShp,resolation,"Landslide")
  crs(trainRaster) <- crs(validationRaster) <- crs(landslideShp)
  
  ##################################################################################################### 
  ### Write Out Train Test Data  ###  Eðitim Test Verilerinin Yazdýrýlmasý
  #####################################################################################################
  arc.progress_label("Write Train and Validation...")
  arc.write(data = trainRaster, path = if(grepl("\\.tif$", trainPath)| grepl("\\.img$", trainPath)) trainPath
            else paste0(normalizePath(dirname(trainPath)),"\\", sub('\\..*$', '', basename(trainPath)),".tif") 
            ,overwrite=TRUE)
  arc.write(data = validationRaster, path = if(grepl("\\.tif$", validationPath)| grepl("\\.img$", validationPath)) validationPath
            else paste0(normalizePath(dirname(validationPath)),"\\", sub('\\..*$', '', basename(validationPath)),".tif")
            ,overwrite=TRUE)
  arc.progress_pos(100)
return(out_params)
}
