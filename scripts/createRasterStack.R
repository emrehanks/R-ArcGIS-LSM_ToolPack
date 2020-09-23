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
### Create Raster Stack (MUlti-Bands) by Resample Referance Factor
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
  if (!requireNamespace("svDialogs", quietly = TRUE))
    install.packages("svDialogs")

  require(rgdal)
  require(raster)
  require(sp)
  require(svDialogs)
  
  ##################################################################################################### 
  ### Define input/output parameters #### Girdi/Çýktý Parametrelerinin Tanýmlanmasý
  ##################################################################################################### 
  arc.progress_label("Reading Data...")
  arc.progress_pos(40)
  otherPath <- in_params[[1]]
  basePath <- in_params[[2]]
  writeCsv <- out_params[[1]]
  kayitPath <-out_params[[2]]
  
  ##################################################################################################### 
  ### Load data
  #####################################################################################################
  rfile <- otherPath
  baseSample <- raster(basePath)
  
  ##################################################################################################### 
  ### Resample Data
  #####################################################################################################
  arc.progress_label("Resample Factor Rasters...")
  arc.progress_pos(60) 
  r <- lapply(rfile,function(x){
    turnRaster <- raster(x)
    resample(turnRaster,baseSample,method='bilinear')
  })
  
  arc.progress_label("Create Raster Stack...")
  arc.progress_pos(80)
  stackData <- stack(r)
  
  ##################################################################################################### 
  ### Write Raster Stack
  #####################################################################################################
  arc.progress_label("Write Raster Stack...")
  arc.progress_pos(90)
  if(length(writeCsv)){
      write.csv(names(stackData),writeCsv)
  }
  writeRaster(x = stackData, filename = if(grepl("\\.tif$", kayitPath)| grepl("\\.img$", kayitPath)) kayitPath
              else paste0(normalizePath(dirname(kayitPath)),"\\", sub('\\..*$', '', basename(kayitPath)),".tif")
              ,overwrite=TRUE)
  return(out_params)
}

