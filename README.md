#  Developing Comprehensive Geocomputation Tools for 
#  Landslide Susceptibility Mapping: LSM Tool Pack

## News and Announcements
* Two new modeling methods were added in The LSM_ToolPack namely, Support Vector Machine (SVM) and eXtreme gradient boosting (XGBoost) 
* Dear Users, Don't forget to follow the "Issues" tab for important announcements!
* If you meet the error code given below, please unzip this "recipes" file on your base R location (e.g. C:\Users\emrehan\OneDrive\Documents\R\win-library\3.6)
*       "Error: package or namespace load failed for ‘caret’ in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]):
         there is no package called ‘recipes’*"
* If you meet the error code given below, use ArcGIS Pro 2 or downgrade the latest R-Base version 4 to 3.6.3.
*       "Failed to initialize R interpreter*"
## Features

* [Data Preparation(Train/Validation Split)](https://github.com/emrehanks/R-ArcGIS/blob/master/scripts/trainValidationSplit.R)
* [Feature Selection](https://github.com/emrehanks/R-ArcGIS/blob/master/scripts/featureSelection.R)
* [Create LSM with Logistic Regression](https://github.com/emrehanks/R-ArcGIS/blob/master/scripts/logisticRegression.R)
* [Create LSM with Random Forest](https://github.com/emrehanks/R-ArcGIS/blob/master/scripts/randomForest.R)
* [Performance Evaluation](https://github.com/emrehanks/R-ArcGIS/blob/master/scripts/LSMComparison.R)
* [Create Raster Stack (Multi-Bands)](https://github.com/emrehanks/R-ArcGIS-LSM_ToolPack/blob/master/scripts/createRasterStack.R)
* [Support Vector Machine](https://github.com/emrehanks/R-ArcGIS-LSM_ToolPack/blob/master/scripts/7_SupportVectorMachines.R)
* [eXtreme gradient boosting (XGBoost)](https://github.com/emrehanks/R-ArcGIS-LSM_ToolPack/blob/master/scripts/8_ExtremeGradientBoosting.R)


## Requirements

* [ArcGIS 10.3.1 or later](http://desktop.arcgis.com/en/desktop/) or [ArcGIS Pro 1.1 or later](http://pro.arcgis.com/en/pro-app/) ([don't have it? try trial edition](http://www.esri.com/software/arcgis/arcgis-for-desktop/free-trial))
* [R Statistical Computing Software, 3.3.2 or later](http://cran.cnr.berkeley.edu/bin/windows/base/) ([What is R?](http://www.r-project.org/about.html)). !!! R Base 4.0 is not supported yet !!! [Recommended Version v3.6.3](https://cran.r-project.org/bin/windows/base/old/3.6.3/)
* [ArcGIS R-Bridge](https://github.com/R-ArcGIS/r-bridge-install) -- [Recommended Version v1.0.1.239](https://github.com/R-ArcGIS/r-bridge/releases/tag/v1.0.1.239)
* [Java Runtime Environment](https://java.com/en/download/manual.jsp) for FSelector package
* If you are setting up modules for the first time, you will need an internet connection to install the R-packages on the depository.

## Installation

* First, make sure you've installed the [requirements.](https://github.com/emrehanks/R-ArcGIS/blob/master/README.md#requirements)
* [Download this repository](https://github.com/emrehanks/R-ArcGIS/archive/master.zip) and unzip this file
* Open your ArcGIS application
* This clip shows you how to add the toolbox:
![](https://github.com/emrehanks/R-ArcGIS/blob/master/img/addtoolbox1.gif)


##  Visual presentation of the tool pack modules

### 1_Data Preparation(Train/Validation Split) Module
This clip shows you how to divide your data train and validation data:

![](https://github.com/emrehanks/R-ArcGIS/blob/master/img/dataPreparation.gif)


### 2_Feature Selection Module
This clip shows you how to use the module for selecting the best feature subset:

![](https://github.com/emrehanks/R-ArcGIS/blob/master/img/featureSelection.gif)


### 3_Logistic Regression Module
This clip shows you how to use the LR algortihm for produce susceptibility map. This module  provides the user statistical results and LR model ROC curve and AUC value.

![](https://github.com/emrehanks/R-ArcGIS/blob/master/img/logisticReg.gif)


### 4_Random Forest Module
This clip shows you how to use the tool: This module provides the user RF feature importance results as an excel sheet paper and RF model ROC curve and AUC value as a 300dpi  TIFF image.

![](https://github.com/emrehanks/R-ArcGIS/blob/master/img/RanFor.gif)

### 5_Performance Evaluation Module
This clip shows you how to use the tool: This module provides the user accuracy metric results (Overall accuracy, Kappa, AUC, and F1 values) as an excel sheet paper. 

![](https://github.com/emrehanks/R-ArcGIS/blob/master/img/PerformanceEvaluator.gif)

### 6_Create Raster Stack (Multi-Bands)
This clip shows you how to use the tool: this module transforms your single factor maps into raster stack map.

![](https://github.com/emrehanks/R-ArcGIS-LSM_ToolPack/blob/master/img/RasterStack.gif)

## License

[Apache 2.0](https://github.com/emrehanks/R-ArcGIS-LSM_ToolPack/blob/master/LICENSE)

## Acknowledgements
 
LSM Tool Pack was prepared as part of the projects “Development of ArcGIS Interfaces with R programming language for Landslide Susceptibility Mapping” (No. 118Y090) funded by The Scientific and Technological Research Council of Turkey (TUBITAK). 

## Reference

Emrehan Kutlug Sahin, Ismail Colkesen, Suheda Semih Acmali, Aykut Akgun, Arif Cagdas Aydinoglu,
Developing comprehensive geocomputation tools for landslide susceptibility mapping: LSM tool pack, Computers & Geosciences, 2020, 104592, ISSN 0098-3004,
https://doi.org/10.1016/j.cageo.2020.104592.
(http://www.sciencedirect.com/science/article/pii/S009830042030577X)
