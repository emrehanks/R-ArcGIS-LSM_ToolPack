# Hybrid Feature Selection(HFS) R-ArcGIS-Toolbox

## Features

* [Hybrid Feature Selection](https://github.com/emrehanks/R-ArcGIS/blob/master/scripts/featureSelection.R)
* [Create LSM with Logistic Regression](https://github.com/emrehanks/R-ArcGIS/blob/master/scripts/logisticRegression.R)
* [Create LSM with Random Forest](https://github.com/emrehanks/R-ArcGIS/blob/master/scripts/randomForest.R)
* [LSM Comparison](https://github.com/emrehanks/R-ArcGIS/blob/master/scripts/LSMComparison.R)
## Requirements

* [ArcGIS 10.3.1 or later](http://desktop.arcgis.com/en/desktop/) or [ArcGIS Pro 1.1 or later](http://pro.arcgis.com/en/pro-app/) ([don't have it? try a 60 day trial](http://www.esri.com/software/arcgis/arcgis-for-desktop/free-trial))
* [R Statistical Computing Software, 3.3.2 or later](http://cran.cnr.berkeley.edu/bin/windows/base/) ([What is R?](http://www.r-project.org/about.html))
* [ArcGIS R-Bridge](https://github.com/R-ArcGIS/r-bridge)
* [Java Runtime Environment](https://www.java.com/en/download/) for FSelector package
* If you are setting up modules for the first time, you will need an internet connection to install the R-packages on the depository.

## Installation

* First, make sure you've installed the [requirements.](https://github.com/emrehanks/R-ArcGIS/blob/master/README.md#requirements)
* [Download this repository](https://github.com/emrehanks/R-ArcGIS/archive/master.zip) and unzip this file
* Open your ArcGIS application
* This clip shows you how to add the toolbox:
![](https://github.com/emrehanks/R-ArcGIS/blob/master/img/addtoolbox.gif)


##  Visual presentation of the tool pack modules

### Hybrid Feature Selection Module
This clip shows you how to use the module for selecting the best feature subset:

![](https://github.com/emrehanks/R-ArcGIS/blob/master/img/featureselectionExecute.gif)


### Logistic Regression Module
This clip shows you how to use the LR algortihm for produce susceptibility map. This module  provides the user statistical results and LR model ROC curve and AUC value.

![](https://github.com/emrehanks/R-ArcGIS/blob/master/img/LogRegExecute.gif)


### Random Forest Tool
This clip shows you how to use the tool: This module provides the user RF feature importance results as an excel sheet paper and RF model ROC curve and AUC value as a 300dpi  TIFF image.

![](https://github.com/emrehanks/R-ArcGIS/blob/master/img/RanForExecute.gif)

### LSM Comparison Tool
This clip shows you how to use the tool: This module provides the user accuracy metric results (Overall accuracy, Kappa, AUC, and F1 values) as an excel sheet paper. 

![](https://github.com/emrehanks/R-ArcGIS/blob/master/img/LSMComparisonExecute.gif)


## Lisence

[Apache 2.0](LISENCE)
