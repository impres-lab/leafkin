# leafkin - 1.0.0
leafkin is an R package to perform the calculations required for kinematic analysis of monocot leaves. The R package is accompanied by a publication in Quantitative Plant Biology and can be found through the following DOI: XXXXXXXX


## Abstract:

Growth is one of the most studied plant responses. At the cellular level, plant growth is driven by cell division and cell expansion. A means to quantify these two cellular processes is through kinematic analysis, a methodology which has been developed and perfected over the past decades, with in depth descriptions of the methodology available. Unfortunately, after performing the lab work, researchers are required to perform time-consuming, repetitive and error prone calculations. To lower the barrier towards this final step in the analysis and to aid researchers currently applying this technique, we have created leafkin, an R package to perform all the calculations involved in the kinematic analysis of monocot leaves using only four functions. These functions support leaf elongation rate calculations, fitting of cell length profiles, extraction of fitted cell lengths and execution of kinematic equations. With the leafkin package, kinematic analysis of monocot leaves becomes more accessible than before.


## User specific errors / difficulties:

### 0. Installation of leafkin.
- Starting with R 4.0.0 (released April 2020), R for **Windows** uses a brand new toolchain bundle called Rtools40. Rtools40 is therefore required (advised) for leafkin installation through GitHub on Windows and can be downloaded from https://cran.r-project.org/bin/windows/Rtools/. After installation of Rtools40 on your computer, you should be set up to install leafkin form the impres-lab GitHub repository.
- During the intallation of leafkin using the following function: `devtools::install_github("impres-lab/leafkin")`, you will be prompted to install and/or update the packages used by the leafkin package. On you first try, you should choose to install/update all packages suggested by the function (first option, **1: All**, when prompted: "*Enter one or more numbers, or an empty line to skip updates:*"). 
- If, during installtion, you are prompted with the question to compile packages or not (i.e. "*Do you want to install from sources the package which needs compilation? (Yes/no/cancel)*"), the most stable option here is to choose **no**, since local compilation of the latest uncompiled packages sometimes results in an error. When you select **no**, the latest precompiled version of the packages is installed, which is just fine for leafkin.
- When you have tried this and there is still an error occuring during installation, you should skip updates of the remaining packages causing an error during installation (just hit your **ENTER** key when prompted: "*Enter one or more numbers, or an empty line to skip updates:*". This should install leafkin.

### 1. Extra columns could be added through the use of Excel when creating the .txt file.

The use of Excel to enter data can, whilst being convenient, sometimes be the cause of errors in R. Excel sometimes adds extra tabs, resulting in extra rows without headers and data. These extra columns get named by R with default names, usually starting with an X. The functions rely on datasets with the right format. Therefor, these extra columns will result in an error when running the leafkin functions. A way to solve this issue is to select only the columns you are interested using the select function, provided by the dplyr package of the tidyverse. The following line will for instance only select 6 columns, starting with the first one, up until the sixth:

`leaf_length_measurements %>% select(1:6)`

If you make sure that the selected columns contain your data and not any extra column, processing of the data should now be errorless. 

### 2. Error when creating the pdf containing cell length fit plots: cannot open file

This means that you have a pdf opened with exactly the same name as the one the get_pdf_with_cell_length_fit_plots() function is trying to create. This happens for instance when you create a pdf file with the get_pdf_with_cell_length_fit_plots() function, open the created pdf and run the function again with the pdf file still open. With the pdf file still open, R cannot replace the old file by the new file. Just close the pdf file and you should be able to run the function again. 

### 3. Error when fitting cell lengths related to gridsize: Binning grid too coarse for current (small) bandwidth.

There are limits to the interval which can be chosen. An interval that is too coarse will result in an error related to the gridsize. Very small intervals will slow down the function. In our experience, the 10-centimetre growth zone of a maize leaf is ideally analysed with an interval of 0.1 or 0.01 centimetre (i.e. resulting in 101 or 1001 datapoints respectively). It is also important to note that an insufficient number of cell length measurements could result in a failed bandwidth calculation (though tests revealed that only extreme borderline disruptions in the data resulted in an error). In that case, cell lengths are not fitted, and a warning is printed after executing the function, indicating the number of plants for which no bandwidth could be calculated.
