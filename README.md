# leafkin
leafkin is an R package to perform the calculations required for kinematic analysis of monocot leaves. The R package is accompanied by a publication in Quantitative Plant Biology and can be found through the following DOI: XXXXXXXX


## Abstract:

Growth is one of the most studied plant responses. At the cellular level, plant growth is driven by cell division and cell expansion. A means to quantify these two cellular processes is through kinematic analysis, a methodology which has been developed and perfected over the past decades, with in depth descriptions of the methodology available. Unfortunately, after performing the lab work, researchers are required to perform time-consuming, repetitive and error prone calculations. To lower the barrier towards this final step in the analysis and to aid researchers currently applying this technique, we have created leafkin, an R-package to perform all the calculations involved in the kinematic analysis of monocot leaves using only four functions. These functions support leaf elongation rate calculations, fitting of cell length profiles, extraction of fitted cell lengths and execution of kinematic equations. With the leafkin package, kinematic analysis of monocot leaves becomes more accessible than before.


## User specific errors / difficulties:

1. Extra columns could be added through the use of Excel when creating the .txt file.

The use of Excel to enter data can, whilst being convenient, sometimes be the cause of errors in R. Excel sometimes adds extra tabs, resulting in extra rows without headers and data. These extra columns get named by R with default names, usually starting with an X. The functions rely on datasets with the right format. Therefor, these extra columns will result in an error when running the leafkin functions. A way to solve this issue is to select only the columns you are interested using the select function, provided by the dplyr package of the tidyverse. The following line will for instance only select 6 columns, starting with the first one, up until the sixth:

leaf_length_measurements %>% select(1:6)

If you make sure that the selected columns contain your data and not any extra column, processing of the data should now be errorless. 
