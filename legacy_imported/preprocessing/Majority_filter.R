# ===========================================================================
# IMPORTED - not part of the original published archive.
#
#   source repo   slade-prosopis
#   source path   Majority_filter.R
#   ref           main @ 671e56f
#   sha256        d0d5a5815e09e038f35bbebe5112d3479ce942b3969578a1830e83702fc76af4
#
#   why           Nearest analogue to the methods 2.6 sieve filter. STUB: reads a raster and stops. Not a recovery path.
#
# Content below is VERBATIM and does not run as-is: it depends on Windows
# absolute paths under E:/Glenn/Botswana/ and on archived packages (rgeos,
# rgdal), and calls windowsFonts(). It is kept as the reference
# implementation to port into the targets pipeline, not to execute.
# See refactor-findings.md and audit/source-recovery-map.md.
# ===========================================================================

# Script comparing Majority filter settings for improving classifiacation



library(terra)
library(lubridate)
library(RColorBrewer)
library(ggplot2)
library(MASS)
library(splines)
library(rgeos)
library(tidyverse)
library(viridis)
library(gridExtra)
library(DescTools)
library(sf)
library(exactextractr)
library(writexl) 
library (ggplot2)
#library (raster)
library(cowplot)
library(viridisLite)
#library(whitebox)

#----1. Read in files----

# Image data


Bokspits_1_Class <- rast("E:/Glenn/Botswana/R_Scripts/Glenn-Prosopis-ML/data/svm_pixel_level_Bokspits_1_stack_5_CHM_ALLVI.tif")
Bokspits_2_Class <- rast("E:/Glenn/Botswana/R_Scripts/Glenn-Prosopis-ML/data/svm_pixel_level_Bokspits_2_stack_5_CHM_ALLVI.tif")
Bokspits_3_Class <- rast("E:/Glenn/Botswana/R_Scripts/Glenn-Prosopis-ML/data/svm_pixel_level_Bokspits_3_stack_5_CHM_ALLVI.tif")

Struizendam_1_Class <- rast("E:/Glenn/Botswana/R_Scripts/Glenn-Prosopis-ML/data/svm_pixel_level_Struizendam_1_stack_5_CHM_ALLVI.tif")
Struizendam_2_Class <- rast("E:/Glenn/Botswana/R_Scripts/Glenn-Prosopis-ML/data/svm_pixel_level_Struizendam_2_stack_5_CHM_ALLVI.tif")
Struizendam_3_Class <- rast("E:/Glenn/Botswana/R_Scripts/Glenn-Prosopis-ML/data/svm_pixel_level_Struizendam_3_stack_5_CHM_ALLVI.tif")
Struizendam_4_Class <- rast("E:/Glenn/Botswana/R_Scripts/Glenn-Prosopis-ML/data/svm_pixel_level_Struizendam_4_stack_5_CHM_ALLVI.tif")

#wbt_majority_filter(Bokspits_1_Class, "E:/Glenn/Botswana/R_Scripts/Glenn-Prosopis-ML/data/Bokspits_1_Class_MJ20",filterx=20,filtery=20)

## S4 method for signature 'SpatRaster'
Bokspits_1_MJ25 <- focal(Bokspits_1_Class, w=25, fun="modal", filename="E:/Glenn/Botswana/R_Scripts/Glenn-Prosopis-ML/data/Bokspits_1_Class_FMJ25.tif", overwrite=TRUE) 
Bokspits_2_MJ25 <- focal(Bokspits_2_Class, w=25, fun="modal", filename="E:/Glenn/Botswana/R_Scripts/Glenn-Prosopis-ML/data/Bokspits_2_Class_FMJ25.tif", overwrite=TRUE) 
Bokspits_3_MJ25 <- focal(Bokspits_3_Class, w=25, fun="modal", filename="E:/Glenn/Botswana/R_Scripts/Glenn-Prosopis-ML/data/Bokspits_3_Class_FMJ25.tif", overwrite=TRUE) 



Struizendam_1_MJ25 <- focal(Struizendam_1_Class, w=25, fun="modal", filename="E:/Glenn/Botswana/R_Scripts/Glenn-Prosopis-ML/data/Struizendam_1_Class_FMJ25.tif", overwrite=TRUE) 
Struizendam_2_MJ25 <- focal(Struizendam_2_Class, w=25, fun="modal", filename="E:/Glenn/Botswana/R_Scripts/Glenn-Prosopis-ML/data/Struizendam_2_Class_FMJ25.tif", overwrite=TRUE) 
Struizendam_3_MJ25 <- focal(Struizendam_3_Class, w=25, fun="modal", filename="E:/Glenn/Botswana/R_Scripts/Glenn-Prosopis-ML/data/Struizendam_3_Class_FMJ25.tif", overwrite=TRUE) 
Struizendam_4_MJ25 <- focal(Struizendam_4_Class, w=25, fun="modal", filename="E:/Glenn/Botswana/R_Scripts/Glenn-Prosopis-ML/data/Struizendam_4_Class_FMJ25.tif", overwrite=TRUE) 


WV2 <- rast("E:/Glenn/Botswana/Satellite_Data/WV2/1_6_m_mosaic/RF_WV2.tif")
WV2_RF_MJ25 <- focal(WV2, w=9, fun="modal", filename="E:/Glenn/Botswana/Satellite_Data/WV2/1_6_m_mosaic/RF_WV2_MJ9.tif", overwrite=TRUE) 

WV2_Prosopis <-rast("E:/Glenn/Botswana/Satellite_Data/WV2/1_6_m_mosaic/RF_WV2_Prosopis_only.tif")
WV2_RF_P_MJ25 <- focal(WV2_Prosopis w=9, fun="modal", filename="E:/Glenn/Botswana/Satellite_Data/WV2/1_6_m_mosaic/RF_WV2_Prosopis_MJ9.tif", overwrite=TRUE) 
