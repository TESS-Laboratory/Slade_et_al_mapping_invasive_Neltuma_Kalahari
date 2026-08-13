# ===========================================================================
# IMPORTED - not part of the original published archive.
#
#   source repo   slade-prosopis
#   source path   WV2/WV_2_Majority_filter.R
#   ref           main @ 671e56f
#   sha256        c2af5fe9d09a711e5224871c7dea5dd6d1b8efff7be751d466d0ae821a4f2b5c
#
#   why           As above, WV2 variant. Also a stub.
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


WV2 <- rast("E:/Glenn/Botswana/Satellite_Data/WV2/1_6_m_mosaic/RF_WV2.tif")

#wbt_majority_filter(Bokspits_1_Class, "E:/Glenn/Botswana/R_Scripts/Glenn-Prosopis-ML/data/Bokspits_1_Class_MJ20",filterx=20,filtery=20)

## S4 method for signature 'SpatRaster'
RF_WV2_MJ25 <- focal(WV2, w=9, fun="modal", filename="E:/Glenn/Botswana/Satellite_Data/WV2/1_6_m_mosaic/RF_WV2_FMJ25.tif", overwrite=TRUE) 
