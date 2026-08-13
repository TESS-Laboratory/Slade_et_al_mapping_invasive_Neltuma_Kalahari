# ===========================================================================
# IMPORTED - not part of the original published archive.
#
#   source repo   slade-prosopis
#   source path   Planet_Struizendam_Merge.R
#   ref           main @ 671e56f
#   sha256        0a6a27c9548b833f71c9c9942a029b8f9ce32e41e345772da26ca2013e3860e2
#
#   why           Produces Struizendam_2022_09_07.tif, an input the published repo consumes but never creates.
#
# Content below is VERBATIM and does not run as-is: it depends on Windows
# absolute paths under E:/Glenn/Botswana/ and on archived packages (rgeos,
# rgdal), and calls windowsFonts(). It is kept as the reference
# implementation to port into the targets pipeline, not to execute.
# See refactor-findings.md and audit/source-recovery-map.md.
# ===========================================================================

# Script to merge planet rastr image data with overlapping extent 
# Planet data for Struizendam AOI comes from two different image tiles and requires merging


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
library (raster)
library(dplyr)

# import raster data

a <- rast("E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope/Raw_Data/files/5911761_3426713_2022-09-07_2446_BGRN_SR_clip.tif")
b <- rast("E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope/Raw_Data/files/5911761_3426813_2022-09-07_2446_BGRN_SR_clip.tif")

c <- mosaic(a, b, fun="mean", filename="E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope/Struizendam_2022_09_07.tif", overwrite=TRUE)
