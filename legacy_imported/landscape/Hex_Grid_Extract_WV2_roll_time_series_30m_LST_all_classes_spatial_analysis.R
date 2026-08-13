# ===========================================================================
# IMPORTED - not part of the original published archive.
#
#   source repo   slade-prosopis
#   source path   Aggregate/Hex_Grid_Extract_WV2_roll_time_series_30m_LST_all_classes_spatial_analysis.R
#   ref           main @ 671e56f
#   sha256        97eaaf35d7e67c8fa80ba2eb14814e60d313819d3f10df9ae183a60ebe82ed16
#
#   why           Most evolved of eight near-identical roll_time_series variants; the other seven are not imported.
#
# Content below is VERBATIM and does not run as-is: it depends on Windows
# absolute paths under E:/Glenn/Botswana/ and on archived packages (rgeos,
# rgdal), and calls windowsFonts(). It is kept as the reference
# implementation to port into the targets pipeline, not to execute.
# See refactor-findings.md and audit/source-recovery-map.md.
# ===========================================================================

### Script to Extract Data from RF Classifications based on Landsat polygons and then extract data from NDVI
#rolling 10 year NDVI slope analysis 

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
library(remotes)
library(cowplot)
library(reshape2)

#----1. Read in files for WV2 ----


WV2_RF_hex <- rast("E:/Glenn/Botswana/Satellite_Data/WV2/merged_mosaic/RF_WV2_all_train_val_combined_b30_additional_WV2_merged_mosaic.tif")

NDVITrend<- rast("E:/Glenn/Botswana/Satellite_Data/Trend_Image_Data/Landsat/Trend_Img_NDVI_1984_2000_bi_dry.tif")
NDVITrend2<- rast("E:/Glenn/Botswana/Satellite_Data/Trend_Image_Data/Landsat/Trend_Img_NDVI_2000_2022_bi_dry.tif")
T94<- rast("E:/Glenn/Botswana/Satellite_Data/Trend_Image_Data/Landsat/Roll/Trend_Img_NDVI_1984-1994.tif")
T95<- rast("E:/Glenn/Botswana/Satellite_Data/Trend_Image_Data/Landsat/Roll/Trend_Img_NDVI_1985-1995.tif")
T96<- rast("E:/Glenn/Botswana/Satellite_Data/Trend_Image_Data/Landsat/Roll/Trend_Img_NDVI_1986-1996.tif")
T97<- rast("E:/Glenn/Botswana/Satellite_Data/Trend_Image_Data/Landsat/Roll/Trend_Img_NDVI_1987-1997.tif")
T98<- rast("E:/Glenn/Botswana/Satellite_Data/Trend_Image_Data/Landsat/Roll/Trend_Img_NDVI_1988-1998.tif")
T99<- rast("E:/Glenn/Botswana/Satellite_Data/Trend_Image_Data/Landsat/Roll/Trend_Img_NDVI_1989-1999.tif")
T00<- rast("E:/Glenn/Botswana/Satellite_Data/Trend_Image_Data/Landsat/Roll/Trend_Img_NDVI_1990-2000.tif")
T01<- rast("E:/Glenn/Botswana/Satellite_Data/Trend_Image_Data/Landsat/Roll/Trend_Img_NDVI_1991-2001.tif")
T02<- rast("E:/Glenn/Botswana/Satellite_Data/Trend_Image_Data/Landsat/Roll/Trend_Img_NDVI_1992-2002.tif")
T03<- rast("E:/Glenn/Botswana/Satellite_Data/Trend_Image_Data/Landsat/Roll/Trend_Img_NDVI_1993-2003.tif")
T04<- rast("E:/Glenn/Botswana/Satellite_Data/Trend_Image_Data/Landsat/Roll/Trend_Img_NDVI_1994-2004.tif")
T05<- rast("E:/Glenn/Botswana/Satellite_Data/Trend_Image_Data/Landsat/Roll/Trend_Img_NDVI_1995-2005.tif")
T06<- rast("E:/Glenn/Botswana/Satellite_Data/Trend_Image_Data/Landsat/Roll/Trend_Img_NDVI_1996-2006.tif")
T07<- rast("E:/Glenn/Botswana/Satellite_Data/Trend_Image_Data/Landsat/Roll/Trend_Img_NDVI_1997-2007.tif")
T08<- rast("E:/Glenn/Botswana/Satellite_Data/Trend_Image_Data/Landsat/Roll/Trend_Img_NDVI_1998-2008.tif")
T09<- rast("E:/Glenn/Botswana/Satellite_Data/Trend_Image_Data/Landsat/Roll/Trend_Img_NDVI_1999-2009.tif")
T10<- rast("E:/Glenn/Botswana/Satellite_Data/Trend_Image_Data/Landsat/Roll/Trend_Img_NDVI_2000-2010.tif")
T11<- rast("E:/Glenn/Botswana/Satellite_Data/Trend_Image_Data/Landsat/Roll/Trend_Img_NDVI_2001-2011.tif")
T12<- rast("E:/Glenn/Botswana/Satellite_Data/Trend_Image_Data/Landsat/Roll/Trend_Img_NDVI_2002-2012.tif")
T13<- rast("E:/Glenn/Botswana/Satellite_Data/Trend_Image_Data/Landsat/Roll/Trend_Img_NDVI_2003-2013.tif")
T14<- rast("E:/Glenn/Botswana/Satellite_Data/Trend_Image_Data/Landsat/Roll/Trend_Img_NDVI_2004-2014.tif")
T15<- rast("E:/Glenn/Botswana/Satellite_Data/Trend_Image_Data/Landsat/Roll/Trend_Img_NDVI_2005-2015.tif")
T16<- rast("E:/Glenn/Botswana/Satellite_Data/Trend_Image_Data/Landsat/Roll/Trend_Img_NDVI_2006-2016.tif")
T17<- rast("E:/Glenn/Botswana/Satellite_Data/Trend_Image_Data/Landsat/Roll/Trend_Img_NDVI_2007-2017.tif")
T18<- rast("E:/Glenn/Botswana/Satellite_Data/Trend_Image_Data/Landsat/Roll/Trend_Img_NDVI_2008-2018.tif")
T19<- rast("E:/Glenn/Botswana/Satellite_Data/Trend_Image_Data/Landsat/Roll/Trend_Img_NDVI_2009-2019.tif")
T20<- rast("E:/Glenn/Botswana/Satellite_Data/Trend_Image_Data/Landsat/Roll/Trend_Img_NDVI_2010-2020.tif")
T21<- rast("E:/Glenn/Botswana/Satellite_Data/Trend_Image_Data/Landsat/Roll/Trend_Img_NDVI_2011-2021.tif")
T22<- rast("E:/Glenn/Botswana/Satellite_Data/Trend_Image_Data/Landsat/Roll/Trend_Img_NDVI_2012-2022.tif")





#LST <-terra::project(NDVITrend, y="EPSG:32734")
NDVI_trend  <-terra::project(NDVITrend, y="EPSG:32734")
NDVI_trend2  <-terra::project(NDVITrend, y="EPSG:32734")
T94  <-terra::project(T94, y="EPSG:32734")
T95  <-terra::project(T95, y="EPSG:32734")
T96  <-terra::project(T96, y="EPSG:32734")
T97  <-terra::project(T97, y="EPSG:32734")
T98  <-terra::project(T98, y="EPSG:32734")
T99  <-terra::project(T99, y="EPSG:32734")
T00  <-terra::project(T00, y="EPSG:32734")
T01  <-terra::project(T01, y="EPSG:32734")
T02  <-terra::project(T02, y="EPSG:32734")
T03  <-terra::project(T03, y="EPSG:32734")
T04  <-terra::project(T04, y="EPSG:32734")
T05  <-terra::project(T05, y="EPSG:32734")
T06  <-terra::project(T06, y="EPSG:32734")
T07  <-terra::project(T07, y="EPSG:32734")
T08  <-terra::project(T08, y="EPSG:32734")
T09  <-terra::project(T09, y="EPSG:32734")
T10  <-terra::project(T10, y="EPSG:32734")
T11  <-terra::project(T11, y="EPSG:32734")
T12  <-terra::project(T12, y="EPSG:32734")
T13  <-terra::project(T13, y="EPSG:32734")
T14  <-terra::project(T14, y="EPSG:32734")
T15  <-terra::project(T15, y="EPSG:32734")
T16  <-terra::project(T16, y="EPSG:32734")
T17  <-terra::project(T17, y="EPSG:32734")
T18  <-terra::project(T18, y="EPSG:32734")
T19  <-terra::project(T19, y="EPSG:32734")
T20  <-terra::project(T20, y="EPSG:32734")
T21  <-terra::project(T21, y="EPSG:32734")
T22  <-terra::project(T22, y="EPSG:32734")


#hex250 <-terra::project(NDVITrend, y="EPSG:32734")
## NB You cant use exact extract with SpatVector so re-importing it a a spatial DF
hex100_Grid_SF <- read_sf(dsn = 'E:/Glenn/Botswana/GIS_aggregate', layer = "hex_100m_wv2_wide_clip_minus_corner")

hex250_Grid_SF <- read_sf(dsn = 'E:/Glenn/Botswana/GIS_aggregate', layer = "hex_250m_wv2_wide_clip")
hex500_Grid_SF <- read_sf(dsn = 'E:/Glenn/Botswana/GIS_aggregate', layer = "hex_500m_wv2_wide_clip")
grid30_SF<- read_sf(dsn = 'E:/Glenn/Botswana/GIS_aggregate', layer = "grid_30m_wv2_wide")

# #---2. Extract data for 250m sized polygons
# 
# 
# WV2_RF_hex_Extract <- exact_extract(WV2_RF_hex,hex250_Grid_SF,"mode")
# names(WV2_RF_hex_Extract) <- c('mode')
# WV2_RF_hex_Extract_DF <-bind_cols(hex250_Grid_SF,WV2_RF_hex_Extract)
# 
# WV2_RF_hex_Extract <- exact_extract(WV2_RF_hex,hex250_Grid_SF,"variance")
# names(WV2_RF_hex_Extract) <- c('variance')
# WV2_RF_hex_Extract_DF <- dplyr::mutate(WV2_RF_hex_Extract_DF, variance = WV2_RF_hex_Extract)
# 
# WV2_RF_hex_Extract <- exact_extract(WV2_RF_hex,hex250_Grid_SF,"majority" )
# names(WV2_RF_hex_Extract) <- c('majority')
# WV2_RF_hex_Extract_DF <- dplyr::mutate(WV2_RF_hex_Extract_DF, majority = WV2_RF_hex_Extract)
# 
# WV2_RF_hex_Extract <- exact_extract(WV2_RF_hex,hex250_Grid_SF,"variety" )
# names(WV2_RF_hex_Extract) <- c('variety')
# WV2_RF_hex_Extract_DF <- dplyr::mutate(WV2_RF_hex_Extract_DF, variety = WV2_RF_hex_Extract)
# 
# WV2_RF_hex_Extract <- exact_extract(WV2_RF_hex,hex250_Grid_SF,"count" )
# names(WV2_RF_hex_Extract) <- c('count')
# WV2_RF_hex_Extract_DF <- dplyr::mutate(WV2_RF_hex_Extract_DF, count = WV2_RF_hex_Extract)
# 
# WV2_RF_hex_Extract <- exact_extract(WV2_RF_hex,hex250_Grid_SF,"frac" )# calculates fraction cover for each classification value
# #a column is added with the FVC for each vegetation type frac1, frac2 etc...
# WV2_RF_hex_Extract_DF <- dplyr::mutate(WV2_RF_hex_Extract_DF, WV2_RF_hex_Extract)
# 
# 
# 
# st_write(WV2_RF_hex_Extract_DF, dsn = "E:/Glenn/Botswana/GIS_aggregate/hex_250m_extract.shp")
# saveRDS(WV2_RF_hex_Extract_DF, file = "E:/Glenn/Botswana/GIS_aggregate/hex_250m_extract.rds")
# write.csv(WV2_RF_hex_Extract_DF, file = "E:/Glenn/Botswana/GIS_aggregate/hex_250m_extract.csv")
# 
# # Now Repeat for 500m szied hex polygons
# 
# WV2_RF_hex_Extract <- exact_extract(WV2_RF_hex,hex500_Grid_SF,"mode")
# names(WV2_RF_hex_Extract) <- c('mode')
# WV2_RF_hex_Extract_DF <-bind_cols(hex500_Grid_SF,WV2_RF_hex_Extract)
# 
# WV2_RF_hex_Extract <- exact_extract(WV2_RF_hex,hex500_Grid_SF,"variance")
# names(WV2_RF_hex_Extract) <- c('variance')
# WV2_RF_hex_Extract_DF <- dplyr::mutate(WV2_RF_hex_Extract_DF, variance = WV2_RF_hex_Extract)
# 
# WV2_RF_hex_Extract <- exact_extract(WV2_RF_hex,hex500_Grid_SF,"majority" )
# names(WV2_RF_hex_Extract) <- c('majority')
# WV2_RF_hex_Extract_DF <- dplyr::mutate(WV2_RF_hex_Extract_DF, majority = WV2_RF_hex_Extract)
# 
# WV2_RF_hex_Extract <- exact_extract(WV2_RF_hex,hex500_Grid_SF,"variety" )
# names(WV2_RF_hex_Extract) <- c('variety')
# WV2_RF_hex_Extract_DF <- dplyr::mutate(WV2_RF_hex_Extract_DF, variety = WV2_RF_hex_Extract)
# 
# WV2_RF_hex_Extract <- exact_extract(WV2_RF_hex,hex500_Grid_SF,"count" )
# names(WV2_RF_hex_Extract) <- c('count')
# WV2_RF_hex_Extract_DF <- dplyr::mutate(WV2_RF_hex_Extract_DF, count = WV2_RF_hex_Extract)
# 
# WV2_RF_hex_Extract <- exact_extract(WV2_RF_hex,hex500_Grid_SF,"frac" )# calculates fraction cover for each classification value
# #a column is added with the FVC for each vegetation type frac1, frac2 etc...
# WV2_RF_hex_Extract_DF <- dplyr::mutate(WV2_RF_hex_Extract_DF, WV2_RF_hex_Extract)
# 
# 
# 
# st_write(WV2_RF_hex_Extract_DF, dsn = "E:/Glenn/Botswana/GIS_aggregate/hex_500m_extract.shp")
# saveRDS(WV2_RF_hex_Extract_DF, file = "E:/Glenn/Botswana/GIS_aggregate/hex_500m_extract.rds")
# write.csv(WV2_RF_hex_Extract_DF, file = "E:/Glenn/Botswana/GIS_aggregate/hex_500m_extract.csv")
# 
# # Now repeat for 30m grid
# 
# WV2_RF_hex_Extract <- exact_extract(WV2_RF_hex,grid30_SF,"mode")
# names(WV2_RF_hex_Extract) <- c('mode')
# WV2_RF_hex_Extract_DF <-bind_cols(grid30_SF,WV2_RF_hex_Extract)
# 
# WV2_RF_hex_Extract <- exact_extract(WV2_RF_hex,grid30_SF,"variance")
# names(WV2_RF_hex_Extract) <- c('variance')
# WV2_RF_hex_Extract_DF <- dplyr::mutate(WV2_RF_hex_Extract_DF, variance = WV2_RF_hex_Extract)
# 
# WV2_RF_hex_Extract <- exact_extract(WV2_RF_hex,grid30_SF,"majority" )
# names(WV2_RF_hex_Extract) <- c('majority')
# WV2_RF_hex_Extract_DF <- dplyr::mutate(WV2_RF_hex_Extract_DF, majority = WV2_RF_hex_Extract)
# 
# WV2_RF_hex_Extract <- exact_extract(WV2_RF_hex,grid30_SF,"variety" )
# names(WV2_RF_hex_Extract) <- c('variety')
# WV2_RF_hex_Extract_DF <- dplyr::mutate(WV2_RF_hex_Extract_DF, variety = WV2_RF_hex_Extract)
# 
# WV2_RF_hex_Extract <- exact_extract(WV2_RF_hex,grid30_SF,"count" )
# names(WV2_RF_hex_Extract) <- c('count')
# WV2_RF_hex_Extract_DF <- dplyr::mutate(WV2_RF_hex_Extract_DF, count = WV2_RF_hex_Extract)
# 
# WV2_RF_hex_Extract <- exact_extract(WV2_RF_hex,grid30_SF,"frac" )# calculates fraction cover for each classification value
# #a column is added with the FVC for each vegetation type frac1, frac2 etc...
# WV2_RF_hex_Extract_DF <- dplyr::mutate(WV2_RF_hex_Extract_DF, WV2_RF_hex_Extract)
# 
# 
# 
# st_write(WV2_RF_hex_Extract_DF, dsn = "E:/Glenn/Botswana/GIS_aggregate/grid_30m_extract.shp")
# saveRDS(WV2_RF_hex_Extract_DF, file = "E:/Glenn/Botswana/GIS_aggregate/grid_30m_extract.rds")
# write.csv(WV2_RF_hex_Extract_DF, file = "E:/Glenn/Botswana/GIS_aggregate/grid_30m_extract.csv")

# Extract for 100m polygons

hex100_Grid_SF<-grid30_SF

WV2_RF_hex_Extract <- exact_extract(WV2_RF_hex,hex100_Grid_SF,"mode")
names(WV2_RF_hex_Extract) <- c('mode')
WV2_RF_hex_Extract_DF <-bind_cols(hex100_Grid_SF,WV2_RF_hex_Extract)

WV2_RF_hex_Extract <- exact_extract(WV2_RF_hex,hex100_Grid_SF,"majority" )
names(WV2_RF_hex_Extract) <- c('majority')
WV2_RF_hex_Extract_DF <- dplyr::mutate(WV2_RF_hex_Extract_DF, majority = WV2_RF_hex_Extract)

WV2_RF_hex_Extract <- exact_extract(WV2_RF_hex,hex100_Grid_SF,"frac" )# calculates fraction cover for each classification value
#a column is added with the FVC for each vegetation type frac1, frac2 etc...
WV2_RF_hex_Extract_DF <- dplyr::mutate(WV2_RF_hex_Extract_DF, WV2_RF_hex_Extract)

WV2_RF_hex_Extract <- exact_extract(NDVITrend,hex100_Grid_SF,"mean" )
names(WV2_RF_hex_Extract) <- c('NDVI_84')
WV2_RF_hex_Extract_DF <- dplyr::mutate(WV2_RF_hex_Extract_DF, NDVI_84 = WV2_RF_hex_Extract)

WV2_RF_hex_Extract <- exact_extract(NDVITrend2,hex100_Grid_SF,"mean" )
names(WV2_RF_hex_Extract) <- c('NDVI_22')
WV2_RF_hex_Extract_DF <- dplyr::mutate(WV2_RF_hex_Extract_DF, NDVI_22 = WV2_RF_hex_Extract)

WV2_RF_hex_Extract <- exact_extract(T94,hex100_Grid_SF,"mean" )
names(WV2_RF_hex_Extract) <- c('T94')
WV2_RF_hex_Extract_DF <- dplyr::mutate(WV2_RF_hex_Extract_DF, "1994" = WV2_RF_hex_Extract)

WV2_RF_hex_Extract <- exact_extract(T95,hex100_Grid_SF,"mean" )
names(WV2_RF_hex_Extract) <- c('T95')
WV2_RF_hex_Extract_DF <- dplyr::mutate(WV2_RF_hex_Extract_DF, "1995" = WV2_RF_hex_Extract)

WV2_RF_hex_Extract <- exact_extract(T96,hex100_Grid_SF,"mean" )
names(WV2_RF_hex_Extract) <- c('T96')
WV2_RF_hex_Extract_DF <- dplyr::mutate(WV2_RF_hex_Extract_DF, "1996" = WV2_RF_hex_Extract)

WV2_RF_hex_Extract <- exact_extract(T97,hex100_Grid_SF,"mean" )
names(WV2_RF_hex_Extract) <- c('T97')
WV2_RF_hex_Extract_DF <- dplyr::mutate(WV2_RF_hex_Extract_DF, "1997" = WV2_RF_hex_Extract)

WV2_RF_hex_Extract <- exact_extract(T98,hex100_Grid_SF,"mean" )
names(WV2_RF_hex_Extract) <- c('T98')
WV2_RF_hex_Extract_DF <- dplyr::mutate(WV2_RF_hex_Extract_DF, "1998" = WV2_RF_hex_Extract)

WV2_RF_hex_Extract <- exact_extract(T99,hex100_Grid_SF,"mean" )
names(WV2_RF_hex_Extract) <- c('T99')
WV2_RF_hex_Extract_DF <- dplyr::mutate(WV2_RF_hex_Extract_DF, "1999" = WV2_RF_hex_Extract)

WV2_RF_hex_Extract <- exact_extract(T00,hex100_Grid_SF,"mean" )
names(WV2_RF_hex_Extract) <- c('T00')
WV2_RF_hex_Extract_DF <- dplyr::mutate(WV2_RF_hex_Extract_DF, "2000" = WV2_RF_hex_Extract)

WV2_RF_hex_Extract <- exact_extract(T01,hex100_Grid_SF,"mean" )
names(WV2_RF_hex_Extract) <- c('T01')
WV2_RF_hex_Extract_DF <- dplyr::mutate(WV2_RF_hex_Extract_DF, "2001" = WV2_RF_hex_Extract)

WV2_RF_hex_Extract <- exact_extract(T02,hex100_Grid_SF,"mean" )
names(WV2_RF_hex_Extract) <- c('T02')
WV2_RF_hex_Extract_DF <- dplyr::mutate(WV2_RF_hex_Extract_DF, "2002" = WV2_RF_hex_Extract)

WV2_RF_hex_Extract <- exact_extract(T03,hex100_Grid_SF,"mean" )
names(WV2_RF_hex_Extract) <- c('T03')
WV2_RF_hex_Extract_DF <- dplyr::mutate(WV2_RF_hex_Extract_DF, "2003" = WV2_RF_hex_Extract)

WV2_RF_hex_Extract <- exact_extract(T04,hex100_Grid_SF,"mean" )
names(WV2_RF_hex_Extract) <- c('T04')
WV2_RF_hex_Extract_DF <- dplyr::mutate(WV2_RF_hex_Extract_DF, "2004" = WV2_RF_hex_Extract)

WV2_RF_hex_Extract <- exact_extract(T05,hex100_Grid_SF,"mean" )
names(WV2_RF_hex_Extract) <- c('T05')
WV2_RF_hex_Extract_DF <- dplyr::mutate(WV2_RF_hex_Extract_DF, "2005" = WV2_RF_hex_Extract)

WV2_RF_hex_Extract <- exact_extract(T06,hex100_Grid_SF,"mean" )
names(WV2_RF_hex_Extract) <- c('T06')
WV2_RF_hex_Extract_DF <- dplyr::mutate(WV2_RF_hex_Extract_DF, "2006" = WV2_RF_hex_Extract)

WV2_RF_hex_Extract <- exact_extract(T07,hex100_Grid_SF,"mean" )
names(WV2_RF_hex_Extract) <- c('T07')
WV2_RF_hex_Extract_DF <- dplyr::mutate(WV2_RF_hex_Extract_DF, "2007" = WV2_RF_hex_Extract)

WV2_RF_hex_Extract <- exact_extract(T08,hex100_Grid_SF,"mean" )
names(WV2_RF_hex_Extract) <- c('T08')
WV2_RF_hex_Extract_DF <- dplyr::mutate(WV2_RF_hex_Extract_DF, "2008" = WV2_RF_hex_Extract)
WV2_RF_hex_Extract <- exact_extract(T09,hex100_Grid_SF,"mean" )
names(WV2_RF_hex_Extract) <- c('T09')
WV2_RF_hex_Extract_DF <- dplyr::mutate(WV2_RF_hex_Extract_DF, "2009" = WV2_RF_hex_Extract)

WV2_RF_hex_Extract <- exact_extract(T10,hex100_Grid_SF,"mean" )
names(WV2_RF_hex_Extract) <- c('T10')
WV2_RF_hex_Extract_DF <- dplyr::mutate(WV2_RF_hex_Extract_DF, "2010" = WV2_RF_hex_Extract)

WV2_RF_hex_Extract <- exact_extract(T11,hex100_Grid_SF,"mean" )
names(WV2_RF_hex_Extract) <- c('T11')
WV2_RF_hex_Extract_DF <- dplyr::mutate(WV2_RF_hex_Extract_DF, "2011" = WV2_RF_hex_Extract)

WV2_RF_hex_Extract <- exact_extract(T12,hex100_Grid_SF,"mean" )
names(WV2_RF_hex_Extract) <- c('T12')
WV2_RF_hex_Extract_DF <- dplyr::mutate(WV2_RF_hex_Extract_DF, "2012" = WV2_RF_hex_Extract)

WV2_RF_hex_Extract <- exact_extract(T13,hex100_Grid_SF,"mean" )
names(WV2_RF_hex_Extract) <- c('T13')
WV2_RF_hex_Extract_DF <- dplyr::mutate(WV2_RF_hex_Extract_DF, "2013" = WV2_RF_hex_Extract)

WV2_RF_hex_Extract <- exact_extract(T14,hex100_Grid_SF,"mean" )
names(WV2_RF_hex_Extract) <- c('T14')
WV2_RF_hex_Extract_DF <- dplyr::mutate(WV2_RF_hex_Extract_DF, "2014" = WV2_RF_hex_Extract)

WV2_RF_hex_Extract <- exact_extract(T15,hex100_Grid_SF,"mean" )
names(WV2_RF_hex_Extract) <- c('T15')
WV2_RF_hex_Extract_DF <- dplyr::mutate(WV2_RF_hex_Extract_DF, "2015" = WV2_RF_hex_Extract)

WV2_RF_hex_Extract <- exact_extract(T16,hex100_Grid_SF,"mean" )
names(WV2_RF_hex_Extract) <- c('T16')
WV2_RF_hex_Extract_DF <- dplyr::mutate(WV2_RF_hex_Extract_DF, "2016" = WV2_RF_hex_Extract)

WV2_RF_hex_Extract <- exact_extract(T17,hex100_Grid_SF,"mean" )
names(WV2_RF_hex_Extract) <- c('T17')
WV2_RF_hex_Extract_DF <- dplyr::mutate(WV2_RF_hex_Extract_DF, "2017" = WV2_RF_hex_Extract)

WV2_RF_hex_Extract <- exact_extract(T18,hex100_Grid_SF,"mean" )
names(WV2_RF_hex_Extract) <- c('T18')
WV2_RF_hex_Extract_DF <- dplyr::mutate(WV2_RF_hex_Extract_DF, "2018" = WV2_RF_hex_Extract)
WV2_RF_hex_Extract <- exact_extract(T19,hex100_Grid_SF,"mean" )
names(WV2_RF_hex_Extract) <- c('T19')
WV2_RF_hex_Extract_DF <- dplyr::mutate(WV2_RF_hex_Extract_DF, "2019" = WV2_RF_hex_Extract)

WV2_RF_hex_Extract <- exact_extract(T20,hex100_Grid_SF,"mean" )
names(WV2_RF_hex_Extract) <- c('T20')
WV2_RF_hex_Extract_DF <- dplyr::mutate(WV2_RF_hex_Extract_DF, "2020" = WV2_RF_hex_Extract)

WV2_RF_hex_Extract <- exact_extract(T21,hex100_Grid_SF,"mean" )
names(WV2_RF_hex_Extract) <- c('T21')
WV2_RF_hex_Extract_DF <- dplyr::mutate(WV2_RF_hex_Extract_DF, "2021" = WV2_RF_hex_Extract)

WV2_RF_hex_Extract <- exact_extract(T22,hex100_Grid_SF,"mean" )
names(WV2_RF_hex_Extract) <- c('T22')
WV2_RF_hex_Extract_DF <- dplyr::mutate(WV2_RF_hex_Extract_DF, "2022" = WV2_RF_hex_Extract)


st_write(WV2_RF_hex_Extract_DF, dsn = "E:/Glenn/Botswana/GIS_aggregate/LST_30m_wide_extract_dry_roll.shp")
saveRDS(WV2_RF_hex_Extract_DF, file = "E:/Glenn/Botswana/GIS_aggregate/LST_30m_extract_dry_roll.rds")
write.csv(WV2_RF_hex_Extract_DF, file = "E:/Glenn/Botswana/GIS_aggregate/LST_30m_extract_dry_roll.csv")

# Summary analysis


roads_buffer_SF <- read_sf(dsn = "E:/Glenn/Botswana/Final_Drone_Survey_Data/Buffers", layer = "1km_concentric_buffers_road_clipped_border")
roads_buffer_SF$mrb_dist <-as.numeric(roads_buffer_SF$mrb_dist)

settlment_buffer_SF <- read_sf(dsn = "E:/Glenn/Botswana/Final_Drone_Survey_Data/Buffers", layer = "1km_concentric_buffers_clipped_border")
settlment_buffer_SF$mrb_dist<-as.numeric(settlment_buffer_SF$mrb_dist)

SB1 <- settlment_buffer_SF%>% filter (settlment_buffer_SF$mrb_dist == 1000)
SB2 <- settlment_buffer_SF%>% filter (settlment_buffer_SF$mrb_dist == 2000)


WV2_RF_hex_Extract_DFSB1 <- st_contains(SB1,WV2_RF_hex_Extract_DF)

WV2_full_Extract_DF_95<- WV2_RF_hex_Extract_DF #%>% filter_at(vars(frac_1,frac_2,frac_3,frac_5,frac_6,frac_7,frac_10), any_vars(. > 0.95))
#WV2_full_Extract_DF_P <- WV2_full_WV2_Extract_DF %>% filter_at(vars(frac_1), any_vars(. > paste0(P)))


WV2_full_Extract_DF_95_1 <- WV2_full_Extract_DF_95 %>% filter (frac_1 > 0.6)
WV2_full_Extract_DF_95_1<- st_drop_geometry(WV2_full_Extract_DF_95_1)

WV2_full_Extract_DF_95_2 <- WV2_full_Extract_DF_95 %>% filter (frac_1 <0.6 )
WV2_full_Extract_DF_95_2 <- WV2_full_Extract_DF_95_2 %>% filter (frac_1 > 0.5 )
WV2_full_Extract_DF_95_2<- st_drop_geometry(WV2_full_Extract_DF_95_2)

WV2_full_Extract_DF_95_3 <- WV2_full_Extract_DF_95 %>% filter (frac_1 < 0.5)
WV2_full_Extract_DF_95_3 <- WV2_full_Extract_DF_95_3 %>% filter (frac_1 > 0.4)
WV2_full_Extract_DF_95_3<- st_drop_geometry(WV2_full_Extract_DF_95_3)

WV2_full_Extract_DF_95_4 <- WV2_full_Extract_DF_95 %>% filter (frac_1 < 0.4)
WV2_full_Extract_DF_95_4 <- WV2_full_Extract_DF_95_4 %>% filter (frac_1 > 0.3)
WV2_full_Extract_DF_95_4<- st_drop_geometry(WV2_full_Extract_DF_95_4)

WV2_full_Extract_DF_95_5 <- WV2_full_Extract_DF_95 %>% filter (frac_1 < 0.3)
WV2_full_Extract_DF_95_5 <- WV2_full_Extract_DF_95_5 %>% filter (frac_1 > 0.2)
WV2_full_Extract_DF_95_5<- st_drop_geometry(WV2_full_Extract_DF_95_5)

WV2_full_Extract_DF_95_6 <- WV2_full_Extract_DF_95 %>% filter (frac_1 < 0.2)
WV2_full_Extract_DF_95_6 <- WV2_full_Extract_DF_95_6 %>% filter (frac_1 > 0.1)
WV2_full_Extract_DF_95_6<- st_drop_geometry(WV2_full_Extract_DF_95_6)

WV2_full_Extract_DF_95_7 <- WV2_full_Extract_DF_95 %>% filter (frac_1 < 0.1)
WV2_full_Extract_DF_95_7 <- WV2_full_Extract_DF_95_7 %>% filter (frac_1 > 0.05)
WV2_full_Extract_DF_95_7<- st_drop_geometry(WV2_full_Extract_DF_95_7)

WV2_full_Extract_DF_95_8 <- WV2_full_Extract_DF_95 %>% filter (frac_1 < 0.01)
WV2_full_Extract_DF_95_8<- st_drop_geometry(WV2_full_Extract_DF_95_8)

Summary_df<-WV2_full_Extract_DF_95_1%>% summarise(across(where(is.numeric), mean,na.rm = TRUE))
Summary_2df<-WV2_full_Extract_DF_95_2%>% summarise(across(where(is.numeric), mean, na.rm = TRUE))
Summary_3df<-WV2_full_Extract_DF_95_3%>% summarise(across(where(is.numeric), mean,na.rm = TRUE))
Summary_4df<-WV2_full_Extract_DF_95_4%>% summarise(across(where(is.numeric), mean,na.rm = TRUE))
Summary_5df<-WV2_full_Extract_DF_95_5%>% summarise(across(where(is.numeric), mean,na.rm = TRUE))
Summary_6df<-WV2_full_Extract_DF_95_6%>% summarise(across(where(is.numeric), mean,na.rm = TRUE))
Summary_7df<-WV2_full_Extract_DF_95_7%>% summarise(across(where(is.numeric), mean,na.rm = TRUE))
Summary_8df<-WV2_full_Extract_DF_95_8%>% summarise(across(where(is.numeric), mean,na.rm = TRUE))


{
# WV2_full_Extract_DF_95_2 <- WV2_full_Extract_DF_95 %>% filter (frac_2 > 0.95)
# WV2_full_Extract_DF_95_3 <- WV2_full_Extract_DF_95 %>% filter (frac_3 > 0.95)
# WV2_full_Extract_DF_95_5 <- WV2_full_Extract_DF_95 %>% filter (frac_5 > 0.95)
# WV2_full_Extract_DF_95_6 <- WV2_full_Extract_DF_95 %>% filter (frac_6 > 0.95)
# WV2_full_Extract_DF_95_7 <- WV2_full_Extract_DF_95 %>% filter (frac_7 > 0.85)
# WV2_full_Extract_DF_95_10 <- WV2_full_Extract_DF_95 %>% filter (frac_10 > 0.95)
# 
# WV2_full_train <- bind_rows(WV2_full_Extract_DF_95_1,WV2_full_Extract_DF_95_2,WV2_full_Extract_DF_95_3,WV2_full_Extract_DF_95_5,WV2_full_Extract_DF_95_6,WV2_full_Extract_DF_95_7)

# saveRDS(WV2_full_train, "E:/Glenn/Botswana/R_Scripts/Glenn-Prosopis-ML/data_in/WV2_pixel_extract_full_train_ndvi.rds")
# st_write(WV2_full_train, dsn = "E:/Glenn/Botswana/R_Scripts/Glenn-Prosopis-ML/data_in/WV2_pixel_extract_full_train_ndvi.shp")

# 
# WV2_full_Extract_DF_95_1e <-WV2_full_Extract_DF_95_1 [ sample( which( WV2_full_Extract_DF_95_1$majority == "1" ) ,280 ) , ]
# WV2_full_Extract_DF_95_2e <-WV2_full_Extract_DF_95_2[ sample( which( WV2_full_Extract_DF_95_2$majority == "2" ) ,280) , ]
# WV2_full_Extract_DF_95_3e <-WV2_full_Extract_DF_95_3[ sample( which( WV2_full_Extract_DF_95_3$majority == "3" ) ,280) , ]
# WV2_full_Extract_DF_95_5e <-WV2_full_Extract_DF_95_5[ sample( which( WV2_full_Extract_DF_95_5$majority == "5" ) , 280) , ]
# WV2_full_Extract_DF_95_6e <-WV2_full_Extract_DF_95_6[ sample( which( WV2_full_Extract_DF_95_6$majority == "6" ) ,280 ), ]
# WV2_full_Extract_DF_95_7e <-WV2_full_Extract_DF_95_7[ sample( which( WV2_full_Extract_DF_95_7$majority == "7" ) ,280) , ]
# 
# WV2_full_traine <- bind_rows(WV2_full_Extract_DF_95_1e,WV2_full_Extract_DF_95_2e,WV2_full_Extract_DF_95_3e,WV2_full_Extract_DF_95_5e,WV2_full_Extract_DF_95_6e,WV2_full_Extract_DF_95_7e)
# 

# saveRDS(WV2_full_traine, "E:/Glenn/Botswana/R_Scripts/Glenn-Prosopis-ML/data_in/WV2_equal_class_size_280_train_ndvi.rds")
# st_write(WV2_full_traine, dsn = "E:/Glenn/Botswana/R_Scripts/Glenn-Prosopis-ML/data_in/WV2_equal_class_size_280_train_ndvi.shp")
}

## Plotting theme
theme_fancy <- function() {
  theme_bw() +
    theme(
      text = element_text(family = "Helvetica"),
      axis.text = element_text(size = 8, color = "black"),
      axis.title = element_text(size = 8, color = "black"),
      axis.line.x = element_line(size = 0.3, color = "black"),
      axis.line.y = element_line(size = 0.3, color = "black"),
      axis.ticks = element_line(size = 0.3, color = "black"),
      panel.border = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank(),
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
      plot.title = element_text(
        size = 8,
        vjust = 1,
        hjust = 0.5,
        color = "black"
      ),
      legend.text = element_text(size = 8, color = "black"),
      legend.title = element_text(size = 8, color = "black"),
      legend.position = c(0.9, 0.9),
      legend.key.size = unit(0.9, "line"),
      legend.background = element_rect(
        color = "black",
        fill = "transparent",
        size = 2,
        linetype = "blank"
      )
    )
}
windowsFonts("Helvetica" = windowsFont("Helvetica")) # Ensure font is mapped correctly

Summary_df2 <-Summary_df%>%dplyr:: select (-c(1:18))
Summary_df3<-Summary_df2 %>% 
  tidyr::pivot_longer(cols=everything(),
    names_to = "year", 
    values_to = "slope")


Summary_2df2 <-Summary_2df%>%dplyr:: select (-c(1:18))
Summary_2df3<-Summary_2df2 %>% 
  tidyr::pivot_longer(cols=everything(),
                      names_to = "year", 
                      values_to = "slope")
Summary_3df2 <-Summary_3df%>%dplyr:: select (-c(1:18))
Summary_3df3<-Summary_3df2 %>% 
  tidyr::pivot_longer(cols=everything(),
                      names_to = "year", 
                      values_to = "slope")
Summary_4df2 <-Summary_4df%>%dplyr:: select (-c(1:18))
Summary_4df3<-Summary_4df2 %>% 
  tidyr::pivot_longer(cols=everything(),
                      names_to = "year", 
                      values_to = "slope")


Summary_5df2 <-Summary_5df%>%dplyr:: select (-c(1:18))
Summary_5df3<-Summary_5df2 %>% 
  tidyr::pivot_longer(cols=everything(),
                      names_to = "year", 
                      values_to = "slope")

Summary_6df2 <-Summary_6df%>%dplyr:: select (-c(1:18))
Summary_6df3<-Summary_6df2 %>% 
  tidyr::pivot_longer(cols=everything(),
                      names_to = "year", 
                      values_to = "slope")
Summary_7df2 <-Summary_7df%>%dplyr:: select (-c(1:18))
Summary_7df3<-Summary_7df2 %>% 
  tidyr::pivot_longer(cols=everything(),
                      names_to = "year", 
                      values_to = "slope")
Summary_8df2 <-Summary_8df%>%dplyr:: select (-c(1:18))
Summary_8df3<-Summary_8df2 %>% 
  tidyr::pivot_longer(cols=everything(),
                      names_to = "year", 
                      values_to = "slope")

df <-  bind_cols(Summary_2df3,Summary_3df3,Summary_4df3,Summary_5df3,Summary_6df3,Summary_7df3,Summary_8df3)
df2 <- df%>%dplyr:: select (-c(3,5,7,9,11,13))
colnames(df2) <- c('year','50%','40%','30%','20%','10%','5%','None')


df4 <- melt(df2 ,  id.vars = 'year', variable.name = 'Type')

df4$year <-as.numeric(df4$year)
df4$Type <- factor(df4$Type, levels = c('year','50%','40%','30%','20%','10%','5%','None'))

PA <- ggplot(df4, aes(x = year, y = value, color = Type)) +
  geom_line()+  xlab("Year")+ ylab("NDVI % Relative Change per year over preceding 10 years")+ scale_color_manual(values=c("green", "blue","lightblue","grey","brown","yellow","orange"))+
  ggtitle( "Change in dry season NDVI over preceding 10 year period for 30m grid square \n with different fractional cover of Prosopis")+
  theme_fancy()+ theme(legend.position = c(0.1, 0.8))+ labs(color = "Prosopis cover") + scale_x_continuous(breaks=c(1990, 1995,2000, 2005,2010,2015,2020))
plot(PA) 

df3 <- df2 %>% mutate (Diff50 = df2$`50%`- df2$None)
df3 <- df3 %>% mutate (Diff40 = df3$`40%`- df3$None)
df3 <- df3 %>% mutate (Diff30 = df3$`30%`- df3$None)
df3 <- df3 %>% mutate (Diff20 = df3$`20%`- df3$None)
df3 <- df3 %>% mutate (Diff10 = df3$`10%`- df3$None)
df3 <- df3 %>% mutate (Diff5 = df3$`5%`- df3$None)


df3 <- df3%>%dplyr:: select (-c(2:8))


df5 <- melt(df3 ,  id.vars = 'year', variable.name = 'Type')

df5$year <-as.numeric(df5$year)
#df5$Type <- factor(df4$Type, levels = c('Diff60','Diff50','Diff40',"Diff30","Diff20","Diff10","Diff5"))



PB <- ggplot(df5, aes(x = year, y = value, color = Type)) +
  geom_line()+  xlab("Year")+ ylab("NDVI % Relative Change per year over preceding 10 years \n (-mean change NDVI for area) ")+ scale_color_manual(values=c("green", "black","blue","lightblue","grey","brown","yellow"))+
  ggtitle( "Change in dry season NDVI realtive to mean change over preceding 10 year period for 30m grid square \n with different fractional cover of Prosopis")+
  theme_fancy()+ theme(legend.position = c(0.1, 0.8))+ labs(color = "Prosopis cover") + scale_x_continuous(breaks=c(1990, 1995,2000, 2005,2010,2015,2020))
plot(PB) 



 

WV2_full_Extract_DF_95<- WV2_RF_hex_Extract_DF #%>% filter_at(vars(frac_1,frac_2,frac_3,frac_5,frac_6,frac_7,frac_10), any_vars(. > 0.95))
#WV2_full_Extract_DF_P <- WV2_full_WV2_Extract_DF %>% filter_at(vars(frac_1), any_vars(. > paste0(P)))


WV2_full_Extract_DF_95_1 <- WV2_full_Extract_DF_95 %>% filter (frac_6 > 0.6)
WV2_full_Extract_DF_95_1<- st_drop_geometry(WV2_full_Extract_DF_95_1)

WV2_full_Extract_DF_95_2 <- WV2_full_Extract_DF_95 %>% filter (frac_6 <0.6 )
WV2_full_Extract_DF_95_2 <- WV2_full_Extract_DF_95_2 %>% filter (frac_6 > 0.5 )
WV2_full_Extract_DF_95_2<- st_drop_geometry(WV2_full_Extract_DF_95_2)

WV2_full_Extract_DF_95_3 <- WV2_full_Extract_DF_95 %>% filter (frac_6 < 0.5)
WV2_full_Extract_DF_95_3 <- WV2_full_Extract_DF_95_3 %>% filter (frac_6 > 0.4)
WV2_full_Extract_DF_95_3<- st_drop_geometry(WV2_full_Extract_DF_95_3)

WV2_full_Extract_DF_95_4 <- WV2_full_Extract_DF_95 %>% filter (frac_6 < 0.4)
WV2_full_Extract_DF_95_4 <- WV2_full_Extract_DF_95_4 %>% filter (frac_6 > 0.3)
WV2_full_Extract_DF_95_4<- st_drop_geometry(WV2_full_Extract_DF_95_4)

WV2_full_Extract_DF_95_5 <- WV2_full_Extract_DF_95 %>% filter (frac_6 < 0.3)
WV2_full_Extract_DF_95_5 <- WV2_full_Extract_DF_95_5 %>% filter (frac_6 > 0.2)
WV2_full_Extract_DF_95_5<- st_drop_geometry(WV2_full_Extract_DF_95_5)

WV2_full_Extract_DF_95_6 <- WV2_full_Extract_DF_95 %>% filter (frac_6 < 0.2)
WV2_full_Extract_DF_95_6 <- WV2_full_Extract_DF_95_6 %>% filter (frac_6 > 0.1)
WV2_full_Extract_DF_95_6<- st_drop_geometry(WV2_full_Extract_DF_95_6)

WV2_full_Extract_DF_95_7 <- WV2_full_Extract_DF_95 %>% filter (frac_6 < 0.1)
WV2_full_Extract_DF_95_7 <- WV2_full_Extract_DF_95_7 %>% filter (frac_6 > 0.05)
WV2_full_Extract_DF_95_7<- st_drop_geometry(WV2_full_Extract_DF_95_7)

WV2_full_Extract_DF_95_8 <- WV2_full_Extract_DF_95 %>% filter (frac_1 < 0.01)
WV2_full_Extract_DF_95_8<- st_drop_geometry(WV2_full_Extract_DF_95_8)

Summary_df<-WV2_full_Extract_DF_95_1%>% summarise(across(where(is.numeric), mean,na.rm = TRUE))
Summary_2df<-WV2_full_Extract_DF_95_2%>% summarise(across(where(is.numeric), mean, na.rm = TRUE))
Summary_3df<-WV2_full_Extract_DF_95_3%>% summarise(across(where(is.numeric), mean,na.rm = TRUE))
Summary_4df<-WV2_full_Extract_DF_95_4%>% summarise(across(where(is.numeric), mean,na.rm = TRUE))
Summary_5df<-WV2_full_Extract_DF_95_5%>% summarise(across(where(is.numeric), mean,na.rm = TRUE))
Summary_6df<-WV2_full_Extract_DF_95_6%>% summarise(across(where(is.numeric), mean,na.rm = TRUE))
Summary_7df<-WV2_full_Extract_DF_95_7%>% summarise(across(where(is.numeric), mean,na.rm = TRUE))
Summary_8df<-WV2_full_Extract_DF_95_8%>% summarise(across(where(is.numeric), mean,na.rm = TRUE))



Summary_df2 <-Summary_df%>%dplyr:: select (-c(1:18))
Summary_df3<-Summary_df2 %>% 
  tidyr::pivot_longer(cols=everything(),
                      names_to = "year", 
                      values_to = "slope")


Summary_2df2 <-Summary_2df%>%dplyr:: select (-c(1:18))
Summary_2df3<-Summary_2df2 %>% 
  tidyr::pivot_longer(cols=everything(),
                      names_to = "year", 
                      values_to = "slope")
Summary_3df2 <-Summary_3df%>%dplyr:: select (-c(1:18))
Summary_3df3<-Summary_3df2 %>% 
  tidyr::pivot_longer(cols=everything(),
                      names_to = "year", 
                      values_to = "slope")
Summary_4df2 <-Summary_4df%>%dplyr:: select (-c(1:18))
Summary_4df3<-Summary_4df2 %>% 
  tidyr::pivot_longer(cols=everything(),
                      names_to = "year", 
                      values_to = "slope")


Summary_5df2 <-Summary_5df%>%dplyr:: select (-c(1:18))
Summary_5df3<-Summary_5df2 %>% 
  tidyr::pivot_longer(cols=everything(),
                      names_to = "year", 
                      values_to = "slope")

Summary_6df2 <-Summary_6df%>%dplyr:: select (-c(1:18))
Summary_6df3<-Summary_6df2 %>% 
  tidyr::pivot_longer(cols=everything(),
                      names_to = "year", 
                      values_to = "slope")
Summary_7df2 <-Summary_7df%>%dplyr:: select (-c(1:18))
Summary_7df3<-Summary_7df2 %>% 
  tidyr::pivot_longer(cols=everything(),
                      names_to = "year", 
                      values_to = "slope")
Summary_8df2 <-Summary_8df%>%dplyr:: select (-c(1:18))
Summary_8df3<-Summary_8df2 %>% 
  tidyr::pivot_longer(cols=everything(),
                      names_to = "year", 
                      values_to = "slope")

df <-  bind_cols(Summary_2df3,Summary_3df3,Summary_4df3,Summary_5df3,Summary_6df3,Summary_7df3,Summary_8df3)
df2 <- df%>%dplyr:: select (-c(3,5,7,9,11,13))
colnames(df2) <- c('year','50%','40%','30%','20%','10%','5%','Area')


df4 <- melt(df2 ,  id.vars = 'year', variable.name = 'Type')

df4$year <-as.numeric(df4$year)
df4$Type <- factor(df4$Type, levels = c('year','50%','40%','30%','20%','10%','5%','Area'))

PC <- ggplot(df4, aes(x = year, y = value, color = Type)) +
  geom_line()+  xlab("Year")+ ylab("NDVI % Relative Change per year over preceding 10 years")+ scale_color_manual(values=c("green", "blue","lightblue","grey","brown","yellow","orange"))+
  ggtitle( "Change in dry season NDVI over preceding 10 year period for 30m grid square \n with different fractional cover of Rig Trig")+
  theme_fancy()+ theme(legend.position = c(0.1, 0.8))+ labs(color = "Rig Trig cover") + scale_x_continuous(breaks=c(1990, 1995,2000, 2005,2010,2015,2020))
plot(PC) 

df3 <- df2 %>% mutate (Diff50 = df2$`50%`- df2$Area)
df3 <- df3 %>% mutate (Diff40 = df3$`40%`- df3$Area)
df3 <- df3 %>% mutate (Diff30 = df3$`30%`- df3$Area)
df3 <- df3 %>% mutate (Diff20 = df3$`20%`- df3$Area)
df3 <- df3 %>% mutate (Diff10 = df3$`10%`- df3$Area)
df3 <- df3 %>% mutate (Diff5 = df3$`5%`- df3$Area)


df3 <- df3%>%dplyr:: select (-c(2:8))


df5 <- melt(df3 ,  id.vars = 'year', variable.name = 'Type')

df5$year <-as.numeric(df5$year)
#df5$Type <- factor(df4$Type, levels = c('Diff60','Diff50','Diff40',"Diff30","Diff20","Diff10","Diff5"))



PD <- ggplot(df5, aes(x = year, y = value, color = Type)) +
  geom_line()+  xlab("Year")+ ylab("NDVI % Relative Change per year over preceding 10 years \n (-mean change NDVI for area) ")+ scale_color_manual(values=c("green", "black","blue","lightblue","grey","brown","yellow"))+
  ggtitle( "Change in dry season NDVI realtive to mean change over preceding 10 year period for 30m grid square \n with different fractional cover of Rig Trig")+
  theme_fancy()+ theme(legend.position = c(0.1, 0.8))+ labs(color = "Rig Trig cover") + scale_x_continuous(breaks=c(1990, 1995,2000, 2005,2010,2015,2020))
plot(PD) 


WV2_full_Extract_DF_95_1 <- WV2_full_Extract_DF_95 %>% filter (frac_3 > 0.6)
WV2_full_Extract_DF_95_1<- st_drop_geometry(WV2_full_Extract_DF_95_1)

WV2_full_Extract_DF_95_2 <- WV2_full_Extract_DF_95 %>% filter (frac_3 <0.6 )
WV2_full_Extract_DF_95_2 <- WV2_full_Extract_DF_95_2 %>% filter (frac_3 > 0.5 )
WV2_full_Extract_DF_95_2<- st_drop_geometry(WV2_full_Extract_DF_95_2)

WV2_full_Extract_DF_95_3 <- WV2_full_Extract_DF_95 %>% filter (frac_3 < 0.5)
WV2_full_Extract_DF_95_3 <- WV2_full_Extract_DF_95_3 %>% filter (frac_3 > 0.4)
WV2_full_Extract_DF_95_3<- st_drop_geometry(WV2_full_Extract_DF_95_3)

WV2_full_Extract_DF_95_4 <- WV2_full_Extract_DF_95 %>% filter (frac_3 < 0.4)
WV2_full_Extract_DF_95_4 <- WV2_full_Extract_DF_95_4 %>% filter (frac_3 > 0.3)
WV2_full_Extract_DF_95_4<- st_drop_geometry(WV2_full_Extract_DF_95_4)

WV2_full_Extract_DF_95_5 <- WV2_full_Extract_DF_95 %>% filter (frac_3 < 0.3)
WV2_full_Extract_DF_95_5 <- WV2_full_Extract_DF_95_5 %>% filter (frac_3 > 0.2)
WV2_full_Extract_DF_95_5<- st_drop_geometry(WV2_full_Extract_DF_95_5)

WV2_full_Extract_DF_95_6 <- WV2_full_Extract_DF_95 %>% filter (frac_3 < 0.2)
WV2_full_Extract_DF_95_6 <- WV2_full_Extract_DF_95_6 %>% filter (frac_3 > 0.1)
WV2_full_Extract_DF_95_6<- st_drop_geometry(WV2_full_Extract_DF_95_6)

WV2_full_Extract_DF_95_7 <- WV2_full_Extract_DF_95 %>% filter (frac_3 < 0.1)
WV2_full_Extract_DF_95_7 <- WV2_full_Extract_DF_95_7 %>% filter (frac_3 > 0.05)
WV2_full_Extract_DF_95_7<- st_drop_geometry(WV2_full_Extract_DF_95_7)

WV2_full_Extract_DF_95_8 <- WV2_full_Extract_DF_95 %>% filter (frac_1 < 0.01)
WV2_full_Extract_DF_95_8<- st_drop_geometry(WV2_full_Extract_DF_95_8)

Summary_df<-WV2_full_Extract_DF_95_1%>% summarise(across(where(is.numeric), mean,na.rm = TRUE))
Summary_2df<-WV2_full_Extract_DF_95_2%>% summarise(across(where(is.numeric), mean, na.rm = TRUE))
Summary_3df<-WV2_full_Extract_DF_95_3%>% summarise(across(where(is.numeric), mean,na.rm = TRUE))
Summary_4df<-WV2_full_Extract_DF_95_4%>% summarise(across(where(is.numeric), mean,na.rm = TRUE))
Summary_5df<-WV2_full_Extract_DF_95_5%>% summarise(across(where(is.numeric), mean,na.rm = TRUE))
Summary_6df<-WV2_full_Extract_DF_95_6%>% summarise(across(where(is.numeric), mean,na.rm = TRUE))
Summary_7df<-WV2_full_Extract_DF_95_7%>% summarise(across(where(is.numeric), mean,na.rm = TRUE))
Summary_8df<-WV2_full_Extract_DF_95_8%>% summarise(across(where(is.numeric), mean,na.rm = TRUE))



Summary_df2 <-Summary_df%>%dplyr:: select (-c(1:18))
Summary_df3<-Summary_df2 %>% 
  tidyr::pivot_longer(cols=everything(),
                      names_to = "year", 
                      values_to = "slope")


Summary_2df2 <-Summary_2df%>%dplyr:: select (-c(1:18))
Summary_2df3<-Summary_2df2 %>% 
  tidyr::pivot_longer(cols=everything(),
                      names_to = "year", 
                      values_to = "slope")
Summary_3df2 <-Summary_3df%>%dplyr:: select (-c(1:18))
Summary_3df3<-Summary_3df2 %>% 
  tidyr::pivot_longer(cols=everything(),
                      names_to = "year", 
                      values_to = "slope")
Summary_4df2 <-Summary_4df%>%dplyr:: select (-c(1:18))
Summary_4df3<-Summary_4df2 %>% 
  tidyr::pivot_longer(cols=everything(),
                      names_to = "year", 
                      values_to = "slope")


Summary_5df2 <-Summary_5df%>%dplyr:: select (-c(1:18))
Summary_5df3<-Summary_5df2 %>% 
  tidyr::pivot_longer(cols=everything(),
                      names_to = "year", 
                      values_to = "slope")

Summary_6df2 <-Summary_6df%>%dplyr:: select (-c(1:18))
Summary_6df3<-Summary_6df2 %>% 
  tidyr::pivot_longer(cols=everything(),
                      names_to = "year", 
                      values_to = "slope")
Summary_7df2 <-Summary_7df%>%dplyr:: select (-c(1:18))
Summary_7df3<-Summary_7df2 %>% 
  tidyr::pivot_longer(cols=everything(),
                      names_to = "year", 
                      values_to = "slope")
Summary_8df2 <-Summary_8df%>%dplyr:: select (-c(1:18))
Summary_8df3<-Summary_8df2 %>% 
  tidyr::pivot_longer(cols=everything(),
                      names_to = "year", 
                      values_to = "slope")

df <-  bind_cols(Summary_2df3,Summary_3df3,Summary_4df3,Summary_5df3,Summary_6df3,Summary_7df3,Summary_8df3)
df2 <- df%>%dplyr:: select (-c(3,5,7,9,11,13))
colnames(df2) <- c('year','50%','40%','30%','20%','10%','5%','Area')


df4 <- melt(df2 ,  id.vars = 'year', variable.name = 'Type')

df4$year <-as.numeric(df4$year)
df4$Type <- factor(df4$Type, levels = c('year','50%','40%','30%','20%','10%','5%','Area'))

PE <- ggplot(df4, aes(x = year, y = value, color = Type)) +
  geom_line()+  xlab("Year")+ ylab("NDVI % Relative Change per year over preceding 10 years")+ scale_color_manual(values=c("green", "blue","lightblue","grey","brown","yellow","orange"))+
  ggtitle( "Change in dry season NDVI over preceding 10 year period for 30m grid square \n with different fractional cover of Grass")+
  theme_fancy()+ theme(legend.position = c(0.1, 0.8))+ labs(color = "Grass cover") + scale_x_continuous(breaks=c(1990, 1995,2000, 2005,2010,2015,2020))
plot(PE) 

df3 <- df2 %>% mutate (Diff50 = df2$`50%`- df2$Area)
df3 <- df3 %>% mutate (Diff40 = df3$`40%`- df3$Area)
df3 <- df3 %>% mutate (Diff30 = df3$`30%`- df3$Area)
df3 <- df3 %>% mutate (Diff20 = df3$`20%`- df3$Area)
df3 <- df3 %>% mutate (Diff10 = df3$`10%`- df3$Area)
df3 <- df3 %>% mutate (Diff5 = df3$`5%`- df3$Area)


df3 <- df3%>%dplyr:: select (-c(2:8))


df5 <- melt(df3 ,  id.vars = 'year', variable.name = 'Type')

df5$year <-as.numeric(df5$year)
#df5$Type <- factor(df4$Type, levels = c('Diff60','Diff50','Diff40',"Diff30","Diff20","Diff10","Diff5"))



PF <- ggplot(df5, aes(x = year, y = value, color = Type)) +
  geom_line()+  xlab("Year")+ ylab("NDVI % Relative Change per year over preceding 10 years \n (-mean change NDVI for area) ")+ scale_color_manual(values=c("green", "black","blue","lightblue","grey","brown","yellow"))+
  ggtitle( "Change in dry season NDVI realtive to mean change over preceding 10 year period for 30m grid square \n with different fractional cover of Grass")+
  theme_fancy()+ theme(legend.position = c(0.1, 0.8))+ labs(color = "Grass cover") + scale_x_continuous(breaks=c(1990, 1995,2000, 2005,2010,2015,2020))
plot(PF) 







ggsave2(
  "E:/Glenn/Botswana/R_Scripts/slade-prosopis/output_data/Plots/30m_LST_ndvi_trend_Rolling_year_prosopis_all_classes.png",
  plot = PA,
  device = NULL,
  path = NULL,
  scale = 1,
  width = 170,
  height = 140,
  units =  "mm",
  dpi = 300,
  limitsize = TRUE
)

ggsave2(
  "E:/Glenn/Botswana/R_Scripts/slade-prosopis/output_data/Plots/30m_LST_ndvi_trend_Rolling_year_prosopis_compared_mean_all_classes.png",
  plot = PB,
  device = NULL,
  path = NULL,
  scale = 1,
  width = 170,
  height = 140,
  units =  "mm",
  dpi = 300,
  limitsize = TRUE
)


ggsave2(
  "E:/Glenn/Botswana/R_Scripts/slade-prosopis/output_data/Plots/30m_LST_ndvi_trend_Rolling_year_RT_all_classes.png",
  plot = PC,
  device = NULL,
  path = NULL,
  scale = 1,
  width = 170,
  height = 140,
  units =  "mm",
  dpi = 300,
  limitsize = TRUE
)

ggsave2(
  "E:/Glenn/Botswana/R_Scripts/slade-prosopis/output_data/Plots/30m_LST_ndvi_trend_Rolling_year_RT_compared_mean_all_classes.png",
  plot = PD,
  device = NULL,
  path = NULL,
  scale = 1,
  width = 170,
  height = 140,
  units =  "mm",
  dpi = 300,
  limitsize = TRUE
)


ggsave2(
  "E:/Glenn/Botswana/R_Scripts/slade-prosopis/output_data/Plots/30m_LST_ndvi_trend_Rolling_year_grass_all_classes.png",
  plot = PE,
  device = NULL,
  path = NULL,
  scale = 1,
  width = 170,
  height = 140,
  units =  "mm",
  dpi = 300,
  limitsize = TRUE
)

ggsave2(
  "E:/Glenn/Botswana/R_Scripts/slade-prosopis/output_data/Plots/30m_LST_ndvi_trend_Rolling_year_grass_compared_mean_all_classes.png",
  plot = PF,
  device = NULL,
  path = NULL,
  scale = 1,
  width = 170,
  height = 140,
  units =  "mm",
  dpi = 300,
  limitsize = TRUE
)
