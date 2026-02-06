### Script to Extract Data from RF Classifications based on WV2Scope ploygons
### matching pixels

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
library(xlsx)

#-------0. SET Parameters to use for training data set

#Fraction  cover to use to define training class 1.0 = 100%
#Change this to vary the pixel definitions
A = "0.95"

#Fraction  cover to use to define training class for Prosopis 1.0 = 100%
#Change this to vary the pixel definitions
P = "0.95"

#Number of pixels from each Class to use in training data

N = "250"

#----1. Read in files for WV2 ----


WV2_RF_WV2 <- rast("E:/Glenn/Botswana/Satellite_Data/WV2/1_6_m_mosaic/RF_WV2_additional.tif")
B1_Predict <-rast("E:/Glenn/Botswana/MLR3_classifications/Bokspits_1_Predict_Map.tif")
B2_Predict <- rast("E:/Glenn/Botswana/MLR3_classifications/Bokspits_2_Predict_Map (2).tif")
B3_Predict <- rast("E:/Glenn/Botswana/MLR3_classifications/Bokspits_3_Predict_Map (1).tif")
S1_Predict <- rast("E:/Glenn/Botswana/MLR3_classifications/Struizendam_1_Predict_Map (1).tif")
S2_Predict <- rast("E:/Glenn/Botswana/MLR3_classifications/Struizendam_2_Predict_Map (2).tif")
S3_Predict <- rast("E:/Glenn/Botswana/MLR3_classifications/Struizendam_3_Predict_Map.tif")
S4_Predict <- rast("E:/Glenn/Botswana/MLR3_classifications/Struizendam_4_Predict_Map (1).tif")



#NDVITrend<- rast("E:/Glenn/Botswana/Satellite_Data/Trend_Image_Data/WV2/Trend_Img_NDVI_2000_2022b.tif")
#WV2 <-terra::project(NDVITrend, y="EPSG:32734")


## NB You cant use exact extract with SpatVector so re-importing it a a spatial DF
#WV2WV_Grid_SF <- read_sf(dsn = 'E:/Glenn/Botswana/Satellite_Data/WV2/WV2_Grids', layer = "WV2_WV2_grid")
S1_Grid_SF <- read_sf(dsn = 'E:/Glenn/Botswana/Satellite_Data/WV2/WV2_Grids', layer = "Struizendam_1_WV2_grid")
S2_Grid_SF <- read_sf(dsn = 'E:/Glenn/Botswana/Satellite_Data/WV2/WV2_Grids', layer = "Struizendam_2_WV2_grid")
S3_Grid_SF <- read_sf(dsn = 'E:/Glenn/Botswana/Satellite_Data/WV2/WV2_Grids', layer = "Struizendam_3_WV2_grid")
S4_Grid_SF <- read_sf(dsn = 'E:/Glenn/Botswana/Satellite_Data/WV2/WV2_Grids', layer = "Struizendam_4_WV2_grid")
B1_Grid_SF <- read_sf(dsn = 'E:/Glenn/Botswana/Satellite_Data/WV2/WV2_Grids', layer = "Bokspits_1_WV2_grid")
B2_Grid_SF <- read_sf(dsn = 'E:/Glenn/Botswana/Satellite_Data/WV2/WV2_Grids', layer = "Bokspits_2_WV2_grid")
B3_Grid_SF <- read_sf(dsn = 'E:/Glenn/Botswana/Satellite_Data/WV2/WV2_Grids', layer = "Bokspits_3_WV2_grid")


#---2. Extract data for WV2 image from WV2 grid



#B1
B1_Predict_Extract <- exact_extract(B1_Predict,B1_Grid_SF,"mode")
names(B1_Predict_Extract) <- c('mode')
B1_Predict_Extract_DF <-bind_cols(B1_Grid_SF,B1_Predict_Extract)

B1_Predict_Extract <- exact_extract(B1_Predict,B1_Grid_SF,"majority" )
names(B1_Predict_Extract) <- c('majority')
B1_Predict_Extract_DF <- dplyr::mutate(B1_Predict_Extract_DF, majority = B1_Predict_Extract)

B1_Predict_Extract <- exact_extract(B1_Predict,B1_Grid_SF,"frac" )# calculates fraction cover for each classification value
#a column is added with the FVC for each vegetation type frac1, frac2 etc...
B1_Predict_Extract_DF <- dplyr::mutate(B1_Predict_Extract_DF, B1_Predict_Extract)

B1_full_Extract_DF <- B1_Predict_Extract_DF

B1_full_Extract_DF[is.na(B1_full_Extract_DF)] <- 0

Drone_extract_prosopis_cover_WV2Grid <- B1_full_Extract_DF%>% dplyr::select (majority,frac_1)

B2_Predict_Extract <- exact_extract(B2_Predict,B2_Grid_SF,"mode")
names(B2_Predict_Extract) <- c('mode')
B2_Predict_Extract_DF <-bind_cols(B2_Grid_SF,B2_Predict_Extract)

B2_Predict_Extract <- exact_extract(B2_Predict,B2_Grid_SF,"majority" )
names(B2_Predict_Extract) <- c('majority')
B2_Predict_Extract_DF <- dplyr::mutate(B2_Predict_Extract_DF, majority = B2_Predict_Extract)

B2_Predict_Extract <- exact_extract(B2_Predict,B2_Grid_SF,"frac" )# calculates fraction cover for each classification value
#a column is added with the FVC for each vegetation type frac1, frac2 etc...
B2_Predict_Extract_DF <- dplyr::mutate(B2_Predict_Extract_DF, B2_Predict_Extract)

B2_full_Extract_DF <- B2_Predict_Extract_DF

B2_full_Extract_DF[is.na(B2_full_Extract_DF)] <- 0

Temp <- B2_full_Extract_DF%>% dplyr::select (majority,frac_1)
Drone_extract_prosopis_cover_WV2Grid <- rbind(Temp,Drone_extract_prosopis_cover_WV2Grid)
B3_Predict_Extract <- exact_extract(B3_Predict,B3_Grid_SF,"mode")
names(B3_Predict_Extract) <- c('mode')
B3_Predict_Extract_DF <-bind_cols(B3_Grid_SF,B3_Predict_Extract)

B3_Predict_Extract <- exact_extract(B3_Predict,B3_Grid_SF,"majority" )
names(B3_Predict_Extract) <- c('majority')
B3_Predict_Extract_DF <- dplyr::mutate(B3_Predict_Extract_DF, majority = B3_Predict_Extract)

B3_Predict_Extract <- exact_extract(B3_Predict,B3_Grid_SF,"frac" )# calculates fraction cover for each classification value
#a column is added with the FVC for each vegetation type frac1, frac2 etc...
B3_Predict_Extract_DF <- dplyr::mutate(B3_Predict_Extract_DF, B3_Predict_Extract)

B3_full_Extract_DF <- B3_Predict_Extract_DF

B3_full_Extract_DF[is.na(B3_full_Extract_DF)] <- 0

Temp <- B3_full_Extract_DF%>% dplyr::select (majority,frac_1)
Drone_extract_prosopis_cover_WV2Grid <- rbind(Temp,Drone_extract_prosopis_cover_WV2Grid)

S1_Predict_Extract <- exact_extract(S1_Predict,S1_Grid_SF,"mode")
names(S1_Predict_Extract) <- c('mode')
S1_Predict_Extract_DF <-bind_cols(S1_Grid_SF,S1_Predict_Extract)

S1_Predict_Extract <- exact_extract(S1_Predict,S1_Grid_SF,"majority" )
names(S1_Predict_Extract) <- c('majority')
S1_Predict_Extract_DF <- dplyr::mutate(S1_Predict_Extract_DF, majority = S1_Predict_Extract)

S1_Predict_Extract <- exact_extract(S1_Predict,S1_Grid_SF,"frac" )# calculates fraction cover for each classification value
#a column is added with the FVC for each vegetation type frac1, frac2 etc...
S1_Predict_Extract_DF <- dplyr::mutate(S1_Predict_Extract_DF, S1_Predict_Extract)

S1_full_Extract_DF <- S1_Predict_Extract_DF

S1_full_Extract_DF[is.na(S1_full_Extract_DF)] <- 0

Temp <- S1_full_Extract_DF%>% dplyr::select (majority,frac_1)
Drone_extract_prosopis_cover_WV2Grid <- rbind(Temp,Drone_extract_prosopis_cover_WV2Grid)

# S3_Predict_Extract <- exact_extract(S3_Predict,S3_Grid_SF,"mode")
# names(S3_Predict_Extract) <- c('mode')
# S3_Predict_Extract_DF <-bind_cols(S3_Grid_SF,S3_Predict_Extract)
# 
# S3_Predict_Extract <- exact_extract(S3_Predict,S3_Grid_SF,"majority" )
# names(S3_Predict_Extract) <- c('majority')
# S3_Predict_Extract_DF <- dplyr::mutate(S3_Predict_Extract_DF, majority = S3_Predict_Extract)
# 
# S3_Predict_Extract <- exact_extract(S3_Predict,S3_Grid_SF,"frac" )# calculates fraction cover for each classification value
# #a column is added with the FVC for each vegetation type frac1, frac2 etc...
# S3_Predict_Extract_DF <- dplyr::mutate(S3_Predict_Extract_DF, S3_Predict_Extract)
# 
# S3_full_Extract_DF <- S3_Predict_Extract_DF
# 
# S3_full_Extract_DF[is.na(S3_full_Extract_DF)] <- 0
# 
# Temp <- S3_full_Extract_DF%>% dplyr::select (majority,frac_1)
# Drone_extract_prosopis_cover_WV2Grid <- rbind(Temp,Drone_extract_prosopis_cover_WV2Grid)

S4_Predict_Extract <- exact_extract(S4_Predict,S4_Grid_SF,"mode")
names(S4_Predict_Extract) <- c('mode')
S4_Predict_Extract_DF <-bind_cols(S4_Grid_SF,S4_Predict_Extract)

S4_Predict_Extract <- exact_extract(S4_Predict,S4_Grid_SF,"majority" )
names(S4_Predict_Extract) <- c('majority')
S4_Predict_Extract_DF <- dplyr::mutate(S4_Predict_Extract_DF, majority = S4_Predict_Extract)

S4_Predict_Extract <- exact_extract(S4_Predict,S4_Grid_SF,"frac" )# calculates fraction cover for each classification value
#a column is added with the FVC for each vegetation type frac1, frac2 etc...
S4_Predict_Extract_DF <- dplyr::mutate(S4_Predict_Extract_DF, S4_Predict_Extract)

S4_full_Extract_DF <- S4_Predict_Extract_DF

S4_full_Extract_DF[is.na(S4_full_Extract_DF)] <- 0

Temp <- S4_full_Extract_DF%>% dplyr::select (majority,frac_1)
Drone_extract_prosopis_cover_WV2Grid <- rbind(Temp,Drone_extract_prosopis_cover_WV2Grid)


write_xlsx(Drone_extract_prosopis_cover_WV2Grid, "E:/Glenn/Botswana/R_Scripts/Glenn-Prosopis-ML/data_out/Drone_extract_prosopis_cover_WV2Grid_6.xlsx")

