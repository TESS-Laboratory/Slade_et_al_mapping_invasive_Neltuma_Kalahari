### Script to Extract Data from RF Classifications based on LSTScope ploygons
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


WV2_RF_S2 <- rast("E:/Glenn/Botswana/Satellite_Data/WV2/1_6_m_mosaic/RF_WV2_additional.tif")
#NDVITrend<- rast("E:/Glenn/Botswana/Satellite_Data/Trend_Image_Data/S2/Trend_Img_NDVI_2000_2022b.tif")
#LST <-terra::project(NDVITrend, y="EPSG:32734")


## NB You cant use exact extract with SpatVector so re-importing it a a spatial DF
S2_Grid_SF <- read_sf(dsn = 'E:/Glenn/Botswana/Satellite_Data/S2/S2_Grids', layer = "WV2_S2_grid")


#---2. Extract data for WV2 image


WV2_RF_S2_Extract <- exact_extract(WV2_RF_S2,S2_Grid_SF,"mode")
names(WV2_RF_S2_Extract) <- c('mode')
WV2_RF_S2_Extract_DF <-bind_cols(S2_Grid_SF,WV2_RF_S2_Extract)

WV2_RF_S2_Extract <- exact_extract(WV2_RF_S2,S2_Grid_SF,"majority" )
names(WV2_RF_S2_Extract) <- c('majority')
WV2_RF_S2_Extract_DF <- dplyr::mutate(WV2_RF_S2_Extract_DF, majority = WV2_RF_S2_Extract)

WV2_RF_S2_Extract <- exact_extract(WV2_RF_S2,S2_Grid_SF,"frac" )# calculates fraction cover for each classification value
#a column is added with the FVC for each vegetation type frac1, frac2 etc...
WV2_RF_S2_Extract_DF <- dplyr::mutate(WV2_RF_S2_Extract_DF, WV2_RF_S2_Extract)

WV2_full_Extract_DF <- WV2_RF_S2_Extract_DF
# make useful data frame of extarcted data 95 % cover data only


WV2_full_Extract_DF[is.na(WV2_full_Extract_DF)] <- 0

#----16.  Select Parameters for defining training 

#Need to define all frac_ in final version

WV2_full_Extract_DF_95<- WV2_full_Extract_DF %>% filter_at(vars(frac_1,frac_2,frac_3,frac_5,frac_6,frac_7,frac_10,frac_53), any_vars(. > paste0(P)))
#WV2_full_Extract_DF_P <- WV2_full_WV2_Extract_DF %>% filter_at(vars(frac_1), any_vars(. > paste0(P)))


saveRDS(WV2_full_Extract_DF_95, "E:/Glenn/Botswana/R_Scripts/Glenn-Prosopis-ML/data_in/S2_full_train.rds")
st_write(WV2_full_Extract_DF_95, dsn = "E:/Glenn/Botswana/R_Scripts/Glenn-Prosopis-ML/data_in/S2_full_extract_95.shp")

#WV2_full_Extract_DF_95<- Bokspits_1_WV2_Extract_DF %>% filter_at(vars(frac_2,frac_3,frac_4,frac_5,frac_6), any_vars(. > paste0(A)))
#WV2_full_Extract_DF_P <- Bokspits_1_WV2_Extract_DF %>% filter_at(vars(frac_1), any_vars(. > paste0(P)))

#N = "500"

WV2_full_Extract_DF_95_1 <- WV2_full_Extract_DF_95 %>% filter (frac_1 > 0.95)
WV2_full_Extract_DF_95_2 <- WV2_full_Extract_DF_95 %>% filter (frac_2 > 0.95)
WV2_full_Extract_DF_95_3 <- WV2_full_Extract_DF_95 %>% filter (frac_3 > 0.95)
WV2_full_Extract_DF_95_5 <- WV2_full_Extract_DF_95 %>% filter (frac_5 > 0.95)
WV2_full_Extract_DF_95_6 <- WV2_full_Extract_DF_95 %>% filter (frac_6 > 0.95)
WV2_full_Extract_DF_95_7 <- WV2_full_Extract_DF_95 %>% filter (frac_7 > 0.95)
WV2_full_Extract_DF_95_53 <- WV2_full_Extract_DF_95 %>% filter (frac_53 > 0.95)
WV2_full_Extract_DF_95_10 <- WV2_full_Extract_DF_95 %>% filter (frac_10 > 0.95)

WV2_full_train <- bind_rows(WV2_full_Extract_DF_95_1,WV2_full_Extract_DF_95_2,WV2_full_Extract_DF_95_3,WV2_full_Extract_DF_95_5,WV2_full_Extract_DF_95_6,WV2_full_Extract_DF_95_7)

saveRDS(WV2_full_train, "E:/Glenn/Botswana/R_Scripts/Glenn-Prosopis-ML/data_in/S2_pixel_extract_full_train.rds")
st_write(WV2_full_train, dsn = "E:/Glenn/Botswana/R_Scripts/Glenn-Prosopis-ML/data_in/S2_pixel_extract_full_train.shp")




WV2_full_Extract_DF_95_1e <-WV2_full_Extract_DF_95_1 [ sample( which( WV2_full_Extract_DF_95_1$majority == "1" ) , 10 ) , ]
WV2_full_Extract_DF_95_2e <-WV2_full_Extract_DF_95_2[ sample( which( WV2_full_Extract_DF_95_2$majority == "2" ) , 10) , ]
WV2_full_Extract_DF_95_3e <-WV2_full_Extract_DF_95_3[ sample( which( WV2_full_Extract_DF_95_3$majority == "3" ) , 10) , ]
WV2_full_Extract_DF_95_5e <-WV2_full_Extract_DF_95_5[ sample( which( WV2_full_Extract_DF_95_5$majority == "5" ) , 8) , ]
WV2_full_Extract_DF_95_6e <-WV2_full_Extract_DF_95_6[ sample( which( WV2_full_Extract_DF_95_6$majority == "6" ) , 10 ), ]
WV2_full_Extract_DF_95_7e <-WV2_full_Extract_DF_95_7[ sample( which( WV2_full_Extract_DF_95_7$majority == "7" ) , 10) , ]
WV2_full_Extract_DF_95_53e <-WV2_full_Extract_DF_95_53[ sample( which( WV2_full_Extract_DF_95_53$majority == "53" ) , 10) , ]

WV2_full_traine <- bind_rows(WV2_full_Extract_DF_95_1e,WV2_full_Extract_DF_95_2e,WV2_full_Extract_DF_95_3e,WV2_full_Extract_DF_95_5e,WV2_full_Extract_DF_95_6e,WV2_full_Extract_DF_95_7e,WV2_full_Extract_DF_95_53e)


saveRDS(WV2_full_traine, "E:/Glenn/Botswana/R_Scripts/Glenn-Prosopis-ML/data_in/S2_equal_class_size_10_train.rds")
st_write(WV2_full_traine, dsn = "E:/Glenn/Botswana/R_Scripts/Glenn-Prosopis-ML/data_in/S2_equal_class_size_10_train.shp")


