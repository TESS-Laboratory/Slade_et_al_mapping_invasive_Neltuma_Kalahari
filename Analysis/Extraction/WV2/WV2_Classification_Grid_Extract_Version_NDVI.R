### Script to Extract Data from WV2 Classifications based on WV2 pixel grid
### matching pixels
### Imports classified drone image data set and calculates fractional cover for each 
### class in each WV2 pixel and then samples that data to produce training data set
### for classification of WV2 image.

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
library(cowplot)

#-------0. SET Parameters to use for training data set

#Fraction  cover to use to define training class 1.0 = 100%
#Change this to vary the pixel definitions
A = "0.95"

#Fraction  cover to use to define training class for Prosopis 1.0 = 100%
#Change this to vary the pixel definitions
P = "0.95"

#Number of pixels from each Class to use in training data

N = "500"

W <- rast("E:/Glenn/Botswana/R_Scripts/Glenn-Prosopis-ML/data_in/WV2/WV2_NDVI.tif")

#----1. Read in files for Bokspits_1 etc Classified image data ----


Bokspits_1_Predict <- rast("E:/Glenn/Botswana/R_Scripts/Glenn-Prosopis-ML/data/svm_pixel_level_Bokspits_1_stack_5_CHM_ALLVI.tif")
Bokspits_1_Predict 
B1 <- rast("E:/Glenn/Botswana/R_Scripts/Glenn-Prosopis-ML/data_in/Bokspits_1/Bokspits_1_NDVI.tif")

## NB You cant use exact extract with SpatVector so importing it a a spatial DF
Bokspits_1_WV2_grid_SF <- read_sf(dsn = 'E:/Glenn/Botswana/R_Scripts/Glenn-Prosopis-ML/data_in/WV2_Grids', layer = "Bokspits_1_WV2_grid")
Bokspits_1_WV2_grid_SF


#---2. Extract data for Bokspits_1 Grid


Bokspits_1_WV2_Extract <- exact_extract(Bokspits_1_Predict,Bokspits_1_WV2_grid_SF,"mode")
names(Bokspits_1_WV2_Extract) <- c('mode')
Bokspits_1_WV2_Extract_DF <-bind_cols(Bokspits_1_WV2_grid_SF,Bokspits_1_WV2_Extract)

Bokspits_1_WV2_Extract <- exact_extract(Bokspits_1_Predict,Bokspits_1_WV2_grid_SF,"majority" )
names(Bokspits_1_WV2_Extract) <- c('majority')
Bokspits_1_WV2_Extract_DF <- dplyr::mutate(Bokspits_1_WV2_Extract_DF, majority = Bokspits_1_WV2_Extract)

Bokspits_1_WV2_Extract <- exact_extract(Bokspits_1_Predict,Bokspits_1_WV2_grid_SF,"frac" )# calculates fraction cover for each classification value
#a column is added with the FVC for each vegetation type frac1, frac2 etc...
Bokspits_1_WV2_Extract_DF <- dplyr::mutate(Bokspits_1_WV2_Extract_DF, Bokspits_1_WV2_Extract)

Bokspits_1_WV2_Extract <- exact_extract(B1,Bokspits_1_WV2_grid_SF,"mean" )
names(Bokspits_1_WV2_Extract) <- c('NDVI')
Bokspits_1_WV2_Extract_DF <- dplyr::mutate(Bokspits_1_WV2_Extract_DF, NDVI = Bokspits_1_WV2_Extract)

Bokspits_1_WV2_Extract <- exact_extract(W,Bokspits_1_WV2_grid_SF,"mean" )
names(Bokspits_1_WV2_Extract) <- c('WV2_NDVI')
Bokspits_1_WV2_Extract_DF <- dplyr::mutate(Bokspits_1_WV2_Extract_DF, WV2_NDVI = Bokspits_1_WV2_Extract)


#----3. Read in files for Bokspits_2 etc Classified image data ----


Bokspits_2_Predict <- rast("E:/Glenn/Botswana/R_Scripts/Glenn-Prosopis-ML/data/svm_pixel_level_Bokspits_2_stack_5_CHM_ALLVI.tif")
Bokspits_2_Predict 
B2 <- rast("E:/Glenn/Botswana/R_Scripts/Glenn-Prosopis-ML/data_in/Bokspits_2/Bokspits_2_NDVI.tif")

## NB You cant use exact extract with SpatVector so importing it a a spatial DF
Bokspits_2_WV2_grid_SF <- read_sf(dsn = 'E:/Glenn/Botswana/R_Scripts/Glenn-Prosopis-ML/data_in/WV2_Grids', layer = "Bokspits_2_WV2_grid")
Bokspits_2_WV2_grid_SF


#---2. Extract data for Bokspits_2 Grid


Bokspits_2_WV2_Extract <- exact_extract(Bokspits_2_Predict,Bokspits_2_WV2_grid_SF,"mode")
names(Bokspits_2_WV2_Extract) <- c('mode')
Bokspits_2_WV2_Extract_DF <-bind_cols(Bokspits_2_WV2_grid_SF,Bokspits_2_WV2_Extract)

Bokspits_2_WV2_Extract <- exact_extract(Bokspits_2_Predict,Bokspits_2_WV2_grid_SF,"majority" )
names(Bokspits_2_WV2_Extract) <- c('majority')
Bokspits_2_WV2_Extract_DF <- dplyr::mutate(Bokspits_2_WV2_Extract_DF, majority = Bokspits_2_WV2_Extract)

Bokspits_2_WV2_Extract <- exact_extract(Bokspits_2_Predict,Bokspits_2_WV2_grid_SF,"frac" )# calculates fraction cover for each classification value
#a column is added with the FVC for each vegetation type frac1, frac2 etc...
Bokspits_2_WV2_Extract_DF <- dplyr::mutate(Bokspits_2_WV2_Extract_DF, Bokspits_2_WV2_Extract)

Bokspits_2_WV2_Extract <- exact_extract(B2,Bokspits_2_WV2_grid_SF,"mean" )
names(Bokspits_2_WV2_Extract) <- c('NDVI')
Bokspits_2_WV2_Extract_DF <- dplyr::mutate(Bokspits_2_WV2_Extract_DF, NDVI = Bokspits_2_WV2_Extract)

Bokspits_2_WV2_Extract <- exact_extract(W,Bokspits_2_WV2_grid_SF,"mean" )
names(Bokspits_2_WV2_Extract) <- c('WV2_NDVI')
Bokspits_2_WV2_Extract_DF <- dplyr::mutate(Bokspits_2_WV2_Extract_DF, WV2_NDVI = Bokspits_2_WV2_Extract)
#----5. Read in files for Bokspits_3 etc Classified image data ----


Bokspits_3_Predict <- rast("E:/Glenn/Botswana/R_Scripts/Glenn-Prosopis-ML/data/svm_pixel_level_Bokspits_3_stack_5_CHM_ALLVI.tif")
Bokspits_3_Predict 
B3 <- rast("E:/Glenn/Botswana/R_Scripts/Glenn-Prosopis-ML/data_in/Bokspits_3/Bokspits_3_NDVI.tif")

## NB You cant use exact extract with SpatVector so importing it a a spatial DF
Bokspits_3_WV2_grid_SF <- read_sf(dsn = 'E:/Glenn/Botswana/R_Scripts/Glenn-Prosopis-ML/data_in/WV2_Grids', layer = "Bokspits_3_WV2_grid")
Bokspits_3_WV2_grid_SF


#---2. Extract data for Bokspits_3 Grid


Bokspits_3_WV2_Extract <- exact_extract(Bokspits_3_Predict,Bokspits_3_WV2_grid_SF,"mode")
names(Bokspits_3_WV2_Extract) <- c('mode')
Bokspits_3_WV2_Extract_DF <-bind_cols(Bokspits_3_WV2_grid_SF,Bokspits_3_WV2_Extract)

Bokspits_3_WV2_Extract <- exact_extract(Bokspits_3_Predict,Bokspits_3_WV2_grid_SF,"majority" )
names(Bokspits_3_WV2_Extract) <- c('majority')
Bokspits_3_WV2_Extract_DF <- dplyr::mutate(Bokspits_3_WV2_Extract_DF, majority = Bokspits_3_WV2_Extract)

Bokspits_3_WV2_Extract <- exact_extract(Bokspits_3_Predict,Bokspits_3_WV2_grid_SF,"frac" )# calculates fraction cover for each classification value
#a column is added with the FVC for each vegetation type frac1, frac2 etc...
Bokspits_3_WV2_Extract_DF <- dplyr::mutate(Bokspits_3_WV2_Extract_DF, Bokspits_3_WV2_Extract)

Bokspits_3_WV2_Extract <- exact_extract(B3,Bokspits_3_WV2_grid_SF,"mean" )
names(Bokspits_3_WV2_Extract) <- c('NDVI')
Bokspits_3_WV2_Extract_DF <- dplyr::mutate(Bokspits_3_WV2_Extract_DF, NDVI = Bokspits_3_WV2_Extract)

Bokspits_3_WV2_Extract <- exact_extract(W,Bokspits_3_WV2_grid_SF,"mean" )
names(Bokspits_3_WV2_Extract) <- c('WV2_NDVI')
Bokspits_3_WV2_Extract_DF <- dplyr::mutate(Bokspits_3_WV2_Extract_DF, WV2_NDVI = Bokspits_3_WV2_Extract)


#----7. Read in files for Struizendam_1 etc Classified image data ----


Struizendam_1_Predict <- rast("E:/Glenn/Botswana/R_Scripts/Glenn-Prosopis-ML/data/svm_pixel_level_Struizendam_1_stack_5_CHM_ALLVI.tif")
Struizendam_1_Predict 
S1 <- rast("E:/Glenn/Botswana/R_Scripts/Glenn-Prosopis-ML/data_in/Struizendam_1/Struizendam_1_NDVI.tif")

## NB You cant use exact extract with SpatVector so importing it a a spatial DF
Struizendam_1_WV2_grid_SF <- read_sf(dsn = 'E:/Glenn/Botswana/R_Scripts/Glenn-Prosopis-ML/data_in/WV2_Grids', layer = "Struizendam_1_WV2_grid")
Struizendam_1_WV2_grid_SF


#---2. Extract data for Struizendam_1 Grid


Struizendam_1_WV2_Extract <- exact_extract(Struizendam_1_Predict,Struizendam_1_WV2_grid_SF,"mode")
names(Struizendam_1_WV2_Extract) <- c('mode')
Struizendam_1_WV2_Extract_DF <-bind_cols(Struizendam_1_WV2_grid_SF,Struizendam_1_WV2_Extract)

Struizendam_1_WV2_Extract <- exact_extract(Struizendam_1_Predict,Struizendam_1_WV2_grid_SF,"majority" )
names(Struizendam_1_WV2_Extract) <- c('majority')
Struizendam_1_WV2_Extract_DF <- dplyr::mutate(Struizendam_1_WV2_Extract_DF, majority = Struizendam_1_WV2_Extract)

Struizendam_1_WV2_Extract <- exact_extract(Struizendam_1_Predict,Struizendam_1_WV2_grid_SF,"frac" )# calculates fraction cover for each classification value
#a column is added with the FVC for each vegetation type frac1, frac2 etc...
Struizendam_1_WV2_Extract_DF <- dplyr::mutate(Struizendam_1_WV2_Extract_DF, Struizendam_1_WV2_Extract)

Struizendam_1_WV2_Extract <- exact_extract(S1,Struizendam_1_WV2_grid_SF,"mean" )
names(Struizendam_1_WV2_Extract) <- c('NDVI')
Struizendam_1_WV2_Extract_DF <- dplyr::mutate(Struizendam_1_WV2_Extract_DF, NDVI = Struizendam_1_WV2_Extract)

Struizendam_1_WV2_Extract <- exact_extract(W,Struizendam_1_WV2_grid_SF,"mean" )
names(Struizendam_1_WV2_Extract) <- c('WV2_NDVI')
Struizendam_1_WV2_Extract_DF <- dplyr::mutate(Struizendam_1_WV2_Extract_DF, WV2_NDVI = Struizendam_1_WV2_Extract)

#----9. Read in files for Struizendam_2 etc Classified image data ----
Struizendam_2_Predict <- rast("E:/Glenn/Botswana/R_Scripts/Glenn-Prosopis-ML/data/svm_pixel_level_Struizendam_2_stack_5_CHM_ALLVI.tif")
Struizendam_2_Predict 
S2 <- rast("E:/Glenn/Botswana/R_Scripts/Glenn-Prosopis-ML/data_in/Struizendam_2/Struizendam_2_NDVI.tif")

## NB You cant use exact extract with SpatVector so importing it a a spatial DF
Struizendam_2_WV2_grid_SF <- read_sf(dsn = 'E:/Glenn/Botswana/R_Scripts/Glenn-Prosopis-ML/data_in/WV2_Grids', layer = "Struizendam_2_WV2_grid")
Struizendam_2_WV2_grid_SF


#---2. Extract data for Struizendam_2 Grid


Struizendam_2_WV2_Extract <- exact_extract(Struizendam_2_Predict,Struizendam_2_WV2_grid_SF,"mode")
names(Struizendam_2_WV2_Extract) <- c('mode')
Struizendam_2_WV2_Extract_DF <-bind_cols(Struizendam_2_WV2_grid_SF,Struizendam_2_WV2_Extract)

Struizendam_2_WV2_Extract <- exact_extract(Struizendam_2_Predict,Struizendam_2_WV2_grid_SF,"majority" )
names(Struizendam_2_WV2_Extract) <- c('majority')
Struizendam_2_WV2_Extract_DF <- dplyr::mutate(Struizendam_2_WV2_Extract_DF, majority = Struizendam_2_WV2_Extract)

Struizendam_2_WV2_Extract <- exact_extract(Struizendam_2_Predict,Struizendam_2_WV2_grid_SF,"frac" )# calculates fraction cover for each classification value
#a column is added with the FVC for each vegetation type frac1, frac2 etc...
Struizendam_2_WV2_Extract_DF <- dplyr::mutate(Struizendam_2_WV2_Extract_DF, Struizendam_2_WV2_Extract)

Struizendam_2_WV2_Extract <- exact_extract(S2,Struizendam_2_WV2_grid_SF,"mean" )
names(Struizendam_2_WV2_Extract) <- c('NDVI')
Struizendam_2_WV2_Extract_DF <- dplyr::mutate(Struizendam_2_WV2_Extract_DF, NDVI = Struizendam_2_WV2_Extract)

Struizendam_2_WV2_Extract <- exact_extract(W,Struizendam_2_WV2_grid_SF,"mean" )
names(Struizendam_2_WV2_Extract) <- c('WV2_NDVI')
Struizendam_2_WV2_Extract_DF <- dplyr::mutate(Struizendam_2_WV2_Extract_DF, WV2_NDVI = Struizendam_2_WV2_Extract)

#----11. Read in files for Struizendam_3 etc Classified image data ----

Struizendam_3_Predict <- rast("E:/Glenn/Botswana/R_Scripts/Glenn-Prosopis-ML/data/svm_pixel_level_Struizendam_3_stack_5_CHM_ALLVI.tif")
Struizendam_3_Predict 
S3 <- rast("E:/Glenn/Botswana/R_Scripts/Glenn-Prosopis-ML/data_in/Struizendam_3/Struizendam_3_NDVI.tif")

## NB You cant use exact extract with SpatVector so importing it a a spatial DF
Struizendam_3_WV2_grid_SF <- read_sf(dsn = 'E:/Glenn/Botswana/R_Scripts/Glenn-Prosopis-ML/data_in/WV2_Grids', layer = "Struizendam_3_WV2_grid")
Struizendam_3_WV2_grid_SF


#---2. Extract data for Struizendam_3 Grid


Struizendam_3_WV2_Extract <- exact_extract(Struizendam_3_Predict,Struizendam_3_WV2_grid_SF,"mode")
names(Struizendam_3_WV2_Extract) <- c('mode')
Struizendam_3_WV2_Extract_DF <-bind_cols(Struizendam_3_WV2_grid_SF,Struizendam_3_WV2_Extract)

Struizendam_3_WV2_Extract <- exact_extract(Struizendam_3_Predict,Struizendam_3_WV2_grid_SF,"majority" )
names(Struizendam_3_WV2_Extract) <- c('majority')
Struizendam_3_WV2_Extract_DF <- dplyr::mutate(Struizendam_3_WV2_Extract_DF, majority = Struizendam_3_WV2_Extract)

Struizendam_3_WV2_Extract <- exact_extract(Struizendam_3_Predict,Struizendam_3_WV2_grid_SF,"frac" )# calculates fraction cover for each classification value
#a column is added with the FVC for each vegetation type frac1, frac2 etc...
Struizendam_3_WV2_Extract_DF <- dplyr::mutate(Struizendam_3_WV2_Extract_DF, Struizendam_3_WV2_Extract)

Struizendam_3_WV2_Extract <- exact_extract(S3,Struizendam_3_WV2_grid_SF,"mean" )
names(Struizendam_3_WV2_Extract) <- c('NDVI')
Struizendam_3_WV2_Extract_DF <- dplyr::mutate(Struizendam_3_WV2_Extract_DF, NDVI = Struizendam_3_WV2_Extract)

Struizendam_3_WV2_Extract <- exact_extract(W,Struizendam_3_WV2_grid_SF,"mean" )
names(Struizendam_3_WV2_Extract) <- c('WV2_NDVI')
Struizendam_3_WV2_Extract_DF <- dplyr::mutate(Struizendam_3_WV2_Extract_DF, WV2_NDVI = Struizendam_3_WV2_Extract)

#----13. Read in files for Struizendam_4 etc Classified image data ----

Struizendam_4_Predict <- rast("E:/Glenn/Botswana/R_Scripts/Glenn-Prosopis-ML/data/svm_pixel_level_Struizendam_4_stack_5_CHM_ALLVI.tif")
Struizendam_4_Predict 
S4 <- rast("E:/Glenn/Botswana/R_Scripts/Glenn-Prosopis-ML/data_in/Struizendam_4/Struizendam_4_NDVI.tif")

## NB You cant use exact extract with SpatVector so importing it a a spatial DF
Struizendam_4_WV2_grid_SF <- read_sf(dsn = 'E:/Glenn/Botswana/R_Scripts/Glenn-Prosopis-ML/data_in/WV2_Grids', layer = "Struizendam_4_WV2_grid")
Struizendam_4_WV2_grid_SF


#---2. Extract data for Struizendam_4 Grid


Struizendam_4_WV2_Extract <- exact_extract(Struizendam_4_Predict,Struizendam_4_WV2_grid_SF,"mode")
names(Struizendam_4_WV2_Extract) <- c('mode')
Struizendam_4_WV2_Extract_DF <-bind_cols(Struizendam_4_WV2_grid_SF,Struizendam_4_WV2_Extract)

Struizendam_4_WV2_Extract <- exact_extract(Struizendam_4_Predict,Struizendam_4_WV2_grid_SF,"majority" )
names(Struizendam_4_WV2_Extract) <- c('majority')
Struizendam_4_WV2_Extract_DF <- dplyr::mutate(Struizendam_4_WV2_Extract_DF, majority = Struizendam_4_WV2_Extract)

Struizendam_4_WV2_Extract <- exact_extract(Struizendam_4_Predict,Struizendam_4_WV2_grid_SF,"frac" )# calculates fraction cover for each classification value
#a column is added with the FVC for each vegetation type frac1, frac2 etc...
Struizendam_4_WV2_Extract_DF <- dplyr::mutate(Struizendam_4_WV2_Extract_DF, Struizendam_4_WV2_Extract)

Struizendam_4_WV2_Extract <- exact_extract(S4,Struizendam_4_WV2_grid_SF,"mean" )
names(Struizendam_4_WV2_Extract) <- c('NDVI')
Struizendam_4_WV2_Extract_DF <- dplyr::mutate(Struizendam_4_WV2_Extract_DF, NDVI = Struizendam_4_WV2_Extract)

Struizendam_4_WV2_Extract <- exact_extract(W,Struizendam_4_WV2_grid_SF,"mean" )
names(Struizendam_4_WV2_Extract) <- c('WV2_NDVI')
Struizendam_4_WV2_Extract_DF <- dplyr::mutate(Struizendam_4_WV2_Extract_DF, WV2_NDVI = Struizendam_4_WV2_Extract)
#-----15. Combine data frames from each drone survey into one training data set for all survey areas

WV2_full_Extract_DF <- bind_rows(Bokspits_1_WV2_Extract_DF, Bokspits_2_WV2_Extract_DF,Bokspits_3_WV2_Extract_DF, Struizendam_1_WV2_Extract_DF, Struizendam_2_WV2_Extract_DF,Struizendam_3_WV2_Extract_DF,Struizendam_4_WV2_Extract_DF)
#need to replace NA with zeros

WV2_full_Extract_DF[is.na(WV2_full_Extract_DF)] <- 0

#----16.  Select Parameters for defining training 

#Need to define all frac_ in final version

WV2_full_Extract_DF_95<- WV2_full_Extract_DF #%>% filter_at(vars(frac_1,frac_2,frac_3,frac_5,frac_6,frac_7,frac_10), any_vars(. > 0.95))
#WV2_full_Extract_DF_P <- WV2_full_WV2_Extract_DF %>% filter_at(vars(frac_1), any_vars(. > paste0(P)))


WV2_full_Extract_DF_95_1 <- WV2_full_Extract_DF_95 %>% filter (frac_1 > 0.95)
WV2_full_Extract_DF_95_2 <- WV2_full_Extract_DF_95 %>% filter (frac_2 > 0.95)
WV2_full_Extract_DF_95_3 <- WV2_full_Extract_DF_95 %>% filter (frac_3 > 0.95)
WV2_full_Extract_DF_95_5 <- WV2_full_Extract_DF_95 %>% filter (frac_5 > 0.95)
WV2_full_Extract_DF_95_6 <- WV2_full_Extract_DF_95 %>% filter (frac_6 > 0.95)
WV2_full_Extract_DF_95_7 <- WV2_full_Extract_DF_95 %>% filter (frac_7 > 0.85)
WV2_full_Extract_DF_95_10 <- WV2_full_Extract_DF_95 %>% filter (frac_10 > 0.95)

WV2_full_train <- bind_rows(WV2_full_Extract_DF_95_1,WV2_full_Extract_DF_95_2,WV2_full_Extract_DF_95_3,WV2_full_Extract_DF_95_5,WV2_full_Extract_DF_95_6,WV2_full_Extract_DF_95_7)

saveRDS(WV2_full_train, "E:/Glenn/Botswana/R_Scripts/Glenn-Prosopis-ML/data_in/WV2_pixel_extract_full_train_ndvi.rds")
st_write(WV2_full_train, dsn = "E:/Glenn/Botswana/R_Scripts/Glenn-Prosopis-ML/data_in/WV2_pixel_extract_full_train_ndvi.shp")


WV2_full_Extract_DF_95_1e <-WV2_full_Extract_DF_95_1 [ sample( which( WV2_full_Extract_DF_95_1$majority == "1" ) ,280 ) , ]
WV2_full_Extract_DF_95_2e <-WV2_full_Extract_DF_95_2[ sample( which( WV2_full_Extract_DF_95_2$majority == "2" ) ,280) , ]
WV2_full_Extract_DF_95_3e <-WV2_full_Extract_DF_95_3[ sample( which( WV2_full_Extract_DF_95_3$majority == "3" ) ,280) , ]
WV2_full_Extract_DF_95_5e <-WV2_full_Extract_DF_95_5[ sample( which( WV2_full_Extract_DF_95_5$majority == "5" ) , 280) , ]
WV2_full_Extract_DF_95_6e <-WV2_full_Extract_DF_95_6[ sample( which( WV2_full_Extract_DF_95_6$majority == "6" ) ,280 ), ]
WV2_full_Extract_DF_95_7e <-WV2_full_Extract_DF_95_7[ sample( which( WV2_full_Extract_DF_95_7$majority == "7" ) ,280) , ]

WV2_full_traine <- bind_rows(WV2_full_Extract_DF_95_1e,WV2_full_Extract_DF_95_2e,WV2_full_Extract_DF_95_3e,WV2_full_Extract_DF_95_5e,WV2_full_Extract_DF_95_6e,WV2_full_Extract_DF_95_7e)


saveRDS(WV2_full_traine, "E:/Glenn/Botswana/R_Scripts/Glenn-Prosopis-ML/data_in/WV2_equal_class_size_280_train_ndvi.rds")
st_write(WV2_full_traine, dsn = "E:/Glenn/Botswana/R_Scripts/Glenn-Prosopis-ML/data_in/WV2_equal_class_size_280_train_ndvi.shp")





#---3. Plot comparison of drone and WV2 data----

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


p2_NDVI <- ggplot(WV2_full_traine,aes (NDVI,WV2_NDVI, colour=factor(majority))) +
  geom_point() + geom_abline(intercept = 0, slope = 1, col='grey' ) +scale_color_manual(name = "Vegetation Type", labels = c("Prosopis", "Bare sand","Grass","Vachellia erioloba","Rhigosum trichotomum","Senegalia mellifera"),values=c("green", "yellow","grey","orange","blue","purple"))+
  theme_fancy()+  coord_fixed(xlim=c(0,0.8),ylim=c(0,0.8))+
  ggtitle("Comparison of Drone NDVI with WV2 NDVI")+ theme(legend.position = c(0.2, 0.85))

plot(p2_NDVI)

ggsave2(
  "E:/Glenn/Botswana/R_Scripts/slade-prosopis/output_data/Plots/WV2_vs_Drone_NDVI_.png",
  plot = p2_NDVI,
  device = NULL,
  path = NULL,
  scale = 1,
  width = 140,
  height = 140,
  units =  "mm",
  dpi = 300,
  limitsize = TRUE
)

# plot with stats

x <- as.vector(WV2_full_traine$NDVI)
y <- as.vector(WV2_full_traine$WV2_NDVI)
df <- data.frame(x = x, y = y,
                 d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
# Calculate Total Least Squares Regression (extracted from base-R PCA function)
pca <- prcomp(~x+y,df)
tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")

# Compute the Lin's  correlation concordance coefficient
ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))

MADval <- mean(abs(x-y))
MADrel <- MADval/mean(x)*100
lmres <- lm(y~x)
r2val <- summary(lmres)$r.squared
#ggplot(df) + geom_point(aes(x, y, color = density)) + scale_color_viridis()
#+ geom_density_2d_filled(alpha = 0.5)


p3_NDVI <- ggplot(WV2_full_traine,aes (NDVI,WV2_NDVI, colour=factor(majority))) +
  geom_point() + geom_abline(intercept = 0, slope = 1, col='grey' ) +scale_color_manual(name = "Vegetation Type", labels = c("Prosopis", "Bare sand","Grass","Vachellia erioloba","Rhigosum trichotomum","Senegalia mellifera"),values=c("green", "yellow","grey","orange","blue","purple","black"))+
  theme_fancy()+  coord_fixed(xlim=c(0,0.8),ylim=c(0,0.8))+
  ggtitle("Comparison of Drone NDVI with WV2 NDVI")+ theme(legend.position = c(0.2, 0.85))+ geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) 
#   geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
#   geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
#   geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
#   geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)
plot(p3_NDVI)

ggsave2(
  "E:/Glenn/Botswana/R_Scripts/slade-prosopis/output_data/Plots/WV2_vs_Drone_NDVI_with_line.png",
  plot = p3_NDVI,
  device = NULL,
  path = NULL,
  scale = 1,
  width = 140,
  height = 140,
  units =  "mm",
  dpi = 300,
  limitsize = TRUE
)

