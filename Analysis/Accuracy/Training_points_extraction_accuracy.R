### Script to Extract Data from Drone Classifications based on Drone pixel grid
### matching pixels
### Imports classified drone image data set and calculates fractional cover for each 
### class in each Drone pixel and then samples that data to produce training data set
### for classification of Drone image.

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
# #Change this to vary the pixel definitions
# A = "0.85"
# 
# #Fraction  cover to use to define training class for Prosopis 1.0 = 100%
# #Change this to vary the pixel definitions
# P = "0.85"
# 
# #Number of pixels from each Class to use in training data
# 
# N = "500"



Bokspits_1_Predict <- rast("output_data/Bokspits_1_Predict_Map.tif")
Bokspits_1_Predict

## NB You cant use exact extract with SpatVector so importing it a a spatial DF
Bokspits_1_points<- read_sf(dsn = 'input_data/', layer = "Bokspits_1_Field_data_points_All_b30")
Bokspits_1_points


#---2. Extract data for Bokspits_1 Grid

Bokspits_1_Drone_Extract <- exact_extract(Bokspits_1_Predict,Bokspits_1_points,"mode")
names(Bokspits_1_Drone_Extract) <- c('mode')
Bokspits_1_Drone_Extract_DF <-bind_cols(Bokspits_1_points,Bokspits_1_Drone_Extract)

Bokspits_1_Drone_Extract <- exact_extract(Bokspits_1_Predict,Bokspits_1_points,"majority" )
names(Bokspits_1_Drone_Extract) <- c('majority')
Bokspits_1_Drone_Extract_DF <- dplyr::mutate(Bokspits_1_Drone_Extract_DF, majority = Bokspits_1_Drone_Extract)

Bokspits_1_Drone_Extract <- exact_extract(Bokspits_1_Predict,Bokspits_1_points,"frac" )# calculates fraction cover for each classification value
#a column is added with the FVC for each vegetation type frac1, frac2 etc...
Bokspits_1_Drone_Extract_DF <- dplyr::mutate(Bokspits_1_Drone_Extract_DF, Bokspits_1_Drone_Extract)

df2 <- Bokspits_1_Drone_Extract_DF  %>% dplyr::select (fid,Type,majority,layer,frac_1, frac_2, frac_3,frac_4,frac_5,frac_6)

#Bokspits 2

Bokspits_2_Predict <- rast("output_data/Bokspits_2_Predict_Map.tif")
Bokspits_2_Predict

## NB You cant use exact extract with SpatVector so importing it a a spatial DF
Bokspits_2_points<- read_sf(dsn = 'input_data/', layer = "Bokspits_2_Field_data_points_All_b30")
Bokspits_2_points


#---2. Extract data for Bokspits_2 Grid

Bokspits_2_Drone_Extract <- exact_extract(Bokspits_2_Predict,Bokspits_2_points,"mode")
names(Bokspits_2_Drone_Extract) <- c('mode')
Bokspits_2_Drone_Extract_DF <-bind_cols(Bokspits_2_points,Bokspits_2_Drone_Extract)

Bokspits_2_Drone_Extract <- exact_extract(Bokspits_2_Predict,Bokspits_2_points,"majority" )
names(Bokspits_2_Drone_Extract) <- c('majority')
Bokspits_2_Drone_Extract_DF <- dplyr::mutate(Bokspits_2_Drone_Extract_DF, majority = Bokspits_2_Drone_Extract)

Bokspits_2_Drone_Extract <- exact_extract(Bokspits_2_Predict,Bokspits_2_points,"frac" )# calculates fraction cover for each classification value
#a column is added with the FVC for each vegetation type frac1, frac2 etc...
Bokspits_2_Drone_Extract_DF <- dplyr::mutate(Bokspits_2_Drone_Extract_DF, Bokspits_2_Drone_Extract)

df3 <- Bokspits_2_Drone_Extract_DF  %>% dplyr::select (fid,Type,majority,layer,frac_1, frac_2, frac_3,frac_4,frac_5,frac_6)

# Bokspits 3

Bokspits_3_Predict <- rast("output_data/Bokspits_3_Predict_Map.tif")
Bokspits_3_Predict

## NB You cant use exact extract with SpatVector so importing it a a spatial DF
Bokspits_3_points<- read_sf(dsn = 'input_data/', layer = "Bokspits_3_Field_data_points_All_b30")
Bokspits_3_points



#---2. Extract data for Bokspits_3 Grid

Bokspits_3_Drone_Extract <- exact_extract(Bokspits_3_Predict,Bokspits_3_points,"mode")
names(Bokspits_3_Drone_Extract) <- c('mode')
Bokspits_3_Drone_Extract_DF <-bind_cols(Bokspits_3_points,Bokspits_3_Drone_Extract)

Bokspits_3_Drone_Extract <- exact_extract(Bokspits_3_Predict,Bokspits_3_points,"majority" )
names(Bokspits_3_Drone_Extract) <- c('majority')
Bokspits_3_Drone_Extract_DF <- dplyr::mutate(Bokspits_3_Drone_Extract_DF, majority = Bokspits_3_Drone_Extract)

Bokspits_3_Drone_Extract <- exact_extract(Bokspits_3_Predict,Bokspits_3_points,"frac" )# calculates fraction cover for each classification value
#a column is added with the FVC for each vegetation type frac1, frac2 etc...
Bokspits_3_Drone_Extract_DF <- dplyr::mutate(Bokspits_3_Drone_Extract_DF, Bokspits_3_Drone_Extract)

df4 <- Bokspits_3_Drone_Extract_DF  %>% dplyr::select (fid,Type,majority,layer,frac_1, frac_2, frac_3,frac_4,frac_5,frac_6)

# Struizendam 1

Struizendam_1_Predict <- rast("output_data/Struizendam_1_Predict_Map.tif")
Struizendam_1_Predict

## NB You cant use exact extract with SpatVector so importing it a a spatial DF
Struizendam_1_points<- read_sf(dsn = 'input_data/', layer = "Struizendam_1_Field_data_points_All_b30")
Struizendam_1_points


#---2. Extract data for Struizendam_1 Grid

Struizendam_1_Drone_Extract <- exact_extract(Struizendam_1_Predict,Struizendam_1_points,"mode")
names(Struizendam_1_Drone_Extract) <- c('mode')
Struizendam_1_Drone_Extract_DF <-bind_cols(Struizendam_1_points,Struizendam_1_Drone_Extract)

Struizendam_1_Drone_Extract <- exact_extract(Struizendam_1_Predict,Struizendam_1_points,"majority" )
names(Struizendam_1_Drone_Extract) <- c('majority')
Struizendam_1_Drone_Extract_DF <- dplyr::mutate(Struizendam_1_Drone_Extract_DF, majority = Struizendam_1_Drone_Extract)

Struizendam_1_Drone_Extract <- exact_extract(Struizendam_1_Predict,Struizendam_1_points,"frac" )# calculates fraction cover for each classification value
#a column is added with the FVC for each vegetation type frac1, frac2 etc...
Struizendam_1_Drone_Extract_DF <- dplyr::mutate(Struizendam_1_Drone_Extract_DF, Struizendam_1_Drone_Extract)

df5 <- Struizendam_1_Drone_Extract_DF  %>% dplyr::select (fid,Type,majority,layer,frac_1, frac_2, frac_3,frac_5,frac_6)
df5[,"frac_4"]=NA
# Struizendam 2

Struizendam_2_Predict <- rast("output_data/Struizendam_2_Predict_Map.tif")
Struizendam_2_Predict

## NB You cant use exact extract with SpatVector so importing it a a spatial DF
Struizendam_2_points<- read_sf(dsn = 'input_data/', layer = "Struizendam_2_Field_data_points_All_b30")
Struizendam_2_points


#---2. Extract data for Struizendam_2 Grid

Struizendam_2_Drone_Extract <- exact_extract(Struizendam_2_Predict,Struizendam_2_points,"mode")
names(Struizendam_2_Drone_Extract) <- c('mode')
Struizendam_2_Drone_Extract_DF <-bind_cols(Struizendam_2_points,Struizendam_2_Drone_Extract)

Struizendam_2_Drone_Extract <- exact_extract(Struizendam_2_Predict,Struizendam_2_points,"majority" )
names(Struizendam_2_Drone_Extract) <- c('majority')
Struizendam_2_Drone_Extract_DF <- dplyr::mutate(Struizendam_2_Drone_Extract_DF, majority = Struizendam_2_Drone_Extract)

Struizendam_2_Drone_Extract <- exact_extract(Struizendam_2_Predict,Struizendam_2_points,"frac" )# calculates fraction cover for each classification value
#a column is added with the FVC for each vegetation type frac1, frac2 etc...
Struizendam_2_Drone_Extract_DF <- dplyr::mutate(Struizendam_2_Drone_Extract_DF, Struizendam_2_Drone_Extract)

df6 <- Struizendam_2_Drone_Extract_DF  %>% dplyr::select (fid,Type,majority,layer,frac_1, frac_2, frac_3,frac_5,frac_6)
df6[,"frac_4"]=NA
# Struizendam 3

Struizendam_3_Predict <- rast("output_data/Struizendam_3_Predict_Map.tif")
Struizendam_3_Predict

## NB You cant use exact extract with SpatVector so importing it a a spatial DF
Struizendam_3_points<- read_sf(dsn = 'input_data/', layer = "Struizendam_3_Field_data_points_All_b30")
Struizendam_3_points


#---2. Extract data for Struizendam_3 Grid

Struizendam_3_Drone_Extract <- exact_extract(Struizendam_3_Predict,Struizendam_3_points,"mode")
names(Struizendam_3_Drone_Extract) <- c('mode')
Struizendam_3_Drone_Extract_DF <-bind_cols(Struizendam_3_points,Struizendam_3_Drone_Extract)

Struizendam_3_Drone_Extract <- exact_extract(Struizendam_3_Predict,Struizendam_3_points,"majority" )
names(Struizendam_3_Drone_Extract) <- c('majority')
Struizendam_3_Drone_Extract_DF <- dplyr::mutate(Struizendam_3_Drone_Extract_DF, majority = Struizendam_3_Drone_Extract)

Struizendam_3_Drone_Extract <- exact_extract(Struizendam_3_Predict,Struizendam_3_points,"frac" )# calculates fraction cover for each classification value
#a column is added with the FVC for each vegetation type frac1, frac2 etc...
Struizendam_3_Drone_Extract_DF <- dplyr::mutate(Struizendam_3_Drone_Extract_DF, Struizendam_3_Drone_Extract)

df7 <- Struizendam_3_Drone_Extract_DF  %>% dplyr::select (fid,Type,majority,layer,frac_1, frac_2, frac_3,frac_5,frac_6)
df7[,"frac_4"]=NA
# Struizendam 4

Struizendam_4_Predict <- rast("output_data/Struizendam_4_Predict_Map.tif")
Struizendam_4_Predict

## NB You cant use exact extract with SpatVector so importing it a a spatial DF
Struizendam_4_points<- read_sf(dsn = 'input_data/', layer = "Struizendam_4_Field_data_points_All_b30")
Struizendam_4_points


#---2. Extract data for Struizendam_4 Grid

Struizendam_4_Drone_Extract <- exact_extract(Struizendam_4_Predict,Struizendam_4_points,"mode")
names(Struizendam_4_Drone_Extract) <- c('mode')
Struizendam_4_Drone_Extract_DF <-bind_cols(Struizendam_4_points,Struizendam_4_Drone_Extract)

Struizendam_4_Drone_Extract <- exact_extract(Struizendam_4_Predict,Struizendam_4_points,"majority" )
names(Struizendam_4_Drone_Extract) <- c('majority')
Struizendam_4_Drone_Extract_DF <- dplyr::mutate(Struizendam_4_Drone_Extract_DF, majority = Struizendam_4_Drone_Extract)

Struizendam_4_Drone_Extract <- exact_extract(Struizendam_4_Predict,Struizendam_4_points,"frac" )# calculates fraction cover for each classification value
#a column is added with the FVC for each vegetation type frac1, frac2 etc...
Struizendam_4_Drone_Extract_DF <- dplyr::mutate(Struizendam_4_Drone_Extract_DF, Struizendam_4_Drone_Extract)

df8 <- Struizendam_4_Drone_Extract_DF  %>% dplyr::select (fid,Type,majority,layer,frac_1, frac_2,frac_5,frac_6)
df8[,"frac_3"]=NA
df8[,"frac_4"]=NA
master_df <- rbind(df2,df3,df4,df5,df6,df7,df8)





prosopis_df <- df_F <- filter(master_df,Type == "1") 

P5 <- ggplot(prosopis_df, aes(x=majority)) +  geom_histogram(stat= "count", fill = "blue")#+
  scale_x_discrete(labels=c("1" = "Prosopis", "2"="Bare Ground", "3" = "Grass", "4" = "Gnidia","5" = "Vachellia erioloba","6" = "Other woody"))+
  xlab("Predicted")+ ylab("Number of predictions")+ ggtitle( "Prosopis")#+ theme_fancy()
plot (P5)



