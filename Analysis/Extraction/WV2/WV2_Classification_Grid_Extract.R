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

#-------0. SET Parameters to use for training data set

#Fraction  cover to use to define training class 1.0 = 100%
#Change this to vary the pixel definitions
A = "0.95"

#Fraction  cover to use to define training class for Prosopis 1.0 = 100%
#Change this to vary the pixel definitions
P = "0.95"

#Number of pixels from each Class to use in training data

N = "100"





#----1. Read in files for Bokspits_1 etc Classified image data ----


Bokspits_1_Predict <- rast("/data_out/Bokspits_1/Bokspits_1_Predict_Map.tif")

## NB You cant use exact extract with SpatVector so importing it a a spatial DF
Bokspits_1_WV2_grid_SF <- read_sf(dsn = '/data_in/WV2_Grids', layer = "Bokspits_1_WV2_grid")



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

#----3. Read in files for Bokspits_2 etc Classified image data ----


Bokspits_2_Predict <- rast("/data_out/Bokspits_2/Bokspits_2_Predict_Map.tif")

## NB You cant use exact extract with SpatVector so re-importing it a a spatial DF
Bokspits_2_WV2_grid_SF <- read_sf(dsn = 'data_in/WV2_Grids', layer = "Bokspits_2_WV2_grid")


#---4.. Extract data for Bokspits_2 Grid


Bokspits_2_WV2_Extract <- exact_extract(Bokspits_2_Predict,Bokspits_2_WV2_grid_SF,"mode")
names(Bokspits_2_WV2_Extract) <- c('mode')
Bokspits_2_WV2_Extract_DF <-bind_cols(Bokspits_2_WV2_grid_SF,Bokspits_2_WV2_Extract)

Bokspits_2_WV2_Extract <- exact_extract(Bokspits_2_Predict,Bokspits_2_WV2_grid_SF,"majority" )
names(Bokspits_2_WV2_Extract) <- c('majority')
Bokspits_2_WV2_Extract_DF <- dplyr::mutate(Bokspits_2_WV2_Extract_DF, majority = Bokspits_2_WV2_Extract)

Bokspits_2_WV2_Extract <- exact_extract(Bokspits_2_Predict,Bokspits_2_WV2_grid_SF,"frac" )# calculates fraction cover for each classification value
#a column is added with the FVC for each vegetation type frac1, frac2 etc...
Bokspits_2_WV2_Extract_DF <- dplyr::mutate(Bokspits_2_WV2_Extract_DF, Bokspits_2_WV2_Extract)

#----5. Read in files for Bokspits_3 etc Classified image data ----


Bokspits_3_Predict <- rast("/data_out/Bokspits_3/Bokspits_3_Predict_Map.tif")

## NB You cant use exact extract with SpatVector so re-importing it a a spatial DF
Bokspits_3_WV2_grid_SF <- read_sf(dsn = 'data_in/WV2_Grids', layer = "Bokspits_3_WV2_grid")


#---6. Extract data for Bokspits_3 Grid


Bokspits_3_WV2_Extract <- exact_extract(Bokspits_3_Predict,Bokspits_3_WV2_grid_SF,"mode")
names(Bokspits_3_WV2_Extract) <- c('mode')
Bokspits_3_WV2_Extract_DF <-bind_cols(Bokspits_3_WV2_grid_SF,Bokspits_3_WV2_Extract)

Bokspits_3_WV2_Extract <- exact_extract(Bokspits_3_Predict,Bokspits_3_WV2_grid_SF,"majority" )
names(Bokspits_3_WV2_Extract) <- c('majority')
Bokspits_3_WV2_Extract_DF <- dplyr::mutate(Bokspits_3_WV2_Extract_DF, majority = Bokspits_3_WV2_Extract)

Bokspits_3_WV2_Extract <- exact_extract(Bokspits_3_Predict,Bokspits_3_WV2_grid_SF,"frac" )# calculates fraction cover for each classification value
#a column is added with the FVC for each vegetation type frac1, frac2 etc...
Bokspits_3_WV2_Extract_DF <- dplyr::mutate(Bokspits_3_WV2_Extract_DF, Bokspits_3_WV2_Extract)


#----7. Read in files for Struizendam_1 etc Classified image data ----


Struizendam_1_Predict <- rast("/data_out/Struizendam_1/Struizendam_1_Predict_Map.tif")

## NB You cant use exact extract with SpatVector so re-importing it a a spatial DF
Struizendam_1_WV2_grid_SF <- read_sf(dsn = 'data_in/WV2_Grids', layer = "Struizendam_1_WV2_grid")


#---8. Extract data for Struizendam_1 Grid


Struizendam_1_WV2_Extract <- exact_extract(Struizendam_1_Predict,Struizendam_1_WV2_grid_SF,"mode")
names(Struizendam_1_WV2_Extract) <- c('mode')
Struizendam_1_WV2_Extract_DF <-bind_cols(Struizendam_1_WV2_grid_SF,Struizendam_1_WV2_Extract)

Struizendam_1_WV2_Extract <- exact_extract(Struizendam_1_Predict,Struizendam_1_WV2_grid_SF,"majority" )
names(Struizendam_1_WV2_Extract) <- c('majority')
Struizendam_1_WV2_Extract_DF <- dplyr::mutate(Struizendam_1_WV2_Extract_DF, majority = Struizendam_1_WV2_Extract)

Struizendam_1_WV2_Extract <- exact_extract(Struizendam_1_Predict,Struizendam_1_WV2_grid_SF,"frac" )# calculates fraction cover for each classification value
#a column is added with the FVC for each vegetation type frac1, frac2 etc...
Struizendam_1_WV2_Extract_DF <- dplyr::mutate(Struizendam_1_WV2_Extract_DF, Struizendam_1_WV2_Extract)


#----9. Read in files for Struizendam_2 etc Classified image data ----


Struizendam_2_Predict <- rast("/data_out/Struizendam_2/Struizendam_2_Predict_Map.tif")

## NB You cant use exact extract with SpatVector so re-importing it a a spatial DF
Struizendam_2_WV2_grid_SF <- read_sf(dsn = 'data_in/WV2_Grids', layer = "Struizendam_2_WV2_grid")


#---10. Extract data for Struizendam_2 Grid


Struizendam_2_WV2_Extract <- exact_extract(Struizendam_2_Predict,Struizendam_2_WV2_grid_SF,"mode")
names(Struizendam_2_WV2_Extract) <- c('mode')
Struizendam_2_WV2_Extract_DF <-bind_cols(Struizendam_2_WV2_grid_SF,Struizendam_2_WV2_Extract)

Struizendam_2_WV2_Extract <- exact_extract(Struizendam_2_Predict,Struizendam_2_WV2_grid_SF,"majority" )
names(Struizendam_2_WV2_Extract) <- c('majority')
Struizendam_2_WV2_Extract_DF <- dplyr::mutate(Struizendam_2_WV2_Extract_DF, majority = Struizendam_2_WV2_Extract)

Struizendam_2_WV2_Extract <- exact_extract(Struizendam_2_Predict,Struizendam_2_WV2_grid_SF,"frac" )# calculates fraction cover for each classification value
#a column is added with the FVC for each vegetation type frac1, frac2 etc...
Struizendam_2_WV2_Extract_DF <- dplyr::mutate(Struizendam_2_WV2_Extract_DF, Struizendam_2_WV2_Extract)

#----11. Read in files for Struizendam_3 etc Classified image data ----


Struizendam_3_Predict <- rast("/data_out/Struizendam_3/Struizendam_3_Predict_Map.tif")

## NB You cant use exact extract with SpatVector so re-importing it a a spatial DF
Struizendam_3_WV2_grid_SF <- read_sf(dsn = 'data_in/WV2_Grids', layer = "Struizendam_3_WV2_grid")


#---12. Extract data for Struizendam_3 Grid


Struizendam_3_WV2_Extract <- exact_extract(Struizendam_3_Predict,Struizendam_3_WV2_grid_SF,"mode")
names(Struizendam_3_WV2_Extract) <- c('mode')
Struizendam_3_WV2_Extract_DF <-bind_cols(Struizendam_3_WV2_grid_SF,Struizendam_3_WV2_Extract)

Struizendam_3_WV2_Extract <- exact_extract(Struizendam_3_Predict,Struizendam_3_WV2_grid_SF,"majority" )
names(Struizendam_3_WV2_Extract) <- c('majority')
Struizendam_3_WV2_Extract_DF <- dplyr::mutate(Struizendam_3_WV2_Extract_DF, majority = Struizendam_3_WV2_Extract)

Struizendam_3_WV2_Extract <- exact_extract(Struizendam_3_Predict,Struizendam_3_WV2_grid_SF,"frac" )# calculates fraction cover for each classification value
#a column is added with the FVC for each vegetation type frac1, frac2 etc...
Struizendam_3_WV2_Extract_DF <- dplyr::mutate(Struizendam_3_WV2_Extract_DF, Struizendam_3_WV2_Extract)

#----13. Read in files for Struizendam_4 etc Classified image data ----


Struizendam_4_Predict <- rast("/data_out/Struizendam_4/Struizendam_4_Predict_Map.tif")

## NB You cant use exact extract with SpatVector so re-importing it a a spatial DF
Struizendam_4_WV2_grid_SF <- read_sf(dsn = 'data_in/WV2_Grids', layer = "Struizendam_4_WV2_grid")


#---14. Extract data for Struizendam_4 Grid


Struizendam_4_WV2_Extract <- exact_extract(Struizendam_4_Predict,Struizendam_4_WV2_grid_SF,"mode")
names(Struizendam_4_WV2_Extract) <- c('mode')
Struizendam_4_WV2_Extract_DF <-bind_cols(Struizendam_4_WV2_grid_SF,Struizendam_4_WV2_Extract)

Struizendam_4_WV2_Extract <- exact_extract(Struizendam_4_Predict,Struizendam_4_WV2_grid_SF,"majority" )
names(Struizendam_4_WV2_Extract) <- c('majority')
Struizendam_4_WV2_Extract_DF <- dplyr::mutate(Struizendam_4_WV2_Extract_DF, majority = Struizendam_4_WV2_Extract)

Struizendam_4_WV2_Extract <- exact_extract(Struizendam_4_Predict,Struizendam_4_WV2_grid_SF,"frac" )# calculates fraction cover for each classification value
#a column is added with the FVC for each vegetation type frac1, frac2 etc...
Struizendam_4_WV2_Extract_DF <- dplyr::mutate(Struizendam_4_WV2_Extract_DF, Struizendam_4_WV2_Extract)

#-----15. Combine data frames from each drone survey into one training data set for all survey areas

WV2_full_Extract_DF <- bind_rows(Bokspits_1_WV2_Extract_DF, Bokspits_2_WV2_Extract_DF,Bokspits_3_WV2_Extract_DF, Struizendam_1_WV2_Extract_DF, Struizendam_2_WV2_Extract_DF,Struizendam_3_WV2_Extract_DF,Struizendam_4_WV2_Extract_DF)
#need to replace NA with zeros

WV2_full_Extract_DF[is.na(WV2_full_Extract_DF)] <- 0

#----16.  Select Parameters for defining training 

#Need to define all frac_ in final version

WV2_full_Extract_DF_95<- WV2_full_Extract_DF %>% filter_at(vars(frac_1,frac_2,frac_3,frac_5,frac_6,frac_7,frac_10), any_vars(. > 0.95))
#WV2_full_Extract_DF_P <- WV2_full_WV2_Extract_DF %>% filter_at(vars(frac_1), any_vars(. > paste0(P)))


saveRDS(WV2_full_Extract_DF_95, "data_in/WV2_full_train.rds")

#WV2_full_Extract_DF_95<- Bokspits_1_WV2_Extract_DF %>% filter_at(vars(frac_2,frac_3,frac_4,frac_5,frac_6), any_vars(. > paste0(A)))
#WV2_full_Extract_DF_P <- Bokspits_1_WV2_Extract_DF %>% filter_at(vars(frac_1), any_vars(. > paste0(P)))



WV2_full_Extract_DF_95_1 <-WV2_full_Extract_DF_95 [ sample( which( WV2_full_Extract_DF_P$majority == "1" ) , paste0(N) ) , ]
WV2_full_Extract_DF_95_2 <-WV2_full_Extract_DF_95[ sample( which( WV2_full_Extract_DF_95$majority == "2" ) , paste0(N)) , ]
WV2_full_Extract_DF_95_3 <-WV2_full_Extract_DF_95[ sample( which( WV2_full_Extract_DF_95$majority == "3" ) , paste0(N)) , ]
WV2_full_Extract_DF_95_5 <-WV2_full_Extract_DF_95[ sample( which( WV2_full_Extract_DF_95$majority == "5" ) , paste0(N)) , ]
WV2_full_Extract_DF_95_6 <-WV2_full_Extract_DF_95[ sample( which( WV2_full_Extract_DF_95$majority == "6" ) , paste0(N)) , ]
WV2_full_Extract_DF_95_7 <-WV2_full_Extract_DF_95[ sample( which( WV2_full_Extract_DF_95$majority == "7" ) , paste0(N)) , ]
WV2_full_Extract_DF_95_10 <-WV2_full_Extract_DF_95[ sample( which( WV2_full_Extract_DF_95$majority == "10" ) , paste0(N)) , ]

WV2_full_train <- bind_rows(WV2_full_Extract_DF_95_1,WV2_full_Extract_DF_95_2,WV2_full_Extract_DF_95_3,WV2_full_Extract_DF_95_5,WV2_full_Extract_DF_95_6,WV2_full_Extract_DF_95_7,WV2_full_Extract_DF_95_10)


saveRDS(WV2_full_train, "data_in/WV2_equal_class_size_train.rds")




#additional scripts not needed here
# extracts EVI trend from landsat data
Bokspits_1_WV2_Extract <- exact_extract(EVI,Bokspits_1_WV2_grid_SF,"mean" )
names(Bokspits_1_WV2_Extract) <- c('EVI_Trend')
Bokspits_1_WV2_Extract_DF <- dplyr::mutate(Bokspits_1_WV2_Extract_DF, EVI_Trend = Bokspits_1_WV2_Extract)

# add a new column and calculate FVC for all woody plant species (ie 100 - class 2 and class 3)


# export df as shape files

#Only Pixels with >95% cover of one veg type - with the type as majority class

Bokspits_1_WV2_classes_95<- Bokspits_1_WV2_Extract_DF %>% filter_at(vars(frac_1,frac_2,frac_3,frac_4,frac_5,frac_6), any_vars(. > 0.95))
write_sf(Bokspits_1_WV2_classes_95,"E:/Glenn/Botswana/Satellite_Data/WV2/Training_Data/Bokspits_1_WV2_Grid_95.shp" )

# script if training and validation separation required
#Bokspits_1_WV2_Grid__95_train <- Bokspits_1_WV2_classes_95 %>% dplyr::sample_frac(0.70)
#a<- as(Bokspits_1_WV2_Grid__95_train, 'Spatial')
#b<- as(Bokspits_1_WV2_classes_95, 'Spatial')
# sf::st_difference() # Hughs suggestion for an improvment on symdif
#Bokspits_1_WV2_Grid_95_val <- symdif(a,b)
#Bokspits_1_WV2_Grid_95_val <- st_as_sf(Bokspits_1_WV2_Grid_95_val)
#write_sf(Bokspits_1_WV2_Grid__95_train,"E:/Glenn/Botswana/Satellite_Data/WV2/Training_Data/Train/Bokspits_1_WV2_Grid_95_train.shp" )
#write_sf(Bokspits_1_WV2_Grid_95_val,"E:/Glenn/Botswana/Satellite_Data/WV2/Training_Data/Val/Bokspits_1_WV2_Grid_95_val.shp" )

#Only Pixels with >85% cover of one veg type - - with the type as majority class

Bokspits_1_WV2_classes_85<- Bokspits_1_WV2_Extract_DF %>% filter_at(vars(frac_1,frac_2,frac_3,frac_4,frac_5,frac_6), any_vars(. > 0.85))
write_sf(Bokspits_1_WV2_classes_85,"E:/Glenn/Botswana/Satellite_Data/WV2/Training_Data/Bokspits_1_WV2_Grid_85.shp" )

#Only Pixels with >75% cover of one veg type - - with the type as majority class

Bokspits_1_WV2_classes_75<- Bokspits_1_WV2_Extract_DF %>% filter_at(vars(frac_1,frac_2,frac_3,frac_4,frac_5,frac_6), any_vars(. > 0.75))
write_sf(Bokspits_1_WV2_classes_75,"E:/Glenn/Botswana/Satellite_Data/WV2/Training_Data/Bokspits_1_WV2_Grid_75.shp" )

#shape file with Prosopis divided into classes 0-7 - and then split this into 2 files - 70% train 30% val



#shape file with FVC (woody cover) ie sum of % all classes except 2 and 3

Bokspits_1_WV2_FVC <- mutate(Bokspits_1_WV2_Extract_DF,FVC = frac_1 +frac_4+ frac_5+frac_6)
write_sf(Bokspits_1_WV2_FVC,"E:/Glenn/Botswana/Satellite_Data/WV2/Training_Data/Bokspits_1_WV2_Grid_FVC.shp" )


#shape file with majority class - all data - to be used with majority class as training layer

write_sf(Bokspits_1_WV2_Extract_DF,"E:/Glenn/Botswana/Satellite_Data/WV2/Training_Data/Bokspits_1_WV2_Grid_All.shp" )


#Bokspits_1_WV2_Grid_All_val  <- dplyr::anti_join(a,b, by = 'id')




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

# Plotting NDVI from Drone vs WV2 Image Data


x <- as.vector(Bokspits_1_WV2_Extract_DF$frac_1)
y <- as.vector(Bokspits_1_WV2_Extract_DF$EVI_Trend)
df <- data.frame(x = x*10, y = y,
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

# Saving Plots

Plotcombined <-  plot_grid(Bp1_NDVI, p1_MSAVI2, p1_MTVI,p1_SAVI, nrow = 2, rel_heights = c(0.5,0.5))

ggsave(
  Plotcombined,
  filename = "E:Glenn/Botswana/R_Scripts/slade-prosopis/output_data/plots/Drone_vs_WV2_VI_combined_plots.png",
  width =17,
  height = 17,
  units = "cm"
)

