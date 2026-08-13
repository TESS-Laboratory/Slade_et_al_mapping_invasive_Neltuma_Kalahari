# ===========================================================================
# IMPORTED - not part of the original published archive.
#
#   source repo   slade-prosopis
#   source path   Aggregate/WV2_and_Drone_Classification_hex_Extract_Version_1.R
#   ref           main @ 671e56f
#   sha256        f39beb5397eb92060e89d06b9849bf7514c14717dd07089d278fde21b3822ff7
#
#   why           Paired WV2 + drone hex extraction.
#
# Content below is VERBATIM and does not run as-is: it depends on Windows
# absolute paths under E:/Glenn/Botswana/ and on archived packages (rgeos,
# rgdal), and calls windowsFonts(). It is kept as the reference
# implementation to port into the targets pipeline, not to execute.
# See refactor-findings.md and audit/source-recovery-map.md.
# ===========================================================================

### Script to Extract Data from WV2 Classifications based on hexagonal grid
### and compares with data extracted from drone data classification


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
WV2 <- rast("E:/Glenn/Botswana/Satellite_Data/WV2/merged_mosaic/RF_WV2_all_train_val_combined_b30_additional_WV2_merged_mosaic.tif")


#----1. Read in files for Bokspits_1 etc Classified image data ----


Bokspits_1_Predict <- rast("E:/Glenn/Botswana/R_Scripts/Glenn-Prosopis-ML/data/svm_pixel_level_Bokspits_1_stack_5_CHM_ALLVI.tif")
Bokspits_1_Predict

## NB You cant use exact extract with SpatVector so importing it a a spatial DF
Bokspits_1_WV2_grid_SF <- read_sf(dsn = "E:/Glenn/Botswana/Satellite_Data/hex/hex_grids", layer = "Bokspits_1_hex_grid")
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

Bokspits_1_WV2_Extract_DF<- Bokspits_1_WV2_Extract_DF %>% rename(
    drone_frac_1 = frac_1,
    drone_frac_2 = frac_2,
    drone_frac_3 = frac_3,
    drone_frac_4 = frac_4,
    drone_frac_5 = frac_5,
    drone_frac_6 = frac_6 )

Bokspits_1_WV2_Extract <- exact_extract(WV2,Bokspits_1_WV2_grid_SF,"frac" )# calculates fraction cover for each classification value
#a column is added with the FVC for each vegetation type frac1, frac2 etc...
Bokspits_1_WV2_Extract_DF <- dplyr::mutate(Bokspits_1_WV2_Extract_DF, Bokspits_1_WV2_Extract)


#Bokspits 2

Bokspits_2_Predict <- rast("E:/Glenn/Botswana/R_Scripts/Glenn-Prosopis-ML/data/svm_pixel_level_Bokspits_2_stack_5_CHM_ALLVI.tif")
Bokspits_2_Predict

## NB You cant use exact extract with SpatVector so importing it a a spatial DF
Bokspits_2_WV2_grid_SF <- read_sf(dsn = "E:/Glenn/Botswana/Satellite_Data/hex/hex_grids", layer = "Bokspits_2_hex_grid")
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

Bokspits_2_WV2_Extract_DF<- Bokspits_2_WV2_Extract_DF %>% rename(
  drone_frac_1 = frac_1,
  drone_frac_2 = frac_2,
  drone_frac_3 = frac_3,
  drone_frac_5 = frac_5,
  drone_frac_6 = frac_6,
  drone_frac_7 = frac_7,
  drone_frac_13 = frac_13)

Bokspits_2_WV2_Extract <- exact_extract(WV2,Bokspits_2_WV2_grid_SF,"frac" )# calculates fraction cover for each classification value
#a column is added with the FVC for each vegetation type frac1, frac2 etc...
Bokspits_2_WV2_Extract_DF <- dplyr::mutate(Bokspits_2_WV2_Extract_DF, Bokspits_2_WV2_Extract)

# Bokspits 3

Bokspits_3_Predict <- rast("E:/Glenn/Botswana/R_Scripts/Glenn-Prosopis-ML/data/svm_pixel_level_Bokspits_3_stack_5_CHM_ALLVI.tif")
Bokspits_3_Predict

## NB You cant use exact extract with SpatVector so importing it a a spatial DF
Bokspits_3_WV2_grid_SF <- read_sf(dsn = "E:/Glenn/Botswana/Satellite_Data/hex/hex_grids", layer = "Bokspits_3_hex_grid")
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

Bokspits_3_WV2_Extract_DF<- Bokspits_3_WV2_Extract_DF %>% rename(
  drone_frac_1 = frac_1,
  drone_frac_2 = frac_2,
  drone_frac_3 = frac_3,
  drone_frac_5 = frac_5,
  drone_frac_6 = frac_6,  drone_frac_7 = frac_7)

Bokspits_3_WV2_Extract <- exact_extract(WV2,Bokspits_3_WV2_grid_SF,"frac" )# calculates fraction cover for each classification value
#a column is added with the FVC for each vegetation type frac1, frac2 etc...
Bokspits_3_WV2_Extract_DF <- dplyr::mutate(Bokspits_3_WV2_Extract_DF, Bokspits_3_WV2_Extract)

# Struizendam_1

Struizendam_1_Predict <- rast("E:/Glenn/Botswana/R_Scripts/Glenn-Prosopis-ML/data/svm_pixel_level_Struizendam_1_stack_5_CHM_ALLVI.tif")
Struizendam_1_Predict

## NB You cant use exact extract with SpatVector so importing it a a spatial DF
Struizendam_1_WV2_grid_SF <- read_sf(dsn = "E:/Glenn/Botswana/Satellite_Data/hex/hex_grids", layer = "Struizendam_1_hex_grid")
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

Struizendam_1_WV2_Extract_DF<- Struizendam_1_WV2_Extract_DF %>% rename(
  drone_frac_1 = frac_1,
  drone_frac_2 = frac_2,
  drone_frac_3 = frac_3,
  drone_frac_5 = frac_5,
  drone_frac_6 = frac_6 )

Struizendam_1_WV2_Extract <- exact_extract(WV2,Struizendam_1_WV2_grid_SF,"frac" )# calculates fraction cover for each classification value
#a column is added with the FVC for each vegetation type frac1, frac2 etc...
Struizendam_1_WV2_Extract_DF <- dplyr::mutate(Struizendam_1_WV2_Extract_DF, Struizendam_1_WV2_Extract)

#Struizendam_2

Struizendam_2_Predict <- rast("E:/Glenn/Botswana/R_Scripts/Glenn-Prosopis-ML/data/svm_pixel_level_Struizendam_2_stack_5_CHM_ALLVI.tif")
Struizendam_2_Predict

## NB You cant use exact extract with SpatVector so importing it a a spatial DF
Struizendam_2_WV2_grid_SF <- read_sf(dsn = "E:/Glenn/Botswana/Satellite_Data/hex/hex_grids", layer = "Struizendam_2_hex_grid")
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

Struizendam_2_WV2_Extract_DF<- Struizendam_2_WV2_Extract_DF %>% rename(
  drone_frac_1 = frac_1,
  drone_frac_2 = frac_2,
  drone_frac_3 = frac_3,
  drone_frac_5 = frac_5,
  drone_frac_6 = frac_6 )

Struizendam_2_WV2_Extract <- exact_extract(WV2,Struizendam_2_WV2_grid_SF,"frac" )# calculates fraction cover for each classification value
#a column is added with the FVC for each vegetation type frac1, frac2 etc...
Struizendam_2_WV2_Extract_DF <- dplyr::mutate(Struizendam_2_WV2_Extract_DF, Struizendam_2_WV2_Extract)

#Struizendam 3

Struizendam_3_Predict <- rast("E:/Glenn/Botswana/R_Scripts/Glenn-Prosopis-ML/data/svm_pixel_level_Struizendam_3_stack_5_CHM_ALLVI.tif")
Struizendam_3_Predict

## NB You cant use exact extract with SpatVector so importing it a a spatial DF
Struizendam_3_WV2_grid_SF <- read_sf(dsn = "E:/Glenn/Botswana/Satellite_Data/hex/hex_grids", layer = "Struizendam_3_hex_grid")
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

Struizendam_3_WV2_Extract_DF<- Struizendam_3_WV2_Extract_DF %>% rename(
  drone_frac_1 = frac_1,
  drone_frac_2 = frac_2,
  drone_frac_3 = frac_3,
  drone_frac_6 = frac_6, drone_frac_7 = frac_7, drone_frac_10 = frac_10 )

Struizendam_3_WV2_Extract <- exact_extract(WV2,Struizendam_3_WV2_grid_SF,"frac" )# calculates fraction cover for each classification value
#a column is added with the FVC for each vegetation type frac1, frac2 etc...
Struizendam_3_WV2_Extract_DF <- dplyr::mutate(Struizendam_3_WV2_Extract_DF, Struizendam_3_WV2_Extract)

# Struizendam 4

Struizendam_4_Predict <- rast("E:/Glenn/Botswana/R_Scripts/Glenn-Prosopis-ML/data/svm_pixel_level_Struizendam_4_stack_5_CHM_ALLVI.tif")
Struizendam_4_Predict

## NB You cant use exact extract with SpatVector so importing it a a spatial DF
Struizendam_4_WV2_grid_SF <- read_sf(dsn = "E:/Glenn/Botswana/Satellite_Data/hex/hex_grids", layer = "Struizendam_4_hex_grid")
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

Struizendam_4_WV2_Extract_DF<- Struizendam_4_WV2_Extract_DF %>% rename(
  drone_frac_1 = frac_1,
  drone_frac_2 = frac_2,
  drone_frac_5 = frac_5,
  drone_frac_6 = frac_6 )

Struizendam_4_WV2_Extract <- exact_extract(WV2,Struizendam_4_WV2_grid_SF,"frac" )# calculates fraction cover for each classification value
#a column is added with the FVC for each vegetation type frac1, frac2 etc...
Struizendam_4_WV2_Extract_DF <- dplyr::mutate(Struizendam_4_WV2_Extract_DF, Struizendam_4_WV2_Extract)


Hex_100m_extract_df <-bind_rows(Bokspits_1_WV2_Extract_DF, Bokspits_2_WV2_Extract_DF,Bokspits_3_WV2_Extract_DF, Struizendam_1_WV2_Extract_DF, Struizendam_2_WV2_Extract_DF,Struizendam_3_WV2_Extract_DF,Struizendam_4_WV2_Extract_DF)


# Plot of Prosopis fractional cover
## Plotting theme
theme_fancy <- function() {
  theme_bw() +
    theme(
      text = element_text(family = "Helvetica"),
      axis.text = element_text(size = 7, color = "black"),
      axis.title = element_text(size = 7, color = "black"),
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
        size = 7,
        vjust = 1,
        hjust = 0.5,
        color = "black"
      ),
      legend.text = element_text(size = 7, color = "black"),
      legend.title = element_text(size = 7, color = "black"),
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



x <- as.vector(Hex_100m_extract_df$drone_frac_1)
y <- as.vector(Hex_100m_extract_df$frac_1)
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

p1 <- ggplot(df) +
  geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
  geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1, col='grey' ) +
  ggtitle("Comparison of Prosopis Fractional Vegetation Cover (FVC)\n derived from Drone Classification vs data \n extracted from WV2 Classification")+
  #theme(aspect.ratio=1)+
  xlab('FVC Prosopis Drone Classification')+
  ylab('FVC Prosopis WV2 Classification')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
plot(p1)


# Baresand
x <- as.vector(Hex_100m_extract_df$drone_frac_2)
y <- as.vector(Hex_100m_extract_df$frac_2)
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

p2 <- ggplot(df) +
  geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.9),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.84),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.80),label=ccc,hjust='left', size=2.0)+
  geom_text(aes(x=0.0,y=0.76),label=equation,hjust='left', size=2.0)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1, col='grey' ) +
  ggtitle("Comparison of Bare Sand Fractional Cover (FC)\n derived from Drone Classification vs data \n extracted from WV2 Classification")+
  #theme(aspect.ratio=1)+
  xlab('FC Bare Sand Drone Classification')+
  ylab('FC Bare Sand WV2 Classification')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,1),ylim=c(0,1))
plot(p2)

# Grass



x <- as.vector(Hex_100m_extract_df$drone_frac_3)
y <- as.vector(Hex_100m_extract_df$frac_3)
df <- data.frame(x = x, y = y,
                 d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))

df <-df %>% na.omit()
x <- as.vector(df$x)
y <- as.vector(df$y)

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

p3 <- ggplot(df) +
  geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.9),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.86),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.82),label=ccc,hjust='left', size=2.0)+
  geom_text(aes(x=0.0,y=0.78),label=equation,hjust='left', size=2.0)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1, col='grey' ) +
  ggtitle("Comparison of Grass Fractional Vegetation Cover (FVC)\n derived from Drone Classification vs data \n extracted from WV2 Classification")+
  #theme(aspect.ratio=1)+
  xlab('FVC Grass Drone Data Classification')+
  ylab('FVC Grass WV2 Data Classification')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,1),ylim=c(0,1))
plot(p3)

# CT


x <- as.vector(Hex_100m_extract_df$drone_frac_5)
y <- as.vector(Hex_100m_extract_df$frac_5)
df <- data.frame(x = x, y = y,
                 d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))

df <-df %>% na.omit()
x <- as.vector(df$x)
y <- as.vector(df$y)

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

p5 <- ggplot(df) +
  geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.09),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.085),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.08),label=ccc,hjust='left', size=2.0)+
  geom_text(aes(x=0.0,y=0.075),label=equation,hjust='left', size=2.0)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1, col='grey' ) +
  ggtitle("Comparison of Vachellia erioloba (VE) Fractional Vegetation \n Cover (FVC) derived from Drone Classification \n vs data extracted from WV2 Classification")+
  #theme(aspect.ratio=1)+
  xlab('FVC VE Drone Classification')+
  ylab('FVC VE WV2 Classification')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,.1),ylim=c(0,.1))
plot(p5)

# Rhig Trig

x <- as.vector(Hex_100m_extract_df$drone_frac_6)
y <- as.vector(Hex_100m_extract_df$frac_6)
df <- data.frame(x = x, y = y,
                 d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))

df <-df %>% na.omit()
x <- as.vector(df$x)
y <- as.vector(df$y)

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

p6 <- ggplot(df) +
  geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.65),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.62),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.59),label=ccc,hjust='left', size=2.0)+
  geom_text(aes(x=0.0,y=0.56),label=equation,hjust='left', size=2.0)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1, col='grey' ) +
  ggtitle("Comparison of Rhigosum trichotomum (RT) Fractional Vegetation \n Cover (FVC) derived from Drone Classification \n vs data extracted from WV2 Classification")+
  #theme(aspect.ratio=1)+
  xlab('FVC RT Drone Data Classification')+
  ylab('FVC RT WV2 Data Classification')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,.75),ylim=c(0,.75))
plot(p6)


# Mellifera

x <- as.vector(Hex_100m_extract_df$drone_frac_7)
y <- as.vector(Hex_100m_extract_df$frac_7)
df <- data.frame(x = x, y = y,
                 d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))

df <-df %>% na.omit()
x <- as.vector(df$x)
y <- as.vector(df$y)

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

p7 <- ggplot(df) +
  geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.20),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.18),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.16),label=ccc,hjust='left', size=2.0)+
  geom_text(aes(x=0.0,y=0.14),label=equation,hjust='left', size=2.0)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1, col='grey' ) +
  ggtitle("Comparison of Senegalia Melifera (SF) Fractional Vegetation \n Cover (FVC) derived from Drone Classification \n vs data extracted from WV2 Classification")+
  #theme(aspect.ratio=1)+
  xlab('FVC SM Drone Data Classification')+
  ylab('FVC SM WV2 Data Classification')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,.2),ylim=c(0,.2))
plot(p7)

# Arrange plots

# title <- ggdraw() + draw_label("Comparison of fractional cover derived from drone and WV2 classifications", fontface='bold')
# top_row <- plot_grid(p1,p2, ncol = 2)
# row_2 <- plot_grid(p3,p5,  ncol = 2)
# row_3 <- plot_grid(p6,p7, ncol = 2)
# p<- plot_grid(p1,p2,p3,p5,p6,p7, nrow = 3,rel_heights = c(1, 1,1))
# p

p <-grid.arrange(p1,p2,p3,p5,p6,p7, nrow = 3)


ggsave2(
  "E:/Glenn/Botswana/R_Scripts/slade-prosopis/output_data/Plots/drone_wv2_fractional_cover_comparison.png",
  plot = p,
  device = NULL,
  path = NULL,
  scale = 1,
  width = 170,
  height = 260,
  units =  "mm",
  dpi = 300,
  limitsize = TRUE
)
