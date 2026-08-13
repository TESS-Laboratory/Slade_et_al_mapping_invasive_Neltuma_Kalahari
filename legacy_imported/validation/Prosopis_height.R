# ===========================================================================
# IMPORTED - not part of the original published archive.
#
#   source repo   slade-prosopis
#   source path   Prosopis_height.R
#   ref           main @ 671e56f
#   sha256        04e7ce5821668790117c058aa6547ff0902211cc12cbea753b20418dc5db05d2
#
#   why           Plant-scale height/detection analysis feeding Table S9 (n=184).
#
# Content below is VERBATIM and does not run as-is: it depends on Windows
# absolute paths under E:/Glenn/Botswana/ and on archived packages (rgeos,
# rgdal), and calls windowsFonts(). It is kept as the reference
# implementation to port into the targets pipeline, not to execute.
# See refactor-findings.md and audit/source-recovery-map.md.
# ===========================================================================

# Script comparing data from Venter 2018 Cover Map with UNDP surveys



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

#----1. Read in files----

# Shape file containing all shape files for height data points

#Bokspits_1_height <- read_sf(dsn = 'E:/Glenn/Botswana/Field_Height_Measurements', layer = "Bokspits_1_Prosopis_height_polygons2")
#Struizendam_1_height <- read_sf(dsn = 'E:/Glenn/Botswana/Field_Height_Measurements', layer = "Struizendam_1_Prosopis_height_polygons2")

Bokspits_1_height <- read_sf(dsn = 'E:/Glenn/Botswana/Field_Height_Measurements', layer = "Bokspits_1_Prosopis_height_points_20b")
Struizendam_1_height <- read_sf(dsn = 'E:/Glenn/Botswana/Field_Height_Measurements', layer = "Struizendam_1_Prosopis_height_points_20b")



#field_surveys1 <- vect(field_surveys)

Bokspits_1_CHM <- rast("E:/Glenn/Botswana/Reflstacks/Bokspits_1_CHM.tif")
Struizendam_1_CHM <- rast("E:/Glenn/Botswana/Reflstacks/Struizendam_1_CHM.tif")
Bokspits_1_Class <- rast("E:/Glenn/Botswana/R_Scripts/Glenn-Prosopis-ML/data/svm_pixel_level_Bokspits_1_stack_5_CHM_ALLVI.tif")
Struizendam_1_Class <- rast("E:/Glenn/Botswana/R_Scripts/Glenn-Prosopis-ML/data/svm_pixel_level_Struizendam_1_stack_5_CHM_ALLVI.tif")


Bokspits_1_MJ <- rast("E:/Glenn/Botswana/R_Scripts/Glenn-Prosopis-ML/data/Bokspits_1_Class_MJ20.tif") 
Struizendam_1_MJ <- rast("E:/Glenn/Botswana/R_Scripts/Glenn-Prosopis-ML/data/Struizendam_1_Class_MJ20.tif")


#---2. Extract data for Grid


H_Extract <- exact_extract(Bokspits_1_CHM,Bokspits_1_height,"max")
names(H_Extract) <- c('CHM')
H_Extract_DF <-bind_cols(Bokspits_1_height,H_Extract)
H_Extract_DF <- dplyr::mutate(H_Extract_DF, CHM = H_Extract)

C_Extract <- exact_extract(Bokspits_1_Class,Bokspits_1_height,"max")
H_Extract_DF <- dplyr::mutate(H_Extract_DF, Class = C_Extract)

F_Extract <- exact_extract(Bokspits_1_MJ,Bokspits_1_height,"max")
H_Extract_DF <- dplyr::mutate(H_Extract_DF, ClassMJ = F_Extract)



H2_Extract <- exact_extract(Struizendam_1_CHM,Struizendam_1_height,"max")
names(H2_Extract) <- c('CHM')
H2_Extract_DF <-bind_cols(Struizendam_1_height,H2_Extract)
H2_Extract_DF <- dplyr::mutate(H2_Extract_DF, CHM = H2_Extract)

C2_Extract <- exact_extract(Struizendam_1_Class,Struizendam_1_height,"max")
H2_Extract_DF <- dplyr::mutate(H2_Extract_DF, Class = C2_Extract)

F2_Extract <- exact_extract(Struizendam_1_MJ,Struizendam_1_height,"max")
H2_Extract_DF <- dplyr::mutate(H2_Extract_DF, ClassMJ = F2_Extract)


# Most basic bar chart
p_bar_B <- ggplot(H_Extract_DF, aes(x = factor(Class))) +geom_bar()
plot(p_bar_B)

p_bar_S <- ggplot(H2_Extract_DF, aes(x = factor(Class))) +geom_bar()

plot(p_bar_S)

p_bar_B <- ggplot(H_Extract_DF, aes(x = factor(ClassMJ))) +geom_bar()
plot(p_bar_B)

p_bar_S <- ggplot(H2_Extract_DF, aes(x = factor(ClassMJ))) +geom_bar()
plot(p_bar_S)


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


# Plotting FVC from Drone vs Venter FVC Map


x <- as.vector(H_Extract_DF$Height)
y <- as.vector(H_Extract_DF$CHM)
df <- data.frame(x = (x/100), y = y,
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
  geom_text(aes(x=0.0,y=4),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=3.5),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=3),label=ccc,hjust='left', size=2.0)+
  geom_text(aes(x=0.0,y=2.5),label=equation,hjust='left', size=2.0)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1, col='grey' ) +
  ggtitle("Comparison of Field Measurement with drone derived CHM")+
  #theme(aspect.ratio=1)+
  xlab('Field Measurement')+
  ylab('Drone derived CHM')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,5),ylim=c(0,5))
plot(p1)

#Struizendam

x <- as.vector(H2_Extract_DF$Height)
y <- as.vector(H2_Extract_DF$CHM)
df <- data.frame(x = (x/100), y = y,
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
  geom_text(aes(x=0.0,y=4),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=3.5),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=3),label=ccc,hjust='left', size=2.0)+
  geom_text(aes(x=0.0,y=2.5),label=equation,hjust='left', size=2.0)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1, col='grey' ) +
  ggtitle("Comparison of Field Measurement with drone derived CHM")+
  #theme(aspect.ratio=1)+
  xlab('Field Measurement')+
  ylab('Drone derived CHM')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,5),ylim=c(0,5))
plot(p2)
