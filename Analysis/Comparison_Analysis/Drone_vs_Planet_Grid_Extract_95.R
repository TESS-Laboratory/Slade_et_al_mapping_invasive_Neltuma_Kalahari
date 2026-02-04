### Script to produce shapefiles of PlanetScope ploygons
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
library(cowplot)
library(viridisLite)

#----1. Read in files----

Bokspits_1_clipper <- read_sf(dsn = 'E:/Glenn/Botswana/Final_Drone_Survey_Data/Bokspits_1', layer = "Bokspits_1_clip")
Bokspits_1_clip <- vect(Bokspits_1_clipper)

Bokspits_1_NDVI <- rast("E:/Glenn/Botswana/Drone_Data_VI/Bokspits_1_NDVI.tif")
Bokspits_1_SAVI <- rast("E:/Glenn/Botswana/Drone_Data_VI/Bokspits_1_SAVI.tif")
Bokspits_1_MSAVI2 <- rast("E:/Glenn/Botswana/Drone_Data_VI/Bokspits_1_MSAVI2.tif")
Bokspits_1_MTVI <- rast("E:/Glenn/Botswana/Drone_Data_VI/Bokspits_1_MTVI.tif")

Planet_NDVI <-  rast("E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope/Bokspits_2022_09_NDVI.tif")
Planet_MSAVI2 <-  rast("E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope/Bokspits_2022_09_MSAVI2.tif")
Planet_SAVI <-  rast("E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope/Bokspits_2022_09_SAVI.tif")
Planet_MTVI <-  rast("E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope/Bokspits_2022_09_MTVI.tif")

## NB You cant use exact extract with SpatVector so re-importing it a a spatial DF
Bokspits_1_Planet_grid_SF <- read_sf(dsn = 'E:/Glenn/Botswana/Satellite_Data/Planet/Training_Data', layer = "Bokspits_1_Planet_Grid_95")


#---2. Extract data for Grid


Bokspits_1_Planet_Drone_Extract <- exact_extract(Bokspits_1_NDVI,Bokspits_1_Planet_grid_SF,"mean")
names(Bokspits_1_Planet_Drone_Extract) <- c('NDVI_Drone')
Bokspits_1_Planet_Drone_Extract_DF <-bind_cols(Bokspits_1_Planet_grid_SF,Bokspits_1_Planet_Drone_Extract)
Bokspits_1_Planet_Drone_Extract_DF <- dplyr::mutate(Bokspits_1_Planet_Drone_Extract_DF, NDVI_Drone = Bokspits_1_Planet_Drone_Extract)


Bokspits_1_Planet_Extract <- exact_extract(Planet_NDVI,Bokspits_1_Planet_grid_SF,"mean")
names(Bokspits_1_Planet_Extract) <- c('NDVI_Planet')
Bokspits_1_Planet_Drone_Extract_DF <- dplyr::mutate(Bokspits_1_Planet_Drone_Extract_DF, NDVI_Planet = Bokspits_1_Planet_Extract)

Bokspits_1_Planet_Extract <- exact_extract(Bokspits_1_MSAVI2,Bokspits_1_Planet_grid_SF,"mean")
names(Bokspits_1_Planet_Extract) <- c('MSAVI2_Drone')
Bokspits_1_Planet_Drone_Extract_DF <- dplyr::mutate(Bokspits_1_Planet_Drone_Extract_DF, MSAVI2_Drone = Bokspits_1_Planet_Extract)

Bokspits_1_Planet_Extract <- exact_extract(Planet_MSAVI2,Bokspits_1_Planet_grid_SF,"mean")
names(Bokspits_1_Planet_Extract) <- c('MSAVI2_Planet')
Bokspits_1_Planet_Drone_Extract_DF <- dplyr::mutate(Bokspits_1_Planet_Drone_Extract_DF, MSAVI2_Planet = Bokspits_1_Planet_Extract)


Bokspits_1_Planet_Extract <- exact_extract(Bokspits_1_MTVI,Bokspits_1_Planet_grid_SF,"mean")
names(Bokspits_1_Planet_Extract) <- c('MTVI_Drone')
Bokspits_1_Planet_Drone_Extract_DF <- dplyr::mutate(Bokspits_1_Planet_Drone_Extract_DF, MTVI_Drone = Bokspits_1_Planet_Extract)

Bokspits_1_Planet_Extract <- exact_extract(Planet_MTVI,Bokspits_1_Planet_grid_SF,"mean")
names(Bokspits_1_Planet_Extract) <- c('MTVI_Planet')
Bokspits_1_Planet_Drone_Extract_DF <- dplyr::mutate(Bokspits_1_Planet_Drone_Extract_DF, MTVI_Planet = Bokspits_1_Planet_Extract)

Bokspits_1_Planet_Extract <- exact_extract(Bokspits_1_SAVI,Bokspits_1_Planet_grid_SF,"mean")
names(Bokspits_1_Planet_Extract) <- c('SAVI_Drone')
Bokspits_1_Planet_Drone_Extract_DF <- dplyr::mutate(Bokspits_1_Planet_Drone_Extract_DF, SAVI_Drone = Bokspits_1_Planet_Extract)

Bokspits_1_Planet_Extract <- exact_extract(Planet_SAVI,Bokspits_1_Planet_grid_SF,"mean")
names(Bokspits_1_Planet_Extract) <- c('SAVI_Planet')
Bokspits_1_Planet_Drone_Extract_DF <- dplyr::mutate(Bokspits_1_Planet_Drone_Extract_DF, SAVI_Planet = Bokspits_1_Planet_Extract)



write_csv(Bokspits_1_Planet_Drone_Extract_DF, "E:/Glenn/Botswana/R_Scripts/slade-prosopis/output_data/Bokspits_Drone_vs_Planet_VI_Extracted_95.csv")



#---3. Plot comparison of drone and planet data----

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


# Plotting NDVI from Drone vs Planet Image Data


x <- as.vector(Bokspits_1_Planet_Drone_Extract_DF$NDVI_Drone)
y <- as.vector(Bokspits_1_Planet_Drone_Extract_DF$NDVI_Planet)
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

p1_NDVI <- ggplot(df) +
  geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1, colour=majority) +
  geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
  geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1, col='grey' ) +
  ggtitle("Comparison of Drone NDVI with Planet NDVI")+
  #theme(aspect.ratio=1)+
  xlab('Drone NDVI')+
  ylab('Planet NDVI')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.8),ylim=c(0,0.8))
plot(p1_NDVI)

# Plotting SAVI from Drone vs Planet Image Data


x <- as.vector(Bokspits_1_Planet_Drone_Extract_DF$SAVI_Drone)
y <- as.vector(Bokspits_1_Planet_Drone_Extract_DF$SAVI_Planet)
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

p1_SAVI <- ggplot(df) +
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
  ggtitle("Comparison of Drone SAVI with Planet SAVI")+
  #theme(aspect.ratio=1)+
  xlab('Drone SAVI')+
  ylab('Planet SAVI')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.8),ylim=c(0,0.8))
plot(p1_SAVI)

# Plotting MSAVI2 from Drone vs Planet Image Data


x <- as.vector(Bokspits_1_Planet_Drone_Extract_DF$MSAVI2_Drone)
y <- as.vector(Bokspits_1_Planet_Drone_Extract_DF$MSAVI2_Planet)
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

p1_MSAVI2 <- ggplot(df) +
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
  ggtitle("Comparison of Drone MSAVI2 with Planet MSAVI2")+
  #theme(aspect.ratio=1)+
  xlab('Drone MSAVI2')+
  ylab('Planet MSAVI2')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.8),ylim=c(0,0.8))
plot(p1_MSAVI2)

# Plotting MTVI from Drone vs Planet Image Data


x <- as.vector(Bokspits_1_Planet_Drone_Extract_DF$MTVI_Drone)
y <- as.vector(Bokspits_1_Planet_Drone_Extract_DF$MTVI_Planet)
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

p1_MTVI <- ggplot(df) +
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
  ggtitle("Comparison of Drone MTVI with Planet MTVI")+
  #theme(aspect.ratio=1)+
  xlab('Drone MTVI')+
  ylab('Planet MTVI')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(-0.2,0.6),ylim=c(-0.2,0.6))
plot(p1_MTVI)

# Saving Plots

Plotcombined <-  plot_grid(p1_NDVI, p1_MSAVI2, p1_MTVI,p1_SAVI, nrow = 2, rel_heights = c(0.5,0.5))

plot (Plotcombined)
ggsave(
  Plotcombined,
  filename = "E:Glenn/Botswana/R_Scripts/slade-prosopis/output_data/plots/Drone_vs_Planet_VI_combined_plots.png",
  width =17,
  height = 17,
  units = "cm"
)


#### Expermienting with colour density plots


x <- as.vector(Bokspits_1_Planet_Drone_Extract_DF$NDVI_Drone)
y <- as.vector(Bokspits_1_Planet_Drone_Extract_DF$NDVI_Planet)
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

p1_NDVI <- ggplot(df,aes (x,y)) + stat_density_2d(geom = "raster",aes(fill = after_stat(density)),contour = FALSE, alpha=0.5) +
  geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  # geom_point(aes(x, y), alpha=0.3, size = 1) + geom_density_2d_filled(aes(x,y),alpha = 0.55)+ 
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
  geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.0)+
  geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.0)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  stat_density_2d(
    geom = "raster",
    aes(fill = after_stat(density)),
    contour = FALSE
  ) + scale_fill_viridis_c()
geom_abline(intercept = 0, slope = 1, col='grey' ) +
  ggtitle("Comparison of Drone NDVI with Planet NDVI")+
  #theme(aspect.ratio=1)+
  xlab('Drone NDVI')+
  ylab('Planet NDVI')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.8),ylim=c(0,0.8))
plot(p1_NDVI)

# Plot of NDVI coloured for veg type

p2_NDVI <- ggplot(Bokspits_1_Planet_Drone_Extract_DF,aes (NDVI_Drone,NDVI_Planet, colour=factor(majority))) +
  geom_point() + geom_abline(intercept = 0, slope = 1, col='grey' ) +scale_color_manual(name = "Vegetation Type", labels = c("Prosopis", "Bare sand","Grass","Gnidia","Vachellia erioloba","Rhigosum trichotomum"),values=c("green", "yellow","grey","brown","orange","blue"))+
  theme_fancy()+  coord_fixed(xlim=c(0,0.8),ylim=c(0,0.8))+
  ggtitle("Comparison of Drone NDVI with Planet NDVI")+ theme(legend.position = c(0.2, 0.75))
  
plot(p2_NDVI)

ggsave2(
  "E:/Glenn/Botswana/R_Scripts/slade-prosopis/output_data/Plots/Planet_vs_Drone_NDVI_Bokspits_1.png",
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

# That was for Bokspits 1 - now do it for all the training data across Boravast

Bokspits_2_clipper <- read_sf(dsn = 'E:/Glenn/Botswana/Final_Drone_Survey_Data/Bokspits_2', layer = "Bokspits_2_clip")
Bokspits_2_clip <- vect(Bokspits_2_clipper)

Bokspits_2_NDVI <- rast("E:/Glenn/Botswana/Drone_Data_VI/Bokspits_2_NDVI.tif")
Bokspits_2_SAVI <- rast("E:/Glenn/Botswana/Drone_Data_VI/Bokspits_2_SAVI.tif")
Bokspits_2_MSAVI2 <- rast("E:/Glenn/Botswana/Drone_Data_VI/Bokspits_2_MSAVI2.tif")
Bokspits_2_MTVI <- rast("E:/Glenn/Botswana/Drone_Data_VI/Bokspits_2_MTVI.tif")


## NB You cant use exact extract with SpatVector so re-importing it a a spatial DF
#Bokspits_2_Planet_grid_SF <- read_sf(dsn = 'E:/Glenn/Botswana/Satellite_Data/Planet/Training_Data', layer = "Bokspits_2_Planet_Grid_95")
VegpolyB2 <- read_sf(dsn = "E:/Glenn/Botswana/Final_Drone_Survey_Data/Bokspits_2", layer = "Bokspits_2_Field_data_points_All_b30")
VegpolyB1 <- read_sf(dsn = "E:/Glenn/Botswana/R_Scripts/Glenn-Prosopis-ML/data_in", layer = "Planet_equal_class_size_270_train_V2")

Bokspits_2_Planet_grid_SF <- VegpolyB2 

#---2. Extract data for Grid


Bokspits_2_Planet_Drone_Extract <- exact_extract(Bokspits_2_NDVI,Bokspits_2_Planet_grid_SF,"mean")
names(Bokspits_2_Planet_Drone_Extract) <- c('NDVI_Drone')
Bokspits_2_Planet_Drone_Extract_DF <-bind_cols(Bokspits_2_Planet_grid_SF,Bokspits_2_Planet_Drone_Extract)
Bokspits_2_Planet_Drone_Extract_DF <- dplyr::mutate(Bokspits_2_Planet_Drone_Extract_DF, NDVI_Drone = Bokspits_2_Planet_Drone_Extract)


Bokspits_2_Planet_Extract <- exact_extract(Planet_NDVI,Bokspits_2_Planet_grid_SF,"mean")
names(Bokspits_2_Planet_Extract) <- c('NDVI_Planet')
Bokspits_2_Planet_Drone_Extract_DF <- dplyr::mutate(Bokspits_2_Planet_Drone_Extract_DF, NDVI_Planet = Bokspits_2_Planet_Extract)

Bokspits_2_Planet_Extract <- exact_extract(Bokspits_2_MSAVI2,Bokspits_2_Planet_grid_SF,"mean")
names(Bokspits_2_Planet_Extract) <- c('MSAVI2_Drone')
Bokspits_2_Planet_Drone_Extract_DF <- dplyr::mutate(Bokspits_2_Planet_Drone_Extract_DF, MSAVI2_Drone = Bokspits_2_Planet_Extract)

Bokspits_2_Planet_Extract <- exact_extract(Planet_MSAVI2,Bokspits_2_Planet_grid_SF,"mean")
names(Bokspits_2_Planet_Extract) <- c('MSAVI2_Planet')
Bokspits_2_Planet_Drone_Extract_DF <- dplyr::mutate(Bokspits_2_Planet_Drone_Extract_DF, MSAVI2_Planet = Bokspits_2_Planet_Extract)


Bokspits_2_Planet_Extract <- exact_extract(Bokspits_2_MTVI,Bokspits_2_Planet_grid_SF,"mean")
names(Bokspits_2_Planet_Extract) <- c('MTVI_Drone')
Bokspits_2_Planet_Drone_Extract_DF <- dplyr::mutate(Bokspits_2_Planet_Drone_Extract_DF, MTVI_Drone = Bokspits_2_Planet_Extract)

Bokspits_2_Planet_Extract <- exact_extract(Planet_MTVI,Bokspits_2_Planet_grid_SF,"mean")
names(Bokspits_2_Planet_Extract) <- c('MTVI_Planet')
Bokspits_2_Planet_Drone_Extract_DF <- dplyr::mutate(Bokspits_2_Planet_Drone_Extract_DF, MTVI_Planet = Bokspits_2_Planet_Extract)

Bokspits_2_Planet_Extract <- exact_extract(Bokspits_2_SAVI,Bokspits_2_Planet_grid_SF,"mean")
names(Bokspits_2_Planet_Extract) <- c('SAVI_Drone')
Bokspits_2_Planet_Drone_Extract_DF <- dplyr::mutate(Bokspits_2_Planet_Drone_Extract_DF, SAVI_Drone = Bokspits_2_Planet_Extract)

Bokspits_2_Planet_Extract <- exact_extract(Planet_SAVI,Bokspits_2_Planet_grid_SF,"mean")
names(Bokspits_2_Planet_Extract) <- c('SAVI_Planet')
Bokspits_2_Planet_Drone_Extract_DF <- dplyr::mutate(Bokspits_2_Planet_Drone_Extract_DF, SAVI_Planet = Bokspits_2_Planet_Extract)


p3_NDVI <- ggplot(Bokspits_2_Planet_Drone_Extract_DF,aes (NDVI_Drone,NDVI_Planet, colour=factor(Type))) +
  geom_point() + geom_abline(intercept = 0, slope = 1, col='grey' )+# +scale_color_manual(name = "Vegetation Type", labels = c("Prosopis", "Bare sand","Grass","Gnidia","Vachellia erioloba","Rhigosum trichotomum"),values=c("green", "yellow","grey","brown","orange","blue"))+
  theme_fancy()+  coord_fixed(xlim=c(0,0.8),ylim=c(0,0.8))
plot(p3_NDVI)

