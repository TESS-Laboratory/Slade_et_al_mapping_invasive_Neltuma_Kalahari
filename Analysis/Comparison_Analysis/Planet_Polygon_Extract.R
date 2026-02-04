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

#----1. Read in files----

Bokspits_1_clipper <- read_sf(dsn = 'E:/Glenn/Botswana/Final_Drone_Survey_Data/Bokspits_1', layer = "Bokspits_1_clip")
Bokspits_1_clip <- vect(Bokspits_1_clipper)

Planet <-  rast("E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope/Bokspits_2022_09_NDVI.tif")
Bokspits_1_NDVI <- rast("E:/Glenn/Botswana/Pix4d/Bokspits_1_MS/4_index/indices/ndvi/Bokspits_1_MS_index_ndvi.tif")

Planet1 <- crop(Planet,Bokspits_1_clip )
Planet_Bokspits <- mask(Planet1,Bokspits_1_clip )
plot(Planet_Bokspits)

## NB You cant use exact extract with SpatVector so re-importing it a a spatial DF
Bokspits_1_Planet_grid_SF <- read_sf(dsn = 'E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope', layer = "Bokspits_1_Planet_grid")


#---2. Extract data for Grid


Bokspits_1_Planet_Drone_Extract <- exact_extract(Bokspits_1_NDVI,Bokspits_1_Planet_grid_SF,"mean")
names(Bokspits_1_Planet_Drone_Extract) <- c('NDVI_Drone')
Bokspits_1_Planet_Drone_Extract_DF <-bind_cols(Bokspits_1_Planet_grid_SF,Bokspits_1_Planet_Drone_Extract)
Bokspits_1_Planet_Drone_Extract_DF <- dplyr::mutate(Bokspits_1_Planet_Drone_Extract_DF, NDVI_Drone = Bokspits_1_Planet_Drone_Extract)


Bokspits_1_Planet_Drone_Extract <- exact_extract(Planet_Bokspits,Bokspits_1_Planet_grid_SF,"mean")
names(Bokspits_1_Planet_Drone_Extract) <- c('NDVI_Planet')
Bokspits_1_Planet_Drone_Extract_DF <- dplyr::mutate(Bokspits_1_Planet_Drone_Extract_DF, NDVI_Planet = Bokspits_1_Planet_Drone_Extract)

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

B1_NDVI <- ggplot(df) +
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
  ggtitle("Comparison of Drone NDVI with Planet NDVI")+
  #theme(aspect.ratio=1)+
  xlab('Drone NDVI')+
  ylab('Planet NDVI')+
  #coord_equal(ratio=1)
  coord_fixed(xlim=c(0,0.8),ylim=c(0,0.8))
plot(B1_NDVI)

ggsave(
  B1_NDVI,
  # filename = "/plots/test.png",
  filename = "E:/Glenn/Botswana/R_Scripts/slade-prosopis/output_data/Plots/B1_Drone_vs_Planet_NDVI.png",
  width = 10,
  height = 10,
  units = "cm"
)

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

B2_NDVI <- ggplot(df) +
  geom_smooth(aes(x, y,col='black',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +  + geom_density_2d_filled(aes(x,y),alpha = 0.55)+
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
plot(B2_NDVI)

ggsave(
  B2_NDVI,
  # filename = "/plots/test.png",
  filename = "E:/Glenn/Botswana/R_Scripts/slade-prosopis/output_data/Plots/B1_Drone_vs_Planet_NDVI_test.png",
  width = 10,
  height = 10,
  units = "cm"
)
