# ===========================================================================
# IMPORTED - not part of the original published archive.
#
#   source repo   slade-prosopis
#   source path   Aggregate/Hex_Grid_Extract_WV2.R
#   ref           main @ 671e56f
#   sha256        269a94b278c696530db5f8e3e0a3317fd94b79d5a546bb7c4d46fd8195085ee5
#
#   why           Canonical hex extraction behind Fig 8A and Table 1. NB reads a Random Forest product, not the mlr3 output Fig 6C reports.
#
# Content below is VERBATIM and does not run as-is: it depends on Windows
# absolute paths under E:/Glenn/Botswana/ and on archived packages (rgeos,
# rgdal), and calls windowsFonts(). It is kept as the reference
# implementation to port into the targets pipeline, not to execute.
# See refactor-findings.md and audit/source-recovery-map.md.
# ===========================================================================

### Script to Extract Data from RF Classifications based on Hex polygons to aggregate data for presentation
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
library(remotes)
library(cowplot)


#----1. Read in files for WV2 ----


WV2_RF_hex <- rast("E:/Glenn/Botswana/Satellite_Data/WV2/merged_mosaic/RF_WV2_all_train_val_combined_b30_additional_WV2_merged_mosaic.tif")

NDVITrend<- rast("E:/Glenn/Botswana/Satellite_Data/Trend_Image_Data/Landsat/Trend_Img_NDVI_1984_2000_bi_dry.tif")
NDVITrend2<- rast("E:/Glenn/Botswana/Satellite_Data/Trend_Image_Data/Landsat/Trend_Img_NDVI_2000_2022_bi_dry.tif")


#LST <-terra::project(NDVITrend, y="EPSG:32734")
NDVI_trend  <-terra::project(NDVITrend, y="EPSG:32734")
NDVI_trend2  <-terra::project(NDVITrend, y="EPSG:32734")

#hex250 <-terra::project(NDVITrend, y="EPSG:32734")
## NB You cant use exact extract with SpatVector so re-importing it a a spatial DF
hex100_Grid_SF <- read_sf(dsn = 'E:/Glenn/Botswana/GIS_aggregate', layer = "hex_100m_wv2_wide_clip_minus_corner")

hex250_Grid_SF <- read_sf(dsn = 'E:/Glenn/Botswana/GIS_aggregate', layer = "hex_250m_wv2_wide_clip")
hex500_Grid_SF <- read_sf(dsn = 'E:/Glenn/Botswana/GIS_aggregate', layer = "hex_500m_wv2_wide_clip")
grid30_SF<- read_sf(dsn = 'E:/Glenn/Botswana/GIS_aggregate', layer = "grid_30m_wv2_wide")

#---2. Extract data for 250m sized polygons


WV2_RF_hex_Extract <- exact_extract(WV2_RF_hex,hex250_Grid_SF,"mode")
names(WV2_RF_hex_Extract) <- c('mode')
WV2_RF_hex_Extract_DF <-bind_cols(hex250_Grid_SF,WV2_RF_hex_Extract)

WV2_RF_hex_Extract <- exact_extract(WV2_RF_hex,hex250_Grid_SF,"variance")
names(WV2_RF_hex_Extract) <- c('variance')
WV2_RF_hex_Extract_DF <- dplyr::mutate(WV2_RF_hex_Extract_DF, variance = WV2_RF_hex_Extract)

WV2_RF_hex_Extract <- exact_extract(WV2_RF_hex,hex250_Grid_SF,"majority" )
names(WV2_RF_hex_Extract) <- c('majority')
WV2_RF_hex_Extract_DF <- dplyr::mutate(WV2_RF_hex_Extract_DF, majority = WV2_RF_hex_Extract)

WV2_RF_hex_Extract <- exact_extract(WV2_RF_hex,hex250_Grid_SF,"variety" )
names(WV2_RF_hex_Extract) <- c('variety')
WV2_RF_hex_Extract_DF <- dplyr::mutate(WV2_RF_hex_Extract_DF, variety = WV2_RF_hex_Extract)

WV2_RF_hex_Extract <- exact_extract(WV2_RF_hex,hex250_Grid_SF,"count" )
names(WV2_RF_hex_Extract) <- c('count')
WV2_RF_hex_Extract_DF <- dplyr::mutate(WV2_RF_hex_Extract_DF, count = WV2_RF_hex_Extract)

WV2_RF_hex_Extract <- exact_extract(WV2_RF_hex,hex250_Grid_SF,"frac" )# calculates fraction cover for each classification value
#a column is added with the FVC for each vegetation type frac1, frac2 etc...
WV2_RF_hex_Extract_DF <- dplyr::mutate(WV2_RF_hex_Extract_DF, WV2_RF_hex_Extract)



st_write(WV2_RF_hex_Extract_DF, dsn = "E:/Glenn/Botswana/GIS_aggregate/hex_250m_extract.shp")
saveRDS(WV2_RF_hex_Extract_DF, file = "E:/Glenn/Botswana/GIS_aggregate/hex_250m_extract.rds")
write.csv(WV2_RF_hex_Extract_DF, file = "E:/Glenn/Botswana/GIS_aggregate/hex_250m_extract.csv")

# Now Repeat for 500m szied hex polygons

WV2_RF_hex_Extract <- exact_extract(WV2_RF_hex,hex500_Grid_SF,"mode")
names(WV2_RF_hex_Extract) <- c('mode')
WV2_RF_hex_Extract_DF <-bind_cols(hex500_Grid_SF,WV2_RF_hex_Extract)

WV2_RF_hex_Extract <- exact_extract(WV2_RF_hex,hex500_Grid_SF,"variance")
names(WV2_RF_hex_Extract) <- c('variance')
WV2_RF_hex_Extract_DF <- dplyr::mutate(WV2_RF_hex_Extract_DF, variance = WV2_RF_hex_Extract)

WV2_RF_hex_Extract <- exact_extract(WV2_RF_hex,hex500_Grid_SF,"majority" )
names(WV2_RF_hex_Extract) <- c('majority')
WV2_RF_hex_Extract_DF <- dplyr::mutate(WV2_RF_hex_Extract_DF, majority = WV2_RF_hex_Extract)

WV2_RF_hex_Extract <- exact_extract(WV2_RF_hex,hex500_Grid_SF,"variety" )
names(WV2_RF_hex_Extract) <- c('variety')
WV2_RF_hex_Extract_DF <- dplyr::mutate(WV2_RF_hex_Extract_DF, variety = WV2_RF_hex_Extract)

WV2_RF_hex_Extract <- exact_extract(WV2_RF_hex,hex500_Grid_SF,"count" )
names(WV2_RF_hex_Extract) <- c('count')
WV2_RF_hex_Extract_DF <- dplyr::mutate(WV2_RF_hex_Extract_DF, count = WV2_RF_hex_Extract)

WV2_RF_hex_Extract <- exact_extract(WV2_RF_hex,hex500_Grid_SF,"frac" )# calculates fraction cover for each classification value
#a column is added with the FVC for each vegetation type frac1, frac2 etc...
WV2_RF_hex_Extract_DF <- dplyr::mutate(WV2_RF_hex_Extract_DF, WV2_RF_hex_Extract)



st_write(WV2_RF_hex_Extract_DF, dsn = "E:/Glenn/Botswana/GIS_aggregate/hex_500m_extract.shp")
saveRDS(WV2_RF_hex_Extract_DF, file = "E:/Glenn/Botswana/GIS_aggregate/hex_500m_extract.rds")
write.csv(WV2_RF_hex_Extract_DF, file = "E:/Glenn/Botswana/GIS_aggregate/hex_500m_extract.csv")

# Now repeat for 30m grid

WV2_RF_hex_Extract <- exact_extract(WV2_RF_hex,grid30_SF,"mode")
names(WV2_RF_hex_Extract) <- c('mode')
WV2_RF_hex_Extract_DF <-bind_cols(grid30_SF,WV2_RF_hex_Extract)

WV2_RF_hex_Extract <- exact_extract(WV2_RF_hex,grid30_SF,"variance")
names(WV2_RF_hex_Extract) <- c('variance')
WV2_RF_hex_Extract_DF <- dplyr::mutate(WV2_RF_hex_Extract_DF, variance = WV2_RF_hex_Extract)

WV2_RF_hex_Extract <- exact_extract(WV2_RF_hex,grid30_SF,"majority" )
names(WV2_RF_hex_Extract) <- c('majority')
WV2_RF_hex_Extract_DF <- dplyr::mutate(WV2_RF_hex_Extract_DF, majority = WV2_RF_hex_Extract)

WV2_RF_hex_Extract <- exact_extract(WV2_RF_hex,grid30_SF,"variety" )
names(WV2_RF_hex_Extract) <- c('variety')
WV2_RF_hex_Extract_DF <- dplyr::mutate(WV2_RF_hex_Extract_DF, variety = WV2_RF_hex_Extract)

WV2_RF_hex_Extract <- exact_extract(WV2_RF_hex,grid30_SF,"count" )
names(WV2_RF_hex_Extract) <- c('count')
WV2_RF_hex_Extract_DF <- dplyr::mutate(WV2_RF_hex_Extract_DF, count = WV2_RF_hex_Extract)

WV2_RF_hex_Extract <- exact_extract(WV2_RF_hex,grid30_SF,"frac" )# calculates fraction cover for each classification value
#a column is added with the FVC for each vegetation type frac1, frac2 etc...
WV2_RF_hex_Extract_DF <- dplyr::mutate(WV2_RF_hex_Extract_DF, WV2_RF_hex_Extract)



st_write(WV2_RF_hex_Extract_DF, dsn = "E:/Glenn/Botswana/GIS_aggregate/grid_30m_extract.shp")
saveRDS(WV2_RF_hex_Extract_DF, file = "E:/Glenn/Botswana/GIS_aggregate/grid_30m_extract.rds")
write.csv(WV2_RF_hex_Extract_DF, file = "E:/Glenn/Botswana/GIS_aggregate/grid_30m_extract.csv")

# Extract for 100m polygons



WV2_RF_hex_Extract <- exact_extract(WV2_RF_hex,hex100_Grid_SF,"mode")
names(WV2_RF_hex_Extract) <- c('mode')
WV2_RF_hex_Extract_DF <-bind_cols(hex100_Grid_SF,WV2_RF_hex_Extract)

WV2_RF_hex_Extract <- exact_extract(WV2_RF_hex,hex100_Grid_SF,"variance")
names(WV2_RF_hex_Extract) <- c('variance')
WV2_RF_hex_Extract_DF <- dplyr::mutate(WV2_RF_hex_Extract_DF, variance = WV2_RF_hex_Extract)

WV2_RF_hex_Extract <- exact_extract(WV2_RF_hex,hex100_Grid_SF,"majority" )
names(WV2_RF_hex_Extract) <- c('majority')
WV2_RF_hex_Extract_DF <- dplyr::mutate(WV2_RF_hex_Extract_DF, majority = WV2_RF_hex_Extract)

WV2_RF_hex_Extract <- exact_extract(WV2_RF_hex,hex100_Grid_SF,"variety" )
names(WV2_RF_hex_Extract) <- c('variety')
WV2_RF_hex_Extract_DF <- dplyr::mutate(WV2_RF_hex_Extract_DF, variety = WV2_RF_hex_Extract)

WV2_RF_hex_Extract <- exact_extract(WV2_RF_hex,hex100_Grid_SF,"count" )
names(WV2_RF_hex_Extract) <- c('count')
WV2_RF_hex_Extract_DF <- dplyr::mutate(WV2_RF_hex_Extract_DF, count = WV2_RF_hex_Extract)

WV2_RF_hex_Extract <- exact_extract(WV2_RF_hex,hex100_Grid_SF,"frac" )# calculates fraction cover for each classification value
#a column is added with the FVC for each vegetation type frac1, frac2 etc...
WV2_RF_hex_Extract_DF <- dplyr::mutate(WV2_RF_hex_Extract_DF, WV2_RF_hex_Extract)

WV2_RF_hex_Extract <- exact_extract(NDVITrend,hex100_Grid_SF,"mean" )
names(WV2_RF_hex_Extract) <- c('NDVI_84')
WV2_RF_hex_Extract_DF <- dplyr::mutate(WV2_RF_hex_Extract_DF, NDVI_84 = WV2_RF_hex_Extract)

WV2_RF_hex_Extract <- exact_extract(NDVITrend2,hex100_Grid_SF,"mean" )
names(WV2_RF_hex_Extract) <- c('NDVI_22')
WV2_RF_hex_Extract_DF <- dplyr::mutate(WV2_RF_hex_Extract_DF, NDVI_22 = WV2_RF_hex_Extract)



st_write(WV2_RF_hex_Extract_DF, dsn = "E:/Glenn/Botswana/GIS_aggregate/hex_100m_wide_extract_dry.shp")
saveRDS(WV2_RF_hex_Extract_DF, file = "E:/Glenn/Botswana/GIS_aggregate/hex_100m_extract_dry.rds")
write.csv(WV2_RF_hex_Extract_DF, file = "E:/Glenn/Botswana/GIS_aggregate/hex_100m_extract_dry.csv")

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

# Plotting NDVI from Drone vs LST Image Data

WV2_RF_hex_Extract_DF2 <- na.omit(WV2_RF_hex_Extract_DF)

Y <- mean(WV2_RF_hex_Extract_DF2$NDVI_22)

x <- as.vector(WV2_RF_hex_Extract_DF2$frac_1)
y <- as.vector(WV2_RF_hex_Extract_DF2$NDVI_22-Y)
df <- data.frame(x = x, y = y,
                 d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))
# Calculate Total Least Squares Regression (extracted from base-R PCA function)
#df2 <- na.omit(df)

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
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.3,y=-1),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.0)+
  geom_text(aes(x=0.3,y=-1.25),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.0)+
  geom_text(aes(x=0.3,y=-1.5),label=ccc,hjust='left', size=2.0)+
  geom_text(aes(x=0.3,y=-1.75),label=equation,hjust='left', size=2.0)+
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1, col='grey' ) +
  ggtitle("Comparison of Fractional Cover Prosopis \n 100m hex grid with Dry season NDVI Trend 2000-2022")+
  #theme(aspect.ratio=1)+
  xlab('Fractional Cover Prosopis - 100m hexgrid')+
  ylab('NDVI Trend 2000-2022 % (-mean)')+
  #coord_equal(ratio=1)
#  scale_x_continuous(breaks = 0.1, labels, limits = c(0,0.5))
  coord_fixed(ratio = 0.1, xlim=c(0,0.5),ylim=c(-2,4.5))

plot(p1_NDVI)



ggsave2(
  "E:/Glenn/Botswana/R_Scripts/slade-prosopis/output_data/Plots/100mhex_ndvi_trend_2022_fractional_cover_comparison.png",
  plot = p1_NDVI,
  device = NULL,
  path = NULL,
  scale = 1,
  width = 170,
  height = 260,
  units =  "mm",
  dpi = 300,
  limitsize = TRUE
)
