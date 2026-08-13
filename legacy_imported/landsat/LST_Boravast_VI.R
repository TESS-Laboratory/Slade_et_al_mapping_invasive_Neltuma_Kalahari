# ===========================================================================
# IMPORTED - not part of the original published archive.
#
#   source repo   slade-prosopis
#   source path   Landsat/LST_Boravast_VI.R
#   ref           main @ 671e56f
#   sha256        564e53967c3f7b06eab3037877c2359d72a78a6c55d86dd6523ea1df3d1afe75
#
#   why           Landsat arm. Produces the 30 m cover table used by the optional fourth panel of the Figure 5 script.
#
# Content below is VERBATIM and does not run as-is: it depends on Windows
# absolute paths under E:/Glenn/Botswana/ and on archived packages (rgeos,
# rgdal), and calls windowsFonts(). It is kept as the reference
# implementation to port into the targets pipeline, not to execute.
# See refactor-findings.md and audit/source-recovery-map.md.
# ===========================================================================

### Script for calculating NDVI for PlanetScope image data

#-----0. Library-----

  ### Script for calculating NDVI for Sentinel2 image data
  
  #-----0. Library-----
  {
    # library(terra)
    library(tidyverse)
    library(viridis)
    library(rgdal)
    library(lubridate)
    library(RColorBrewer)
    library(ggplot2)
    library(raster)
    library(MASS)
    library(splines)
    library(rgeos)
    library(gridExtra)
    library(DescTools)
    library(sf)
    library(exactextractr)
    library(writexl)
    library(terra)
  }
  #-----1.Theme--------
  
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
  
  #-----2.Read in ROI shape files------
  
 Clip2 <- read_sf(dsn = 'E:/Glenn/Botswana/R_Scripts/Glenn-Prosopis-ML/data_in/WV2', layer = "WV2_clip")
 Clip <- vect(Clip2)
 Clip
  # ----3.Read in Stacked images ----
  
  LS8 <- rast("E:/Glenn/Botswana/Satellite_Data/LST/LS8/LS8_Large_stack.tif")
  LS8
  
  LS_Slope <- rast("E:/Glenn/Botswana/Satellite_Data/Trend_Image_Data/Landsat/Trend_Img_NDVI_2000_2022_bi_dry.tif")
  LS_Slope 
  
  LS_Slope  <-terra::project(  LS_Slope, y="EPSG:32734")
  LS8  <-terra::project(  LS8, y="EPSG:32734")
  LS8
  LS_Slope 
  
  LS_Change_Mapper <- rast("E:/Glenn/Botswana/Satellite_Data/Trend_Image_Data/Landsat/lt-gee_growth_map_2000_2022_mid.tif")
  LS_Change_Mapper
  LS_Change_Mapper <-terra::project(  LS_Change_Mapper, y="EPSG:32734")
  LS_Change_Mapper
  
  #Clip Image stack
 LS8_crop <- crop(LS8,Clip )
LS8_clip <- mask(LS8_crop ,Clip)
  plot(LS8_clip)

  writeRaster(LS8_clip,"E:/Glenn/Botswana/Satellite_Data/LST/LS8/LS8_stack_crop.tif",overwrite=TRUE)
  
  
  #clip Time series products
  
  LS8_crop <- crop(LS_Slope,Clip )
  LS8_clip <- mask(LS8_crop ,Clip)
  plot(LS8_clip)
  
  writeRaster(LS8_clip,"E:/Glenn/Botswana/Satellite_Data/LST/LS8/LSTrend_2000_2022_bi_dry_crop.tif",overwrite=TRUE)
  
  LS8_crop <- crop(LS_Change_Mapper,Clip )
  LS8_clip <- mask(LS8_crop ,Clip)
  plot(LS8_clip)
  
  writeRaster(LS8_clip,"E:/Glenn/Botswana/Satellite_Data/LST/LS8/Change_Mapper_2000_2022_dry_crop.tif",overwrite=TRUE)
  
  LS8_CM_mag <- LS8_clip$mag
  
  writeRaster(LS8_CM_mag,"E:/Glenn/Botswana/Satellite_Data/LST/LS8/Change_Mapper_2000_2022_dry_crop_mag.tif",overwrite=TRUE)
  
  plot (LS8_CM_mag)
  
  #----4. Calculate Vegetation indices for LS8-----
  
  #Import Planet bands
  
  
  plot (LS8)
  
  LS8_BLUE <- LS8$LS8_Large_Stack_1
  LS8_GREEN <- LS8$LS8_Large_Stack_2
  LS8_RED <- LS8$LS8_Large_Stack_3
  LS8_NIR <- LS8$LS8_Large_Stack_4
  
  
  
  #--- Calculate SAVI,MSAVI, MSAVI2, MTVI-----
  
  #LS8 MSAVI
  
  LS8_MSAVI = LS8_NIR + 0.5 - (0.5 * sqrt((2 * LS8_NIR + 1)^2 - 8 * (LS8_NIR - (2 * LS8_RED))))
  
  plot (LS8_MSAVI)
  
  writeRaster(LS8_MSAVI,"E:/Glenn/Botswana/Satellite_Data/LST/LS8/LS8_MSAVI.tif",overwrite=TRUE)
  
  LS8_crop <- crop(LS8_MSAVI,Clip )
  LS8_clip <- mask(LS8_crop ,Clip)
  plot(LS8_clip)
  
  writeRaster(LS8_clip,"E:/Glenn/Botswana/Satellite_Data/LST/LS8/LS8_MSAVI_crop.tif",overwrite=TRUE)
  
  
  #LS8 NDVI
  LS8_NDVI = (LS8_NIR - LS8_RED)/(LS8_NIR +LS8_RED)
  
  plot (LS8_NDVI)
  writeRaster(LS8_NDVI,"E:/Glenn/Botswana/Satellite_Data/LST/LS8/LS8_NDVI.tif", overwrite=TRUE)
  
  LS8_crop <- crop(LS8_NDVI,Clip )
  LS8_clip <- mask(LS8_crop ,Clip)
  plot(LS8_clip)
  
  writeRaster(LS8_clip,"E:/Glenn/Botswana/Satellite_Data/LST/LS8/LS8_NDVI_crop.tif",overwrite=TRUE)
  
  
   #LS8_tinel SAVI
  
  L=0.5
  LS8_SAVI= (1 + L)*(LS8_NIR - LS8_RED)/(LS8_NIR + LS8_RED + L)
  plot (LS8_SAVI)
  writeRaster(LS8_SAVI,"E:/Glenn/Botswana/Satellite_Data/LST/LS8/LS8_SAVI.tif", overwrite=TRUE)
  
  LS8_crop <- crop(LS8_SAVI,Clip )
  LS8_clip <- mask(LS8_crop ,Clip)
  plot(LS8_clip)
  
  writeRaster(LS8_clip,"E:/Glenn/Botswana/Satellite_Data/LST/LS8/LS8_SAVI_crop.tif",overwrite=TRUE)
  
  
  #MSAVI2
  
  LS8_MSAVI2 = (2 * LS8_NIR + 1 - sqrt( (2 * LS8_NIR + 1)^2 - 8 * (LS8_NIR - LS8_RED) )) / 2 
  
  plot(LS8_MSAVI2)
  
  writeRaster(LS8_MSAVI2,"E:/Glenn/Botswana/Satellite_Data/LST/LS8/LS8_MSAVI2.tif", overwrite=TRUE)
  
  LS8_crop <- crop(LS8_MSAVI2,Clip )
  LS8_clip <- mask(LS8_crop ,Clip)
  plot(LS8_clip)
  
  writeRaster(LS8_clip,"E:/Glenn/Botswana/Satellite_Data/LST/LS8/LS8_MSAVI2_crop.tif",overwrite=TRUE)
  
  #MTVI
  #Modified Triangular Vegetation Index 2 (MTVI)
  
  LS8_MTVI = 1.5 * (1.2 * (LS8_NIR - LS8_GREEN) - 2.5 * (LS8_RED - LS8_GREEN)) /  sqrt( (2 * LS8_NIR + 1)^2 - (6 * LS8_NIR - 5 * sqrt(LS8_RED) - 0.5) )
  plot(LS8_MTVI)
  
  writeRaster(LS8_MTVI,"E:/Glenn/Botswana/Satellite_Data/LST/LS8/LS8_MTVI.tif", overwrite=TRUE)
  
  LS8_crop <- crop(LS8_MTVI,Clip )
  LS8_clip <- mask(LS8_crop ,Clip)
  plot(LS8_clip)
  
  writeRaster(LS8_clip,"E:/Glenn/Botswana/Satellite_Data/LST/LS8/LS8_MTVI_crop.tif",overwrite=TRUE)
  
  
