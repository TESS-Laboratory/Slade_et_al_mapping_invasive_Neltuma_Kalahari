### Script for calculating NDVI for PlanetScope image data

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

# read in Planet_grid with cells that are 95% one veg class type when available


# ----3.Read in Stacked images ----

WV2 <- rast("E:/Glenn/Botswana/Satellite_Data/WV2/merged_mosaic/WV2_merged_corrected.TIF")

WV2



#----4. Calculate Vegetation indices for WV2-----

#Import Planet bands


plot (WV2)

WV2_BLUE <- WV2$lyr1
WV2_GREEN <- WV2$lyr2
WV2_RED <- WV2$lyr3
WV2_NIR <- WV2$lyr4



#--- Calculate SAVI,MSAVI, MSAVI2, MTVI-----

#WV2 MSAVI

WV2_MSAVI = WV2_NIR + 0.5 - (0.5 * sqrt((2 * WV2_NIR + 1)^2 - 8 * (WV2_NIR - (2 * WV2_RED))))

plot (WV2_MSAVI)

writeRaster(WV2_MSAVI,"E:/Glenn/Botswana/Satellite_Data/WV2/merged_mosaic/WV2_MSAVI.tif",overwrite=TRUE)


#WV2 NDVI
WV2_NDVI = (WV2_NIR - WV2_RED)/(WV2_NIR +WV2_RED)

plot (WV2_NDVI)
writeRaster(WV2_NDVI,"E:/Glenn/Botswana/Satellite_Data/WV2/merged_mosaic/WV2_NDVI.tif", overwrite=TRUE)


#WV2_tinel SAVI

L=0.5
WV2_SAVI= (1 + L)*(WV2_NIR - WV2_RED)/(WV2_NIR + WV2_RED + L)
plot (WV2_SAVI)
writeRaster(WV2_SAVI,"E:/Glenn/Botswana/Satellite_Data/WV2/merged_mosaic/WV2_SAVI.tif", overwrite=TRUE)

#MSAVI2

WV2_MSAVI2 = (2 * WV2_NIR + 1 - sqrt( (2 * WV2_NIR + 1)^2 - 8 * (WV2_NIR - WV2_RED) )) / 2 

plot(WV2_MSAVI2)

writeRaster(WV2_MSAVI2,"E:/Glenn/Botswana/Satellite_Data/WV2/merged_mosaic/WV2_MSAVI2.tif", overwrite=TRUE)

#MTVI
#Modified Triangular Vegetation Index 2 (MTVI)

WV2_MTVI = 1.5 * (1.2 * (WV2_NIR - WV2_GREEN) - 2.5 * (WV2_RED - WV2_GREEN)) /  sqrt( (2 * WV2_NIR + 1)^2 - (6 * WV2_NIR - 5 * sqrt(WV2_RED) - 0.5) )
plot(WV2_MTVI)

writeRaster(WV2_MTVI,"E:/Glenn/Botswana/Satellite_Data/WV2/merged_mosaic/WV2_MTVI.tif", overwrite=TRUE)



writeRaster(WV2_BLUE,"E:/Glenn/Botswana/Satellite_Data/WV2/merged_mosaic/WV2_merged_corrected_blue.tif", overwrite=TRUE)
writeRaster(WV2_GREEN,"E:/Glenn/Botswana/Satellite_Data/WV2/merged_mosaic/WV2_merged_corrected_green.tif", overwrite=TRUE)
writeRaster(WV2_RED,"E:/Glenn/Botswana/Satellite_Data/WV2/merged_mosaic/WV2_merged_corrected_red.tif", overwrite=TRUE)
writeRaster(WV2_NIR,"E:/Glenn/Botswana/Satellite_Data/WV2/merged_mosaic/WV2_merged_corrected_nir.tif", overwrite=TRUE)

