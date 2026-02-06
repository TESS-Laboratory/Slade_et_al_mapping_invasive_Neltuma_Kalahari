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

# read in Planet_grid with cells that are 95% one veg class type when available


# ----3.Read in Stacked images ----

S2 <- rast("E:/Glenn/Botswana/Satellite_Data/S2/S2_stack.TIF")


S2

S2_RE_20M <- rast ("E:/Glenn/Botswana/Satellite_Data/S2/files/T34JDR_20220907T081609_B07_clip.tif")


S2_RE <- resample(S2_RE_20M , S2, method="bilinear")# resample the lower resolution image to the higher resolution


#----4. Calculate Vegetation indices for S2-----

#Import Planet bands


plot (S2)

S2_BLUE <- S2$S2_stack_1
S2_GREEN <- S2$S2_stack_2
S2_RED <- S2$S2_stack_3
S2_NIR <- S2$S2_stack_4



#--- Calculate SAVI,MSAVI, MSAVI2, MTVI-----

#S2 MSAVI

S2_MSAVI = S2_NIR + 0.5 - (0.5 * sqrt((2 * S2_NIR + 1)^2 - 8 * (S2_NIR - (2 * S2_RED))))

plot (S2_MSAVI)

writeRaster(S2_MSAVI,"E:/Glenn/Botswana/Satellite_Data/S2/S2_MSAVI.tif",overwrite=TRUE)


#S2 NDVI
S2_NDVI = (S2_NIR - S2_RED)/(S2_NIR +S2_RED)

plot (S2_NDVI)
writeRaster(S2_NDVI,"E:/Glenn/Botswana/Satellite_Data/S2/S2_NDVI.tif", overwrite=TRUE)

#S2 REVI

S2_REVI = (S2_RE - S2_RED)/(S2_RE +S2_RED)
plot (S2_REVI)
writeRaster(S2_REVI,"E:/Glenn/Botswana/Satellite_Data/S2/S2_REVI.tif", overwrite=TRUE)

#S2_tinel SAVI

L=0.5
S2_SAVI= (1 + L)*(S2_NIR - S2_RED)/(S2_NIR + S2_RED + L)
plot (S2_SAVI)
 writeRaster(S2_SAVI,"E:/Glenn/Botswana/Satellite_Data/S2/S2_SAVI.tif", overwrite=TRUE)

#MSAVI2

S2_MSAVI2 = (2 * S2_NIR + 1 - sqrt( (2 * S2_NIR + 1)^2 - 8 * (S2_NIR - S2_RED) )) / 2 

plot(S2_MSAVI2)

writeRaster(S2_MSAVI2,"E:/Glenn/Botswana/Satellite_Data/S2/S2_MSAVI2.tif", overwrite=TRUE)

#MTVI
#Modified Triangular Vegetation Index 2 (MTVI)

S2_MTVI = 1.5 * (1.2 * (S2_NIR - S2_GREEN) - 2.5 * (S2_RED - S2_GREEN)) /  sqrt( (2 * S2_NIR + 1)^2 - (6 * S2_NIR - 5 * sqrt(S2_RED) - 0.5) )
plot(S2_MTVI)

writeRaster(S2_MTVI,"E:/Glenn/Botswana/Satellite_Data/S2/S2_MTVI.tif", overwrite=TRUE)





