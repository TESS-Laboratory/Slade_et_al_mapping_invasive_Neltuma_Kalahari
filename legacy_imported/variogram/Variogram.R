# ===========================================================================
# IMPORTED - not part of the original published archive.
#
#   source repo   slade-prosopis
#   source path   Variogram.R
#   ref           main @ 671e56f
#   sha256        cada99fa570ef631bd5eedaa54dad2520dc31811b3e2d738f4a50a25f7b70b29
#
#   why           Empirical variogram; relevant to choosing a defensible spcv_block range if block CV is adopted.
#
# Content below is VERBATIM and does not run as-is: it depends on Windows
# absolute paths under E:/Glenn/Botswana/ and on archived packages (rgeos,
# rgdal), and calls windowsFonts(). It is kept as the reference
# implementation to port into the targets pipeline, not to execute.
# See refactor-findings.md and audit/source-recovery-map.md.
# ===========================================================================

#NDVI Variogram script
# Library
{
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
  library("usdm")
  library (gstat)
}
#----------1.Theme--------

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

#----32. Read in shape file



AreaROI <- readOGR(dsn = 'E:/Glenn/Botswana/Final_Drone_Survey_Data/Bokspits_1', layer = "Bokspits_1_clip")

##-----3. Read in NDVI, crop NDVI-----


NDVI_1 <- raster("E:/Glenn/Botswana/Pix4d/Bokspits_1_MS/4_index/indices/ndvi/Bokspits_1_MS_index_ndvi.tif")
Crop <- crop(NDVI_1,AreaROI)
NDVI <- mask(Crop,AreaROI)
plot (NDVI)

#----4. Variogram-----

NDVI_1.var <- Variogram(NDVI,  size=1000, cutoff = 20000) 
plot(NDVI_1.var)

#plot(SEQ_NDVI.var, cloud=TRUE) 
#plot(SEQ_NDVI.var, box=TRUE)

# fit.variogram(MRE_NDVI.var,fit.sills = TRUE, fit.ranges = TRUE,
#              fit.method = 7, debug.level = 1, warn.if.neg = FALSE, fit.kappa = FALSE)
