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
VegpolyB1 <- read_sf(dsn = 'E:/Glenn/Botswana/Final_Drone_Survey_Data/Bokspits_1', layer = "Bokspits_1_Field_data_points_train_b30")
#VegpolyB2 <-read_sf(dsn = 'E:/Glenn/Botswana/Final_Drone_Survey_Data/Bokspits_2', layer = "Bokspits_2_Field_data_points_train_b30")
#VegpolyB3 <-read_sf(dsn = 'E:/Glenn/Botswana/Final_Drone_Survey_Data/Bokspits_3', layer = "Bokspits_3_Field_data_points_train_b30")
#VegpolyS1 <-read_sf(dsn = 'E:/Glenn/Botswana/Final_Drone_Survey_Data/Struizendam_1', layer = "Struizendam_1_Field_data_points_train_b30")
#VegpolyS2 <-read_sf(dsn = 'E:/Glenn/Botswana/Final_Drone_Survey_Data/Struizendam_2', layer = "Struizendam_2_Field_data_points_train_b30")
#VegpolyS3 <-read_sf(dsn = 'E:/Glenn/Botswana/Final_Drone_Survey_Data/Struizendam_3', layer = "Struizendam_3_Field_data_points_train_b30")
#VegpolyS4 <-read_sf(dsn = 'E:/Glenn/Botswana/Final_Drone_Survey_Data/Struizendam_4', layer = "Struizendam_4_Field_data_points_train_b30")

# ----3.Read in Stacked images ----

Bokspits_2022_01 <- rast("E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope/Bokspits_2022_01_01.tif")
Bokspits_2022_02 <- rast("E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope/Bokspits_2022_02_11.tif")
Bokspits_2022_03 <- rast("E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope/Bokspits_2022_03_01.tif")
Bokspits_2022_04 <- rast("E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope/Bokspits_2022_04_06.tif")
Bokspits_2022_05 <- rast("E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope/Bokspits_2022_05_02.tif")
Bokspits_2022_06 <- rast("E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope/Bokspits_2022_06_02.tif")
Bokspits_2022_07 <- rast("E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope/Bokspits_2022_07_01.tif")
Bokspits_2022_08 <- rast("E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope/Bokspits_2022_08_04.tif")
Bokspits_2022_09 <- rast("E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope/Bokspits_2022_09_07.tif")
Bokspits_2022_10 <- rast("E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope/Bokspits_2022_10_02.tif")
Bokspits_2022_11 <- rast("E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope/Bokspits_2022_11_03.tif")
Bokspits_2022_12 <- rast("E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope/Bokspits_2022_12_02.tif")

#----4. Calculate Vegetation indices for Bokspits_2022_01-----

#Import Planet bands


plot (Bokspits_2022_01)
Bokspits_2022_01_BLUE <- Bokspits_2022_01$blue
Bokspits_2022_01_GREEN <- Bokspits_2022_01$green
Bokspits_2022_01_RED <- Bokspits_2022_01$red
Bokspits_2022_01_NIR <- Bokspits_2022_01$nir



#--- Calculate SAVI,MSAVI, MSAVI2, MTVI-----

#Planet MSAVI

Bokspits_2022_01_MSAVI = Bokspits_2022_01_NIR + 0.5 - (0.5 * sqrt((2 * Bokspits_2022_01_NIR + 1)^2 - 8 * (Bokspits_2022_01_NIR - (2 * Bokspits_2022_01_RED))))

plot (Bokspits_2022_01_MSAVI)

writeRaster(Bokspits_2022_01_MSAVI,"E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope/Bokspits_2022_01_MSAVI.tif",overwrite=TRUE)


#Bokspits_2022_01_tinel NDVI
Bokspits_2022_01_NDVI = (Bokspits_2022_01_NIR - Bokspits_2022_01_RED)/(Bokspits_2022_01_NIR +Bokspits_2022_01_RED)

plot (Bokspits_2022_01_NDVI)
writeRaster(Bokspits_2022_01_NDVI,"E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope/Bokspits_2022_01_NDVI.tif", overwrite=TRUE)


#Bokspits_2022_01_tinel SAVI

L=0.5
Bokspits_2022_01_SAVI= (1 + L)*(Bokspits_2022_01_NIR - Bokspits_2022_01_RED)/(Bokspits_2022_01_NIR + Bokspits_2022_01_RED + L)
plot (Bokspits_2022_01_SAVI)
writeRaster(Bokspits_2022_01_SAVI,"E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope/Bokspits_2022_01_SAVI.tif", overwrite=TRUE)

#MSAVI2

Bokspits_2022_01_MSAVI2 = (2 * Bokspits_2022_01_NIR + 1 - sqrt( (2 * Bokspits_2022_01_NIR + 1)^2 - 8 * (Bokspits_2022_01_NIR - Bokspits_2022_01_RED) )) / 2 

plot(Bokspits_2022_01_MSAVI2)

writeRaster(Bokspits_2022_01_MSAVI2,"E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope/Bokspits_2022_01_MSAVI2.tif", overwrite=TRUE)

#MTVI
#Modified Triangular Vegetation Index 2 (MTVI)

Bokspits_2022_01_MTVI = 1.5 * (1.2 * (Bokspits_2022_01_NIR - Bokspits_2022_01_GREEN) - 2.5 * (Bokspits_2022_01_RED - Bokspits_2022_01_GREEN)) /  sqrt( (2 * Bokspits_2022_01_NIR + 1)^2 - (6 * Bokspits_2022_01_NIR - 5 * sqrt(Bokspits_2022_01_RED) - 0.5) )
plot(Bokspits_2022_01_MTVI)

writeRaster(Bokspits_2022_01_MTVI,"E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope/Bokspits_2022_01_MTVI.tif", overwrite=TRUE)



#----5. Calculate Vegetation indices for Bokspits_2022_02-----

#Import Planet bands


plot (Bokspits_2022_02)
Bokspits_2022_02_BLUE <- Bokspits_2022_02$blue
Bokspits_2022_02_GREEN <- Bokspits_2022_02$green
Bokspits_2022_02_RED <- Bokspits_2022_02$red
Bokspits_2022_02_NIR <- Bokspits_2022_02$nir



#--- Calculate SAVI,MSAVI, MSAVI2, MTVI-----

#Planet MSAVI

Bokspits_2022_02_MSAVI = Bokspits_2022_02_NIR + 0.5 - (0.5 * sqrt((2 * Bokspits_2022_02_NIR + 1)^2 - 8 * (Bokspits_2022_02_NIR - (2 * Bokspits_2022_02_RED))))

plot (Bokspits_2022_02_MSAVI)

writeRaster(Bokspits_2022_02_MSAVI,"E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope/Bokspits_2022_02_MSAVI.tif",overwrite=TRUE)


#Bokspits_2022_02_tinel NDVI
Bokspits_2022_02_NDVI = (Bokspits_2022_02_NIR - Bokspits_2022_02_RED)/(Bokspits_2022_02_NIR +Bokspits_2022_02_RED)

plot (Bokspits_2022_02_NDVI)
writeRaster(Bokspits_2022_02_NDVI,"E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope/Bokspits_2022_02_NDVI.tif", overwrite=TRUE)


#Bokspits_2022_02_tinel SAVI

L=0.5
Bokspits_2022_02_SAVI= (1 + L)*(Bokspits_2022_02_NIR - Bokspits_2022_02_RED)/(Bokspits_2022_02_NIR + Bokspits_2022_02_RED + L)
plot (Bokspits_2022_02_SAVI)
writeRaster(Bokspits_2022_02_SAVI,"E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope/Bokspits_2022_02_SAVI.tif", overwrite=TRUE)

#MSAVI2

Bokspits_2022_02_MSAVI2 = (2 * Bokspits_2022_02_NIR + 1 - sqrt( (2 * Bokspits_2022_02_NIR + 1)^2 - 8 * (Bokspits_2022_02_NIR - Bokspits_2022_02_RED) )) / 2 

plot(Bokspits_2022_02_MSAVI2)

writeRaster(Bokspits_2022_02_MSAVI2,"E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope/Bokspits_2022_02_MSAVI2.tif", overwrite=TRUE)

#MTVI
#Modified Triangular Vegetation Index 2 (MTVI)

Bokspits_2022_02_MTVI = 1.5 * (1.2 * (Bokspits_2022_02_NIR - Bokspits_2022_02_GREEN) - 2.5 * (Bokspits_2022_02_RED - Bokspits_2022_02_GREEN)) /  sqrt( (2 * Bokspits_2022_02_NIR + 1)^2 - (6 * Bokspits_2022_02_NIR - 5 * sqrt(Bokspits_2022_02_RED) - 0.5) )
plot(Bokspits_2022_02_MTVI)

writeRaster(Bokspits_2022_02_MTVI,"E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope/Bokspits_2022_02_MTVI.tif", overwrite=TRUE)

#----6. Calculate Vegetation indices for Bokspits_2022_03-----

#Import Planet bands


plot (Bokspits_2022_03)
Bokspits_2022_03_BLUE <- Bokspits_2022_03$blue
Bokspits_2022_03_GREEN <- Bokspits_2022_03$green
Bokspits_2022_03_RED <- Bokspits_2022_03$red
Bokspits_2022_03_NIR <- Bokspits_2022_03$nir



#--- Calculate SAVI,MSAVI, MSAVI2, MTVI-----

#Planet MSAVI

Bokspits_2022_03_MSAVI = Bokspits_2022_03_NIR + 0.5 - (0.5 * sqrt((2 * Bokspits_2022_03_NIR + 1)^2 - 8 * (Bokspits_2022_03_NIR - (2 * Bokspits_2022_03_RED))))

plot (Bokspits_2022_03_MSAVI)

writeRaster(Bokspits_2022_03_MSAVI,"E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope/Bokspits_2022_03_MSAVI.tif",overwrite=TRUE)


#Bokspits_2022_03_tinel NDVI
Bokspits_2022_03_NDVI = (Bokspits_2022_03_NIR - Bokspits_2022_03_RED)/(Bokspits_2022_03_NIR +Bokspits_2022_03_RED)

plot (Bokspits_2022_03_NDVI)
writeRaster(Bokspits_2022_03_NDVI,"E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope/Bokspits_2022_03_NDVI.tif", overwrite=TRUE)


#Bokspits_2022_03_tinel SAVI

L=0.5
Bokspits_2022_03_SAVI= (1 + L)*(Bokspits_2022_03_NIR - Bokspits_2022_03_RED)/(Bokspits_2022_03_NIR + Bokspits_2022_03_RED + L)
plot (Bokspits_2022_03_SAVI)
writeRaster(Bokspits_2022_03_SAVI,"E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope/Bokspits_2022_03_SAVI.tif", overwrite=TRUE)

#MSAVI2

Bokspits_2022_03_MSAVI2 = (2 * Bokspits_2022_03_NIR + 1 - sqrt( (2 * Bokspits_2022_03_NIR + 1)^2 - 8 * (Bokspits_2022_03_NIR - Bokspits_2022_03_RED) )) / 2 

plot(Bokspits_2022_03_MSAVI2)

writeRaster(Bokspits_2022_03_MSAVI2,"E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope/Bokspits_2022_03_MSAVI2.tif", overwrite=TRUE)

#MTVI
#Modified Triangular Vegetation Index 2 (MTVI)

Bokspits_2022_03_MTVI = 1.5 * (1.2 * (Bokspits_2022_03_NIR - Bokspits_2022_03_GREEN) - 2.5 * (Bokspits_2022_03_RED - Bokspits_2022_03_GREEN)) /  sqrt( (2 * Bokspits_2022_03_NIR + 1)^2 - (6 * Bokspits_2022_03_NIR - 5 * sqrt(Bokspits_2022_03_RED) - 0.5) )
plot(Bokspits_2022_03_MTVI)

writeRaster(Bokspits_2022_03_MTVI,"E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope/Bokspits_2022_03_MTVI.tif", overwrite=TRUE)

#----7. Calculate Vegetation indices for Bokspits_2022_04-----

#Import Planet bands


plot (Bokspits_2022_04)
Bokspits_2022_04_BLUE <- Bokspits_2022_04$blue
Bokspits_2022_04_GREEN <- Bokspits_2022_04$green
Bokspits_2022_04_RED <- Bokspits_2022_04$red
Bokspits_2022_04_NIR <- Bokspits_2022_04$nir



#--- Calculate SAVI,MSAVI, MSAVI2, MTVI-----

#Planet MSAVI

Bokspits_2022_04_MSAVI = Bokspits_2022_04_NIR + 0.5 - (0.5 * sqrt((2 * Bokspits_2022_04_NIR + 1)^2 - 8 * (Bokspits_2022_04_NIR - (2 * Bokspits_2022_04_RED))))

plot (Bokspits_2022_04_MSAVI)

writeRaster(Bokspits_2022_04_MSAVI,"E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope/Bokspits_2022_04_MSAVI.tif",overwrite=TRUE)


#Bokspits_2022_04_tinel NDVI
Bokspits_2022_04_NDVI = (Bokspits_2022_04_NIR - Bokspits_2022_04_RED)/(Bokspits_2022_04_NIR +Bokspits_2022_04_RED)

plot (Bokspits_2022_04_NDVI)
writeRaster(Bokspits_2022_04_NDVI,"E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope/Bokspits_2022_04_NDVI.tif", overwrite=TRUE)


#Bokspits_2022_04_tinel SAVI

L=0.5
Bokspits_2022_04_SAVI= (1 + L)*(Bokspits_2022_04_NIR - Bokspits_2022_04_RED)/(Bokspits_2022_04_NIR + Bokspits_2022_04_RED + L)
plot (Bokspits_2022_04_SAVI)
writeRaster(Bokspits_2022_04_SAVI,"E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope/Bokspits_2022_04_SAVI.tif", overwrite=TRUE)

#MSAVI2

Bokspits_2022_04_MSAVI2 = (2 * Bokspits_2022_04_NIR + 1 - sqrt( (2 * Bokspits_2022_04_NIR + 1)^2 - 8 * (Bokspits_2022_04_NIR - Bokspits_2022_04_RED) )) / 2 

plot(Bokspits_2022_04_MSAVI2)

writeRaster(Bokspits_2022_04_MSAVI2,"E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope/Bokspits_2022_04_MSAVI2.tif", overwrite=TRUE)

#MTVI
#Modified Triangular Vegetation Index 2 (MTVI)

Bokspits_2022_04_MTVI = 1.5 * (1.2 * (Bokspits_2022_04_NIR - Bokspits_2022_04_GREEN) - 2.5 * (Bokspits_2022_04_RED - Bokspits_2022_04_GREEN)) /  sqrt( (2 * Bokspits_2022_04_NIR + 1)^2 - (6 * Bokspits_2022_04_NIR - 5 * sqrt(Bokspits_2022_04_RED) - 0.5) )
plot(Bokspits_2022_04_MTVI)

writeRaster(Bokspits_2022_04_MTVI,"E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope/Bokspits_2022_04_MTVI.tif", overwrite=TRUE)



#----8. Calculate Vegetation indices for Bokspits_2022_05-----

#Import Planet bands


plot (Bokspits_2022_05)
Bokspits_2022_05_BLUE <- Bokspits_2022_05$blue
Bokspits_2022_05_GREEN <- Bokspits_2022_05$green
Bokspits_2022_05_RED <- Bokspits_2022_05$red
Bokspits_2022_05_NIR <- Bokspits_2022_05$nir



#--- Calculate SAVI,MSAVI, MSAVI2, MTVI-----

#Planet MSAVI

Bokspits_2022_05_MSAVI = Bokspits_2022_05_NIR + 0.5 - (0.5 * sqrt((2 * Bokspits_2022_05_NIR + 1)^2 - 8 * (Bokspits_2022_05_NIR - (2 * Bokspits_2022_05_RED))))

plot (Bokspits_2022_05_MSAVI)

writeRaster(Bokspits_2022_05_MSAVI,"E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope/Bokspits_2022_05_MSAVI.tif",overwrite=TRUE)


#Bokspits_2022_05_tinel NDVI
Bokspits_2022_05_NDVI = (Bokspits_2022_05_NIR - Bokspits_2022_05_RED)/(Bokspits_2022_05_NIR +Bokspits_2022_05_RED)

plot (Bokspits_2022_05_NDVI)
writeRaster(Bokspits_2022_05_NDVI,"E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope/Bokspits_2022_05_NDVI.tif", overwrite=TRUE)


#Bokspits_2022_05_tinel SAVI

L=0.5
Bokspits_2022_05_SAVI= (1 + L)*(Bokspits_2022_05_NIR - Bokspits_2022_05_RED)/(Bokspits_2022_05_NIR + Bokspits_2022_05_RED + L)
plot (Bokspits_2022_05_SAVI)
writeRaster(Bokspits_2022_05_SAVI,"E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope/Bokspits_2022_05_SAVI.tif", overwrite=TRUE)

#MSAVI2

Bokspits_2022_05_MSAVI2 = (2 * Bokspits_2022_05_NIR + 1 - sqrt( (2 * Bokspits_2022_05_NIR + 1)^2 - 8 * (Bokspits_2022_05_NIR - Bokspits_2022_05_RED) )) / 2 

plot(Bokspits_2022_05_MSAVI2)

writeRaster(Bokspits_2022_05_MSAVI2,"E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope/Bokspits_2022_05_MSAVI2.tif", overwrite=TRUE)

#MTVI
#Modified Triangular Vegetation Index 2 (MTVI)

Bokspits_2022_05_MTVI = 1.5 * (1.2 * (Bokspits_2022_05_NIR - Bokspits_2022_05_GREEN) - 2.5 * (Bokspits_2022_05_RED - Bokspits_2022_05_GREEN)) /  sqrt( (2 * Bokspits_2022_05_NIR + 1)^2 - (6 * Bokspits_2022_05_NIR - 5 * sqrt(Bokspits_2022_05_RED) - 0.5) )
plot(Bokspits_2022_05_MTVI)

writeRaster(Bokspits_2022_05_MTVI,"E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope/Bokspits_2022_05_MTVI.tif", overwrite=TRUE)



#----9. Calculate Vegetation indices for Bokspits_2022_06-----

#Import Planet bands


plot (Bokspits_2022_06)
Bokspits_2022_06_BLUE <- Bokspits_2022_06$blue
Bokspits_2022_06_GREEN <- Bokspits_2022_06$green
Bokspits_2022_06_RED <- Bokspits_2022_06$red
Bokspits_2022_06_NIR <- Bokspits_2022_06$nir



#--- Calculate SAVI,MSAVI, MSAVI2, MTVI-----

#Planet MSAVI

Bokspits_2022_06_MSAVI = Bokspits_2022_06_NIR + 0.5 - (0.5 * sqrt((2 * Bokspits_2022_06_NIR + 1)^2 - 8 * (Bokspits_2022_06_NIR - (2 * Bokspits_2022_06_RED))))

plot (Bokspits_2022_06_MSAVI)

writeRaster(Bokspits_2022_06_MSAVI,"E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope/Bokspits_2022_06_MSAVI.tif",overwrite=TRUE)


#Bokspits_2022_06_tinel NDVI
Bokspits_2022_06_NDVI = (Bokspits_2022_06_NIR - Bokspits_2022_06_RED)/(Bokspits_2022_06_NIR +Bokspits_2022_06_RED)

plot (Bokspits_2022_06_NDVI)
writeRaster(Bokspits_2022_06_NDVI,"E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope/Bokspits_2022_06_NDVI.tif", overwrite=TRUE)


#Bokspits_2022_06_tinel SAVI

L=0.5
Bokspits_2022_06_SAVI= (1 + L)*(Bokspits_2022_06_NIR - Bokspits_2022_06_RED)/(Bokspits_2022_06_NIR + Bokspits_2022_06_RED + L)
plot (Bokspits_2022_06_SAVI)
writeRaster(Bokspits_2022_06_SAVI,"E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope/Bokspits_2022_06_SAVI.tif", overwrite=TRUE)

#MSAVI2

Bokspits_2022_06_MSAVI2 = (2 * Bokspits_2022_06_NIR + 1 - sqrt( (2 * Bokspits_2022_06_NIR + 1)^2 - 8 * (Bokspits_2022_06_NIR - Bokspits_2022_06_RED) )) / 2 

plot(Bokspits_2022_06_MSAVI2)

writeRaster(Bokspits_2022_06_MSAVI2,"E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope/Bokspits_2022_06_MSAVI2.tif", overwrite=TRUE)

#MTVI
#Modified Triangular Vegetation Index 2 (MTVI)

Bokspits_2022_06_MTVI = 1.5 * (1.2 * (Bokspits_2022_06_NIR - Bokspits_2022_06_GREEN) - 2.5 * (Bokspits_2022_06_RED - Bokspits_2022_06_GREEN)) /  sqrt( (2 * Bokspits_2022_06_NIR + 1)^2 - (6 * Bokspits_2022_06_NIR - 5 * sqrt(Bokspits_2022_06_RED) - 0.5) )
plot(Bokspits_2022_06_MTVI)

writeRaster(Bokspits_2022_06_MTVI,"E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope/Bokspits_2022_06_MTVI.tif", overwrite=TRUE)



#----10. Calculate Vegetation indices for Bokspits_2022_07-----

#Import Planet bands


plot (Bokspits_2022_07)
Bokspits_2022_07_BLUE <- Bokspits_2022_07$blue
Bokspits_2022_07_GREEN <- Bokspits_2022_07$green
Bokspits_2022_07_RED <- Bokspits_2022_07$red
Bokspits_2022_07_NIR <- Bokspits_2022_07$nir



#--- Calculate SAVI,MSAVI, MSAVI2, MTVI-----

#Planet MSAVI

Bokspits_2022_07_MSAVI = Bokspits_2022_07_NIR + 0.5 - (0.5 * sqrt((2 * Bokspits_2022_07_NIR + 1)^2 - 8 * (Bokspits_2022_07_NIR - (2 * Bokspits_2022_07_RED))))

plot (Bokspits_2022_07_MSAVI)

writeRaster(Bokspits_2022_07_MSAVI,"E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope/Bokspits_2022_07_MSAVI.tif",overwrite=TRUE)


#Bokspits_2022_07_tinel NDVI
Bokspits_2022_07_NDVI = (Bokspits_2022_07_NIR - Bokspits_2022_07_RED)/(Bokspits_2022_07_NIR +Bokspits_2022_07_RED)

plot (Bokspits_2022_07_NDVI)
writeRaster(Bokspits_2022_07_NDVI,"E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope/Bokspits_2022_07_NDVI.tif", overwrite=TRUE)


#Bokspits_2022_07_tinel SAVI

L=0.5
Bokspits_2022_07_SAVI= (1 + L)*(Bokspits_2022_07_NIR - Bokspits_2022_07_RED)/(Bokspits_2022_07_NIR + Bokspits_2022_07_RED + L)
plot (Bokspits_2022_07_SAVI)
writeRaster(Bokspits_2022_07_SAVI,"E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope/Bokspits_2022_07_SAVI.tif", overwrite=TRUE)

#MSAVI2

Bokspits_2022_07_MSAVI2 = (2 * Bokspits_2022_07_NIR + 1 - sqrt( (2 * Bokspits_2022_07_NIR + 1)^2 - 8 * (Bokspits_2022_07_NIR - Bokspits_2022_07_RED) )) / 2 

plot(Bokspits_2022_07_MSAVI2)

writeRaster(Bokspits_2022_07_MSAVI2,"E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope/Bokspits_2022_07_MSAVI2.tif", overwrite=TRUE)

#MTVI
#Modified Triangular Vegetation Index 2 (MTVI)

Bokspits_2022_07_MTVI = 1.5 * (1.2 * (Bokspits_2022_07_NIR - Bokspits_2022_07_GREEN) - 2.5 * (Bokspits_2022_07_RED - Bokspits_2022_07_GREEN)) /  sqrt( (2 * Bokspits_2022_07_NIR + 1)^2 - (6 * Bokspits_2022_07_NIR - 5 * sqrt(Bokspits_2022_07_RED) - 0.5) )
plot(Bokspits_2022_07_MTVI)

writeRaster(Bokspits_2022_07_MTVI,"E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope/Bokspits_2022_07_MTVI.tif", overwrite=TRUE)



#----11. Calculate Vegetation indices for Bokspits_2022_08-----

#Import Planet bands


plot (Bokspits_2022_08)
Bokspits_2022_08_BLUE <- Bokspits_2022_08$blue
Bokspits_2022_08_GREEN <- Bokspits_2022_08$green
Bokspits_2022_08_RED <- Bokspits_2022_08$red
Bokspits_2022_08_NIR <- Bokspits_2022_08$nir



#--- Calculate SAVI,MSAVI, MSAVI2, MTVI-----

#Planet MSAVI

Bokspits_2022_08_MSAVI = Bokspits_2022_08_NIR + 0.5 - (0.5 * sqrt((2 * Bokspits_2022_08_NIR + 1)^2 - 8 * (Bokspits_2022_08_NIR - (2 * Bokspits_2022_08_RED))))

plot (Bokspits_2022_08_MSAVI)

writeRaster(Bokspits_2022_08_MSAVI,"E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope/Bokspits_2022_08_MSAVI.tif",overwrite=TRUE)


#Bokspits_2022_08_tinel NDVI
Bokspits_2022_08_NDVI = (Bokspits_2022_08_NIR - Bokspits_2022_08_RED)/(Bokspits_2022_08_NIR +Bokspits_2022_08_RED)

plot (Bokspits_2022_08_NDVI)
writeRaster(Bokspits_2022_08_NDVI,"E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope/Bokspits_2022_08_NDVI.tif", overwrite=TRUE)


#Bokspits_2022_08_tinel SAVI

L=0.5
Bokspits_2022_08_SAVI= (1 + L)*(Bokspits_2022_08_NIR - Bokspits_2022_08_RED)/(Bokspits_2022_08_NIR + Bokspits_2022_08_RED + L)
plot (Bokspits_2022_08_SAVI)
writeRaster(Bokspits_2022_08_SAVI,"E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope/Bokspits_2022_08_SAVI.tif", overwrite=TRUE)

#MSAVI2

Bokspits_2022_08_MSAVI2 = (2 * Bokspits_2022_08_NIR + 1 - sqrt( (2 * Bokspits_2022_08_NIR + 1)^2 - 8 * (Bokspits_2022_08_NIR - Bokspits_2022_08_RED) )) / 2 

plot(Bokspits_2022_08_MSAVI2)

writeRaster(Bokspits_2022_08_MSAVI2,"E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope/Bokspits_2022_08_MSAVI2.tif", overwrite=TRUE)

#MTVI
#Modified Triangular Vegetation Index 2 (MTVI)

Bokspits_2022_08_MTVI = 1.5 * (1.2 * (Bokspits_2022_08_NIR - Bokspits_2022_08_GREEN) - 2.5 * (Bokspits_2022_08_RED - Bokspits_2022_08_GREEN)) /  sqrt( (2 * Bokspits_2022_08_NIR + 1)^2 - (6 * Bokspits_2022_08_NIR - 5 * sqrt(Bokspits_2022_08_RED) - 0.5) )
plot(Bokspits_2022_08_MTVI)

writeRaster(Bokspits_2022_08_MTVI,"E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope/Bokspits_2022_08_MTVI.tif", overwrite=TRUE)



#----12. Calculate Vegetation indices for Bokspits_2022_09-----

#Import Planet bands


plot (Bokspits_2022_09)
Bokspits_2022_09_BLUE <- Bokspits_2022_09$blue
Bokspits_2022_09_GREEN <- Bokspits_2022_09$green
Bokspits_2022_09_RED <- Bokspits_2022_09$red
Bokspits_2022_09_NIR <- Bokspits_2022_09$nir



#--- Calculate SAVI,MSAVI, MSAVI2, MTVI-----

#Planet MSAVI

Bokspits_2022_09_MSAVI = Bokspits_2022_09_NIR + 0.5 - (0.5 * sqrt((2 * Bokspits_2022_09_NIR + 1)^2 - 8 * (Bokspits_2022_09_NIR - (2 * Bokspits_2022_09_RED))))

plot (Bokspits_2022_09_MSAVI)

writeRaster(Bokspits_2022_09_MSAVI,"E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope/Bokspits_2022_09_MSAVI.tif",overwrite=TRUE)


#Bokspits_2022_09_tinel NDVI
Bokspits_2022_09_NDVI = (Bokspits_2022_09_NIR - Bokspits_2022_09_RED)/(Bokspits_2022_09_NIR +Bokspits_2022_09_RED)

plot (Bokspits_2022_09_NDVI)
writeRaster(Bokspits_2022_09_NDVI,"E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope/Bokspits_2022_09_NDVI.tif", overwrite=TRUE)


#Bokspits_2022_09_tinel SAVI

L=0.5
Bokspits_2022_09_SAVI= (1 + L)*(Bokspits_2022_09_NIR - Bokspits_2022_09_RED)/(Bokspits_2022_09_NIR + Bokspits_2022_09_RED + L)
plot (Bokspits_2022_09_SAVI)
writeRaster(Bokspits_2022_09_SAVI,"E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope/Bokspits_2022_09_SAVI.tif", overwrite=TRUE)

#MSAVI2

Bokspits_2022_09_MSAVI2 = (2 * Bokspits_2022_09_NIR + 1 - sqrt( (2 * Bokspits_2022_09_NIR + 1)^2 - 8 * (Bokspits_2022_09_NIR - Bokspits_2022_09_RED) )) / 2 

plot(Bokspits_2022_09_MSAVI2)

writeRaster(Bokspits_2022_09_MSAVI2,"E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope/Bokspits_2022_09_MSAVI2.tif", overwrite=TRUE)

#MTVI
#Modified Triangular Vegetation Index 2 (MTVI)

Bokspits_2022_09_MTVI = 1.5 * (1.2 * (Bokspits_2022_09_NIR - Bokspits_2022_09_GREEN) - 2.5 * (Bokspits_2022_09_RED - Bokspits_2022_09_GREEN)) /  sqrt( (2 * Bokspits_2022_09_NIR + 1)^2 - (6 * Bokspits_2022_09_NIR - 5 * sqrt(Bokspits_2022_09_RED) - 0.5) )
plot(Bokspits_2022_09_MTVI)

writeRaster(Bokspits_2022_09_MTVI,"E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope/Bokspits_2022_09_MTVI.tif", overwrite=TRUE)




#----13. Calculate Vegetation indices for Bokspits_2022_10-----

#Import Planet bands


plot (Bokspits_2022_10)
Bokspits_2022_10_BLUE <- Bokspits_2022_10$blue
Bokspits_2022_10_GREEN <- Bokspits_2022_10$green
Bokspits_2022_10_RED <- Bokspits_2022_10$red
Bokspits_2022_10_NIR <- Bokspits_2022_10$nir



#--- Calculate SAVI,MSAVI, MSAVI2, MTVI-----

#Planet MSAVI

Bokspits_2022_10_MSAVI = Bokspits_2022_10_NIR + 0.5 - (0.5 * sqrt((2 * Bokspits_2022_10_NIR + 1)^2 - 8 * (Bokspits_2022_10_NIR - (2 * Bokspits_2022_10_RED))))

plot (Bokspits_2022_10_MSAVI)

writeRaster(Bokspits_2022_10_MSAVI,"E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope/Bokspits_2022_10_MSAVI.tif",overwrite=TRUE)


#Bokspits_2022_10_tinel NDVI
Bokspits_2022_10_NDVI = (Bokspits_2022_10_NIR - Bokspits_2022_10_RED)/(Bokspits_2022_10_NIR +Bokspits_2022_10_RED)

plot (Bokspits_2022_10_NDVI)
writeRaster(Bokspits_2022_10_NDVI,"E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope/Bokspits_2022_10_NDVI.tif", overwrite=TRUE)


#Bokspits_2022_10_tinel SAVI

L=0.5
Bokspits_2022_10_SAVI= (1 + L)*(Bokspits_2022_10_NIR - Bokspits_2022_10_RED)/(Bokspits_2022_10_NIR + Bokspits_2022_10_RED + L)
plot (Bokspits_2022_10_SAVI)
writeRaster(Bokspits_2022_10_SAVI,"E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope/Bokspits_2022_10_SAVI.tif", overwrite=TRUE)

#MSAVI2

Bokspits_2022_10_MSAVI2 = (2 * Bokspits_2022_10_NIR + 1 - sqrt( (2 * Bokspits_2022_10_NIR + 1)^2 - 8 * (Bokspits_2022_10_NIR - Bokspits_2022_10_RED) )) / 2 

plot(Bokspits_2022_10_MSAVI2)

writeRaster(Bokspits_2022_10_MSAVI2,"E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope/Bokspits_2022_10_MSAVI2.tif", overwrite=TRUE)

#MTVI
#Modified Triangular Vegetation Index 2 (MTVI)

Bokspits_2022_10_MTVI = 1.5 * (1.2 * (Bokspits_2022_10_NIR - Bokspits_2022_10_GREEN) - 2.5 * (Bokspits_2022_10_RED - Bokspits_2022_10_GREEN)) /  sqrt( (2 * Bokspits_2022_10_NIR + 1)^2 - (6 * Bokspits_2022_10_NIR - 5 * sqrt(Bokspits_2022_10_RED) - 0.5) )
plot(Bokspits_2022_10_MTVI)

writeRaster(Bokspits_2022_10_MTVI,"E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope/Bokspits_2022_10_MTVI.tif", overwrite=TRUE)



#----14. Calculate Vegetation indices for Bokspits_2022_11-----

#Import Planet bands


plot (Bokspits_2022_11)
Bokspits_2022_11_BLUE <- Bokspits_2022_11$blue
Bokspits_2022_11_GREEN <- Bokspits_2022_11$green
Bokspits_2022_11_RED <- Bokspits_2022_11$red
Bokspits_2022_11_NIR <- Bokspits_2022_11$nir



#--- Calculate SAVI,MSAVI, MSAVI2, MTVI-----

#Planet MSAVI

Bokspits_2022_11_MSAVI = Bokspits_2022_11_NIR + 0.5 - (0.5 * sqrt((2 * Bokspits_2022_11_NIR + 1)^2 - 8 * (Bokspits_2022_11_NIR - (2 * Bokspits_2022_11_RED))))

plot (Bokspits_2022_11_MSAVI)

writeRaster(Bokspits_2022_11_MSAVI,"E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope/Bokspits_2022_11_MSAVI.tif",overwrite=TRUE)


#Bokspits_2022_11_tinel NDVI
Bokspits_2022_11_NDVI = (Bokspits_2022_11_NIR - Bokspits_2022_11_RED)/(Bokspits_2022_11_NIR +Bokspits_2022_11_RED)

plot (Bokspits_2022_11_NDVI)
writeRaster(Bokspits_2022_11_NDVI,"E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope/Bokspits_2022_11_NDVI.tif", overwrite=TRUE)


#Bokspits_2022_11_tinel SAVI

L=0.5
Bokspits_2022_11_SAVI= (1 + L)*(Bokspits_2022_11_NIR - Bokspits_2022_11_RED)/(Bokspits_2022_11_NIR + Bokspits_2022_11_RED + L)
plot (Bokspits_2022_11_SAVI)
writeRaster(Bokspits_2022_11_SAVI,"E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope/Bokspits_2022_11_SAVI.tif", overwrite=TRUE)

#MSAVI2

Bokspits_2022_11_MSAVI2 = (2 * Bokspits_2022_11_NIR + 1 - sqrt( (2 * Bokspits_2022_11_NIR + 1)^2 - 8 * (Bokspits_2022_11_NIR - Bokspits_2022_11_RED) )) / 2 

plot(Bokspits_2022_11_MSAVI2)

writeRaster(Bokspits_2022_11_MSAVI2,"E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope/Bokspits_2022_11_MSAVI2.tif", overwrite=TRUE)

#MTVI
#Modified Triangular Vegetation Index 2 (MTVI)

Bokspits_2022_11_MTVI = 1.5 * (1.2 * (Bokspits_2022_11_NIR - Bokspits_2022_11_GREEN) - 2.5 * (Bokspits_2022_11_RED - Bokspits_2022_11_GREEN)) /  sqrt( (2 * Bokspits_2022_11_NIR + 1)^2 - (6 * Bokspits_2022_11_NIR - 5 * sqrt(Bokspits_2022_11_RED) - 0.5) )
plot(Bokspits_2022_11_MTVI)

writeRaster(Bokspits_2022_11_MTVI,"E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope/Bokspits_2022_11_MTVI.tif", overwrite=TRUE)



#----15. Calculate Vegetation indices for Bokspits_2022_12-----

#Import Planet bands


plot (Bokspits_2022_12)
Bokspits_2022_12_BLUE <- Bokspits_2022_12$blue
Bokspits_2022_12_GREEN <- Bokspits_2022_12$green
Bokspits_2022_12_RED <- Bokspits_2022_12$red
Bokspits_2022_12_NIR <- Bokspits_2022_12$nir



#--- Calculate SAVI,MSAVI, MSAVI2, MTVI-----

#Planet MSAVI

Bokspits_2022_12_MSAVI = Bokspits_2022_12_NIR + 0.5 - (0.5 * sqrt((2 * Bokspits_2022_12_NIR + 1)^2 - 8 * (Bokspits_2022_12_NIR - (2 * Bokspits_2022_12_RED))))

plot (Bokspits_2022_12_MSAVI)

writeRaster(Bokspits_2022_12_MSAVI,"E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope/Bokspits_2022_12_MSAVI.tif",overwrite=TRUE)


#Bokspits_2022_12_tinel NDVI
Bokspits_2022_12_NDVI = (Bokspits_2022_12_NIR - Bokspits_2022_12_RED)/(Bokspits_2022_12_NIR +Bokspits_2022_12_RED)

plot (Bokspits_2022_12_NDVI)
writeRaster(Bokspits_2022_12_NDVI,"E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope/Bokspits_2022_12_NDVI.tif", overwrite=TRUE)


#Bokspits_2022_12_tinel SAVI

L=0.5
Bokspits_2022_12_SAVI= (1 + L)*(Bokspits_2022_12_NIR - Bokspits_2022_12_RED)/(Bokspits_2022_12_NIR + Bokspits_2022_12_RED + L)
plot (Bokspits_2022_12_SAVI)
writeRaster(Bokspits_2022_12_SAVI,"E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope/Bokspits_2022_12_SAVI.tif", overwrite=TRUE)

#MSAVI2

Bokspits_2022_12_MSAVI2 = (2 * Bokspits_2022_12_NIR + 1 - sqrt( (2 * Bokspits_2022_12_NIR + 1)^2 - 8 * (Bokspits_2022_12_NIR - Bokspits_2022_12_RED) )) / 2 

plot(Bokspits_2022_12_MSAVI2)

writeRaster(Bokspits_2022_12_MSAVI2,"E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope/Bokspits_2022_12_MSAVI2.tif", overwrite=TRUE)

#MTVI
#Modified Triangular Vegetation Index 2 (MTVI)

Bokspits_2022_12_MTVI = 1.5 * (1.2 * (Bokspits_2022_12_NIR - Bokspits_2022_12_GREEN) - 2.5 * (Bokspits_2022_12_RED - Bokspits_2022_12_GREEN)) /  sqrt( (2 * Bokspits_2022_12_NIR + 1)^2 - (6 * Bokspits_2022_12_NIR - 5 * sqrt(Bokspits_2022_12_RED) - 0.5) )
plot(Bokspits_2022_12_MTVI)

writeRaster(Bokspits_2022_12_MTVI,"E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope/Bokspits_2022_12_MTVI.tif", overwrite=TRUE)




#----16. Extract Data for Training Polygons
#----NDVI----
Year <- exact_extract(Bokspits_2022_01_NDVI,VegpolyB1,"mean")
names(Year) <- c('Bokspits_2022_01_NDVI')
Year_DF <- dplyr::mutate(Year_DF, Bokspits_2022_01_NDVI = Year)

Year <- exact_extract(Bokspits_2022_02_NDVI,VegpolyB1,"mean")
names(Year) <- c('Bokspits_2022_02_NDVI')
Year_DF <- dplyr::mutate(Year_DF, Bokspits_2022_02_NDVI = Year)

Year <- exact_extract(Bokspits_2022_03_NDVI,VegpolyB1,"mean")
names(Year) <- c('Bokspits_2022_03_NDVI')
Year_DF <- dplyr::mutate(Year_DF, Bokspits_2022_03_NDVI = Year)

Year <- exact_extract(Bokspits_2022_04_NDVI,VegpolyB1,"mean")
names(Year) <- c('Bokspits_2022_04_NDVI')
Year_DF <- dplyr::mutate(Year_DF, Bokspits_2022_04_NDVI = Year)

Year <- exact_extract(Bokspits_2022_05_NDVI,VegpolyB1,"mean")
names(Year) <- c('Bokspits_2022_05_NDVI')
Year_DF <- dplyr::mutate(Year_DF, Bokspits_2022_05_NDVI = Year)

Year <- exact_extract(Bokspits_2022_06_NDVI,VegpolyB1,"mean")
names(Year) <- c('Bokspits_2022_06_NDVI')
Year_DF <- dplyr::mutate(Year_DF, Bokspits_2022_06_NDVI = Year)

Year <- exact_extract(Bokspits_2022_07_NDVI,VegpolyB1,"mean")
names(Year) <- c('Bokspits_2022_07_NDVI')
Year_DF <- dplyr::mutate(Year_DF, Bokspits_2022_07_NDVI = Year)

Year <- exact_extract(Bokspits_2022_08_NDVI,VegpolyB1,"mean")
names(Year) <- c('Bokspits_2022_08_NDVI')
Year_DF <- dplyr::mutate(Year_DF, Bokspits_2022_08_NDVI = Year)

Year <- exact_extract(Bokspits_2022_09_NDVI,VegpolyB1,"mean")
names(Year) <- c('Bokspits_2022_09_NDVI')
Year_DF <- dplyr::mutate(Year_DF, Bokspits_2022_09_NDVI = Year)

Year <- exact_extract(Bokspits_2022_10_NDVI,VegpolyB1,"mean")
names(Year) <- c('Bokspits_2022_10_NDVI')
Year_DF <- dplyr::mutate(Year_DF, Bokspits_2022_10_NDVI = Year)

Year <- exact_extract(Bokspits_2022_11_NDVI,VegpolyB1,"mean")
names(Year) <- c('Bokspits_2022_11_NDVI')
Year_DF <- dplyr::mutate(Year_DF, Bokspits_2022_11_NDVI = Year)

Year <- exact_extract(Bokspits_2022_12_NDVI,VegpolyB1,"mean")
names(Year) <- c('Bokspits_2022_12_NDVI')
Year_DF <- dplyr::mutate(Year_DF, Bokspits_2022_12_NDVI = Year)

#----SAVI-----

Year <- exact_extract(Bokspits_2022_01_SAVI,VegpolyB1,"mean")
names(Year) <- c('Bokspits_2022_01_SAVI')
Year_DF <- dplyr::mutate(Year_DF, Bokspits_2022_01_SAVI = Year)

Year <- exact_extract(Bokspits_2022_02_SAVI,VegpolyB1,"mean")
names(Year) <- c('Bokspits_2022_02_SAVI')
Year_DF <- dplyr::mutate(Year_DF, Bokspits_2022_02_SAVI = Year)

Year <- exact_extract(Bokspits_2022_03_SAVI,VegpolyB1,"mean")
names(Year) <- c('Bokspits_2022_03_SAVI')
Year_DF <- dplyr::mutate(Year_DF, Bokspits_2022_03_SAVI = Year)

Year <- exact_extract(Bokspits_2022_04_SAVI,VegpolyB1,"mean")
names(Year) <- c('Bokspits_2022_04_SAVI')
Year_DF <- dplyr::mutate(Year_DF, Bokspits_2022_04_SAVI = Year)

Year <- exact_extract(Bokspits_2022_05_SAVI,VegpolyB1,"mean")
names(Year) <- c('Bokspits_2022_05_SAVI')
Year_DF <- dplyr::mutate(Year_DF, Bokspits_2022_05_SAVI = Year)

Year <- exact_extract(Bokspits_2022_06_SAVI,VegpolyB1,"mean")
names(Year) <- c('Bokspits_2022_06_SAVI')
Year_DF <- dplyr::mutate(Year_DF, Bokspits_2022_06_SAVI = Year)

Year <- exact_extract(Bokspits_2022_07_SAVI,VegpolyB1,"mean")
names(Year) <- c('Bokspits_2022_07_SAVI')
Year_DF <- dplyr::mutate(Year_DF, Bokspits_2022_07_SAVI = Year)

Year <- exact_extract(Bokspits_2022_08_SAVI,VegpolyB1,"mean")
names(Year) <- c('Bokspits_2022_08_SAVI')
Year_DF <- dplyr::mutate(Year_DF, Bokspits_2022_08_SAVI = Year)

Year <- exact_extract(Bokspits_2022_09_SAVI,VegpolyB1,"mean")
names(Year) <- c('Bokspits_2022_09_SAVI')
Year_DF <- dplyr::mutate(Year_DF, Bokspits_2022_09_SAVI = Year)

Year <- exact_extract(Bokspits_2022_10_SAVI,VegpolyB1,"mean")
names(Year) <- c('Bokspits_2022_10_SAVI')
Year_DF <- dplyr::mutate(Year_DF, Bokspits_2022_10_SAVI = Year)

Year <- exact_extract(Bokspits_2022_11_SAVI,VegpolyB1,"mean")
names(Year) <- c('Bokspits_2022_11_SAVI')
Year_DF <- dplyr::mutate(Year_DF, Bokspits_2022_11_SAVI = Year)

Year <- exact_extract(Bokspits_2022_12_SAVI,VegpolyB1,"mean")
names(Year) <- c('Bokspits_2022_12_SAVI')
Year_DF <- dplyr::mutate(Year_DF, Bokspits_2022_12_SAVI = Year)
#Vegpoly_DFMain <- bind_rows (Vegpoly_DFB1,Vegpoly_DFB2,Vegpoly_DFB3,Vegpoly_DFS1,Vegpoly_DFS2,Vegpoly_DFS3, Vegpoly_DFS4)

#----MSAVI2----

Year <- exact_extract(Bokspits_2022_01_MSAVI2,VegpolyB1,"mean")
names(Year) <- c('Bokspits_2022_01_MSAVI2')
Year_DF <- dplyr::mutate(Year_DF, Bokspits_2022_01_MSAVI2 = Year)

Year <- exact_extract(Bokspits_2022_02_MSAVI2,VegpolyB1,"mean")
names(Year) <- c('Bokspits_2022_02_MSAVI2')
Year_DF <- dplyr::mutate(Year_DF, Bokspits_2022_02_MSAVI2 = Year)

Year <- exact_extract(Bokspits_2022_03_MSAVI2,VegpolyB1,"mean")
names(Year) <- c('Bokspits_2022_03_MSAVI2')
Year_DF <- dplyr::mutate(Year_DF, Bokspits_2022_03_MSAVI2 = Year)

Year <- exact_extract(Bokspits_2022_04_MSAVI2,VegpolyB1,"mean")
names(Year) <- c('Bokspits_2022_04_MSAVI2')
Year_DF <- dplyr::mutate(Year_DF, Bokspits_2022_04_MSAVI2 = Year)

Year <- exact_extract(Bokspits_2022_05_MSAVI2,VegpolyB1,"mean")
names(Year) <- c('Bokspits_2022_05_MSAVI2')
Year_DF <- dplyr::mutate(Year_DF, Bokspits_2022_05_MSAVI2 = Year)

Year <- exact_extract(Bokspits_2022_06_MSAVI2,VegpolyB1,"mean")
names(Year) <- c('Bokspits_2022_06_MSAVI2')
Year_DF <- dplyr::mutate(Year_DF, Bokspits_2022_06_MSAVI2 = Year)

Year <- exact_extract(Bokspits_2022_07_MSAVI2,VegpolyB1,"mean")
names(Year) <- c('Bokspits_2022_07_MSAVI2')
Year_DF <- dplyr::mutate(Year_DF, Bokspits_2022_07_MSAVI2 = Year)

Year <- exact_extract(Bokspits_2022_08_MSAVI2,VegpolyB1,"mean")
names(Year) <- c('Bokspits_2022_08_MSAVI2')
Year_DF <- dplyr::mutate(Year_DF, Bokspits_2022_08_MSAVI2 = Year)

Year <- exact_extract(Bokspits_2022_09_MSAVI2,VegpolyB1,"mean")
names(Year) <- c('Bokspits_2022_09_MSAVI2')
Year_DF <- dplyr::mutate(Year_DF, Bokspits_2022_09_MSAVI2 = Year)

Year <- exact_extract(Bokspits_2022_10_MSAVI2,VegpolyB1,"mean")
names(Year) <- c('Bokspits_2022_10_MSAVI2')
Year_DF <- dplyr::mutate(Year_DF, Bokspits_2022_10_MSAVI2 = Year)

Year <- exact_extract(Bokspits_2022_11_MSAVI2,VegpolyB1,"mean")
names(Year) <- c('Bokspits_2022_11_MSAVI2')
Year_DF <- dplyr::mutate(Year_DF, Bokspits_2022_11_MSAVI2 = Year)

Year <- exact_extract(Bokspits_2022_12_MSAVI2,VegpolyB1,"mean")
names(Year) <- c('Bokspits_2022_12_MSAVI2')
Year_DF <- dplyr::mutate(Year_DF, Bokspits_2022_12_MSAVI2 = Year)

#----MTVI----

Year <- exact_extract(Bokspits_2022_01_MTVI,VegpolyB1,"mean")
names(Year) <- c('Bokspits_2022_01_MTVI')
Year_DF <- dplyr::mutate(Year_DF, Bokspits_2022_01_MTVI = Year)

Year <- exact_extract(Bokspits_2022_02_MTVI,VegpolyB1,"mean")
names(Year) <- c('Bokspits_2022_02_MTVI')
Year_DF <- dplyr::mutate(Year_DF, Bokspits_2022_02_MTVI = Year)

Year <- exact_extract(Bokspits_2022_03_MTVI,VegpolyB1,"mean")
names(Year) <- c('Bokspits_2022_03_MTVI')
Year_DF <- dplyr::mutate(Year_DF, Bokspits_2022_03_MTVI = Year)

Year <- exact_extract(Bokspits_2022_04_MTVI,VegpolyB1,"mean")
names(Year) <- c('Bokspits_2022_04_MTVI')
Year_DF <- dplyr::mutate(Year_DF, Bokspits_2022_04_MTVI = Year)

Year <- exact_extract(Bokspits_2022_05_MTVI,VegpolyB1,"mean")
names(Year) <- c('Bokspits_2022_05_MTVI')
Year_DF <- dplyr::mutate(Year_DF, Bokspits_2022_05_MTVI = Year)

Year <- exact_extract(Bokspits_2022_06_MTVI,VegpolyB1,"mean")
names(Year) <- c('Bokspits_2022_06_MTVI')
Year_DF <- dplyr::mutate(Year_DF, Bokspits_2022_06_MTVI = Year)

Year <- exact_extract(Bokspits_2022_07_MTVI,VegpolyB1,"mean")
names(Year) <- c('Bokspits_2022_07_MTVI')
Year_DF <- dplyr::mutate(Year_DF, Bokspits_2022_07_MTVI = Year)

Year <- exact_extract(Bokspits_2022_08_MTVI,VegpolyB1,"mean")
names(Year) <- c('Bokspits_2022_08_MTVI')
Year_DF <- dplyr::mutate(Year_DF, Bokspits_2022_08_MTVI = Year)

Year <- exact_extract(Bokspits_2022_09_MTVI,VegpolyB1,"mean")
names(Year) <- c('Bokspits_2022_09_MTVI')
Year_DF <- dplyr::mutate(Year_DF, Bokspits_2022_09_MTVI = Year)

Year <- exact_extract(Bokspits_2022_10_MTVI,VegpolyB1,"mean")
names(Year) <- c('Bokspits_2022_10_MTVI')
Year_DF <- dplyr::mutate(Year_DF, Bokspits_2022_10_MTVI = Year)

Year <- exact_extract(Bokspits_2022_11_MTVI,VegpolyB1,"mean")
names(Year) <- c('Bokspits_2022_11_MTVI')
Year_DF <- dplyr::mutate(Year_DF, Bokspits_2022_11_MTVI = Year)

Year <- exact_extract(Bokspits_2022_12_MTVI,VegpolyB1,"mean")
names(Year) <- c('Bokspits_2022_12_MTVI')
Year_DF <- dplyr::mutate(Year_DF, Bokspits_2022_12_MTVI = Year)

