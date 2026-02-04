#Script to Calculate Vegetation Indices for Botswana Drone Image Data

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

#----1.Bokspits 1 Drone Survey Image Data - Calculate Vegetation Indices ----

# read in data
Bokspits_1_clipper <- read_sf(dsn = 'E:/Glenn/Botswana/Final_Drone_Survey_Data/Bokspits_1', layer = "Bokspits_1_clip")
Bokspits_1_clip <- vect(Bokspits_1_clipper)

Bokspits_1_BLUE <-  rast("E:/Glenn/Botswana/Pix4d/Bokspits_1_MS/4_index/reflectance/Bokspits_1_MS_transparent_reflectance_blue.tif")
Bokspits_1_GREEN <-  rast("E:/Glenn/Botswana/Pix4d/Bokspits_1_MS/4_index/reflectance/Bokspits_1_MS_transparent_reflectance_green.tif")
Bokspits_1_RED<-  rast("E:/Glenn/Botswana/Pix4d/Bokspits_1_MS/4_index/reflectance/Bokspits_1_MS_transparent_reflectance_red.tif")
Bokspits_1_RedEdge <-  rast("E:/Glenn/Botswana/Pix4d/Bokspits_1_MS/4_index/reflectance/Bokspits_1_MS_transparent_reflectance_red edge.tif")
Bokspits_1_NIR <-  rast("E:/Glenn/Botswana/Pix4d/Bokspits_1_MS/4_index/reflectance/Bokspits_1_MS_transparent_reflectance_NIR.tif")

#Calculate Vegetation Indices

#--- Calculate SAVI,MSAVI, MSAVI2, MTVI-----

#Planet MSAVI

Bokspits_1_MSAVI = Bokspits_1_NIR + 0.5 - (0.5 * sqrt((2 * Bokspits_1_NIR + 1)^2 - 8 * (Bokspits_1_NIR - (2 * Bokspits_1_RED))))

plot (Bokspits_1_MSAVI)

writeRaster(Bokspits_1_MSAVI,"E:/Glenn/Botswana/Drone_Data_VI/Bokspits_1_MSAVI.tif",overwrite=TRUE)


#Bokspits_1 NDVI
Bokspits_1_NDVI = (Bokspits_1_NIR - Bokspits_1_RED)/(Bokspits_1_NIR +Bokspits_1_RED)

plot (Bokspits_1_NDVI)
writeRaster(Bokspits_1_NDVI,"E:/Glenn/Botswana/Drone_Data_VI/Bokspits_1_NDVI.tif", overwrite=TRUE)


#Bokspits_1 SAVI

L=0.5
Bokspits_1_SAVI= (1 + L)*(Bokspits_1_NIR - Bokspits_1_RED)/(Bokspits_1_NIR + Bokspits_1_RED + L)
plot (Bokspits_1_SAVI)
writeRaster(Bokspits_1_SAVI,"E:/Glenn/Botswana/Drone_Data_VI/Bokspits_1_SAVI.tif", overwrite=TRUE)

#MSAVI2

Bokspits_1_MSAVI2 = (2 * Bokspits_1_NIR + 1 - sqrt( (2 * Bokspits_1_NIR + 1)^2 - 8 * (Bokspits_1_NIR - Bokspits_1_RED) )) / 2 

plot(Bokspits_1_MSAVI2)

writeRaster(Bokspits_1_MSAVI2,"E:/Glenn/Botswana/Drone_Data_VI/Bokspits_1_MSAVI2.tif", overwrite=TRUE)

#MTVI
#Modified Triangular Vegetation Index 2 (MTVI)

Bokspits_1_MTVI = 1.5 * (1.2 * (Bokspits_1_NIR - Bokspits_1_GREEN) - 2.5 * (Bokspits_1_RED - Bokspits_1_GREEN)) /  sqrt( (2 * Bokspits_1_NIR + 1)^2 - (6 * Bokspits_1_NIR - 5 * sqrt(Bokspits_1_RED) - 0.5) )
plot(Bokspits_1_MTVI)

writeRaster(Bokspits_1_MTVI,"E:/Glenn/Botswana/Drone_Data_VI/Bokspits_1_MTVI.tif", overwrite=TRUE)


#----2. Bokspits 2 Drone survey Image Data - calculate Vegetation indices----

# Read in data
Bokspits_2_clipper <- read_sf(dsn = 'E:/Glenn/Botswana/Final_Drone_Survey_Data/Bokspits_2', layer = "Bokspits_2_clip")
Bokspits_2_clip <- vect(Bokspits_2_clipper)

Bokspits_2_BLUE <-  rast("E:/Glenn/Botswana/Pix4d/Bokspits_2_MS/4_index/reflectance/Bokspits_2_MS_transparent_reflectance_blue.tif")
Bokspits_2_GREEN <-  rast("E:/Glenn/Botswana/Pix4d/Bokspits_2_MS/4_index/reflectance/Bokspits_2_MS_transparent_reflectance_green.tif")
Bokspits_2_RED<-  rast("E:/Glenn/Botswana/Pix4d/Bokspits_2_MS/4_index/reflectance/Bokspits_2_MS_transparent_reflectance_red.tif")
Bokspits_2_RedEdge <-  rast("E:/Glenn/Botswana/Pix4d/Bokspits_2_MS/4_index/reflectance/Bokspits_2_MS_transparent_reflectance_red edge.tif")
Bokspits_2_NIR <-  rast("E:/Glenn/Botswana/Pix4d/Bokspits_2_MS/4_index/reflectance/Bokspits_2_MS_transparent_reflectance_NIR.tif")

#Calculate Vegetation Indices

#--- Calculate SAVI,MSAVI, MSAVI2, MTVI-----

#Planet MSAVI

Bokspits_2_MSAVI = Bokspits_2_NIR + 0.5 - (0.5 * sqrt((2 * Bokspits_2_NIR + 1)^2 - 8 * (Bokspits_2_NIR - (2 * Bokspits_2_RED))))

plot (Bokspits_2_MSAVI)

writeRaster(Bokspits_2_MSAVI,"E:/Glenn/Botswana/Drone_Data_VI/Bokspits_2_MSAVI.tif",overwrite=TRUE)


#Bokspits_2 NDVI
Bokspits_2_NDVI = (Bokspits_2_NIR - Bokspits_2_RED)/(Bokspits_2_NIR +Bokspits_2_RED)

plot (Bokspits_2_NDVI)
writeRaster(Bokspits_2_NDVI,"E:/Glenn/Botswana/Drone_Data_VI/Bokspits_2_NDVI.tif", overwrite=TRUE)


#Bokspits_2 SAVI

L=0.5
Bokspits_2_SAVI= (1 + L)*(Bokspits_2_NIR - Bokspits_2_RED)/(Bokspits_2_NIR + Bokspits_2_RED + L)
plot (Bokspits_2_SAVI)
writeRaster(Bokspits_2_SAVI,"E:/Glenn/Botswana/Drone_Data_VI/Bokspits_2_SAVI.tif", overwrite=TRUE)

#MSAVI2

Bokspits_2_MSAVI2 = (2 * Bokspits_2_NIR + 1 - sqrt( (2 * Bokspits_2_NIR + 1)^2 - 8 * (Bokspits_2_NIR - Bokspits_2_RED) )) / 2 

plot(Bokspits_2_MSAVI2)

writeRaster(Bokspits_2_MSAVI2,"E:/Glenn/Botswana/Drone_Data_VI/Bokspits_2_MSAVI2.tif", overwrite=TRUE)

#MTVI
#Modified Triangular Vegetation Index 2 (MTVI)

Bokspits_2_MTVI = 1.5 * (1.2 * (Bokspits_2_NIR - Bokspits_2_GREEN) - 2.5 * (Bokspits_2_RED - Bokspits_2_GREEN)) /  sqrt( (2 * Bokspits_2_NIR + 1)^2 - (6 * Bokspits_2_NIR - 5 * sqrt(Bokspits_2_RED) - 0.5) )
plot(Bokspits_2_MTVI)

writeRaster(Bokspits_2_MTVI,"E:/Glenn/Botswana/Drone_Data_VI/Bokspits_2_MTVI.tif", overwrite=TRUE)


#----3. Bokspits_3 Drone survey Image Data - calculate Vegetation indices----

# Read in data
Bokspits_3_clipper <- read_sf(dsn = 'E:/Glenn/Botswana/Final_Drone_Survey_Data/Bokspits_3', layer = "Bokspits_3_clip")
Bokspits_3_clip <- vect(Bokspits_3_clipper)

Bokspits_3_BLUE <-  rast("E:/Glenn/Botswana/Pix4d/Bokspits_3_MS/4_index/reflectance/Bokspits_3_MS_transparent_reflectance_blue.tif")
Bokspits_3_GREEN <-  rast("E:/Glenn/Botswana/Pix4d/Bokspits_3_MS/4_index/reflectance/Bokspits_3_MS_transparent_reflectance_green.tif")
Bokspits_3_RED<-  rast("E:/Glenn/Botswana/Pix4d/Bokspits_3_MS/4_index/reflectance/Bokspits_3_MS_transparent_reflectance_red.tif")
Bokspits_3_RedEdge <-  rast("E:/Glenn/Botswana/Pix4d/Bokspits_3_MS/4_index/reflectance/Bokspits_3_MS_transparent_reflectance_red edge.tif")
Bokspits_3_NIR <-  rast("E:/Glenn/Botswana/Pix4d/Bokspits_3_MS/4_index/reflectance/Bokspits_3_MS_transparent_reflectance_NIR.tif")

#Calculate Vegetation Indices

#--- Calculate SAVI,MSAVI, MSAVI2, MTVI-----

#Planet MSAVI

Bokspits_3_MSAVI = Bokspits_3_NIR + 0.5 - (0.5 * sqrt((2 * Bokspits_3_NIR + 1)^2 - 8 * (Bokspits_3_NIR - (2 * Bokspits_3_RED))))

plot (Bokspits_3_MSAVI)

writeRaster(Bokspits_3_MSAVI,"E:/Glenn/Botswana/Drone_Data_VI/Bokspits_3_MSAVI.tif",overwrite=TRUE)


#Bokspits_3 NDVI
Bokspits_3_NDVI = (Bokspits_3_NIR - Bokspits_3_RED)/(Bokspits_3_NIR +Bokspits_3_RED)

plot (Bokspits_3_NDVI)
writeRaster(Bokspits_3_NDVI,"E:/Glenn/Botswana/Drone_Data_VI/Bokspits_3_NDVI.tif", overwrite=TRUE)


#Bokspits_3 SAVI

L=0.5
Bokspits_3_SAVI= (1 + L)*(Bokspits_3_NIR - Bokspits_3_RED)/(Bokspits_3_NIR + Bokspits_3_RED + L)
plot (Bokspits_3_SAVI)
writeRaster(Bokspits_3_SAVI,"E:/Glenn/Botswana/Drone_Data_VI/Bokspits_3_SAVI.tif", overwrite=TRUE)

#MSAVI2

Bokspits_3_MSAVI2 = (2 * Bokspits_3_NIR + 1 - sqrt( (2 * Bokspits_3_NIR + 1)^2 - 8 * (Bokspits_3_NIR - Bokspits_3_RED) )) / 2 

plot(Bokspits_3_MSAVI2)

writeRaster(Bokspits_3_MSAVI2,"E:/Glenn/Botswana/Drone_Data_VI/Bokspits_3_MSAVI2.tif", overwrite=TRUE)

#MTVI
#Modified Triangular Vegetation Index 2 (MTVI)

Bokspits_3_MTVI = 1.5 * (1.2 * (Bokspits_3_NIR - Bokspits_3_GREEN) - 2.5 * (Bokspits_3_RED - Bokspits_3_GREEN)) /  sqrt( (2 * Bokspits_3_NIR + 1)^2 - (6 * Bokspits_3_NIR - 5 * sqrt(Bokspits_3_RED) - 0.5) )
plot(Bokspits_3_MTVI)

writeRaster(Bokspits_3_MTVI,"E:/Glenn/Botswana/Drone_Data_VI/Bokspits_3_MTVI.tif", overwrite=TRUE)

#----4. Struizendam_1 Drone survey Image Data - calculate Vegetation indices----

# Read in data
Struizendam_1_clipper <- read_sf(dsn = 'E:/Glenn/Botswana/Final_Drone_Survey_Data/Struizendam_1', layer = "Struizendam_1_clip")
Struizendam_1_clip <- vect(Struizendam_1_clipper)

Struizendam_1_BLUE <-  rast("E:/Glenn/Botswana/Pix4d/Struizendam_1_MS/4_index/reflectance/Struizendam_1_MS_transparent_reflectance_blue.tif")
Struizendam_1_GREEN <-  rast("E:/Glenn/Botswana/Pix4d/Struizendam_1_MS/4_index/reflectance/Struizendam_1_MS_transparent_reflectance_green.tif")
Struizendam_1_RED<-  rast("E:/Glenn/Botswana/Pix4d/Struizendam_1_MS/4_index/reflectance/Struizendam_1_MS_transparent_reflectance_red.tif")
Struizendam_1_RedEdge <-  rast("E:/Glenn/Botswana/Pix4d/Struizendam_1_MS/4_index/reflectance/Struizendam_1_MS_transparent_reflectance_red edge.tif")
Struizendam_1_NIR <-  rast("E:/Glenn/Botswana/Pix4d/Struizendam_1_MS/4_index/reflectance/Struizendam_1_MS_transparent_reflectance_NIR.tif")

#Calculate Vegetation Indices

#--- Calculate SAVI,MSAVI, MSAVI2, MTVI-----

#Planet MSAVI

Struizendam_1_MSAVI = Struizendam_1_NIR + 0.5 - (0.5 * sqrt((2 * Struizendam_1_NIR + 1)^2 - 8 * (Struizendam_1_NIR - (2 * Struizendam_1_RED))))

plot (Struizendam_1_MSAVI)

writeRaster(Struizendam_1_MSAVI,"E:/Glenn/Botswana/Drone_Data_VI/Struizendam_1_MSAVI.tif",overwrite=TRUE)


#Struizendam_1 NDVI
Struizendam_1_NDVI = (Struizendam_1_NIR - Struizendam_1_RED)/(Struizendam_1_NIR +Struizendam_1_RED)

plot (Struizendam_1_NDVI)
writeRaster(Struizendam_1_NDVI,"E:/Glenn/Botswana/Drone_Data_VI/Struizendam_1_NDVI.tif", overwrite=TRUE)


#Struizendam_1 SAVI

L=0.5
Struizendam_1_SAVI= (1 + L)*(Struizendam_1_NIR - Struizendam_1_RED)/(Struizendam_1_NIR + Struizendam_1_RED + L)
plot (Struizendam_1_SAVI)
writeRaster(Struizendam_1_SAVI,"E:/Glenn/Botswana/Drone_Data_VI/Struizendam_1_SAVI.tif", overwrite=TRUE)

#MSAVI2

Struizendam_1_MSAVI2 = (2 * Struizendam_1_NIR + 1 - sqrt( (2 * Struizendam_1_NIR + 1)^2 - 8 * (Struizendam_1_NIR - Struizendam_1_RED) )) / 2 

plot(Struizendam_1_MSAVI2)

writeRaster(Struizendam_1_MSAVI2,"E:/Glenn/Botswana/Drone_Data_VI/Struizendam_1_MSAVI2.tif", overwrite=TRUE)

#MTVI
#Modified Triangular Vegetation Index 2 (MTVI)

Struizendam_1_MTVI = 1.5 * (1.2 * (Struizendam_1_NIR - Struizendam_1_GREEN) - 2.5 * (Struizendam_1_RED - Struizendam_1_GREEN)) /  sqrt( (2 * Struizendam_1_NIR + 1)^2 - (6 * Struizendam_1_NIR - 5 * sqrt(Struizendam_1_RED) - 0.5) )
plot(Struizendam_1_MTVI)

writeRaster(Struizendam_1_MTVI,"E:/Glenn/Botswana/Drone_Data_VI/Struizendam_1_MTVI.tif", overwrite=TRUE)



#----5. Struizendam_2 Drone survey Image Data - calculate Vegetation indices----

# Read in data
Struizendam_2_clipper <- read_sf(dsn = 'E:/Glenn/Botswana/Final_Drone_Survey_Data/Struizendam_2', layer = "Struizendam_2_clip")
Struizendam_2_clip <- vect(Struizendam_2_clipper)

Struizendam_2_BLUE <-  rast("E:/Glenn/Botswana/Pix4d/Struizendam_2_MS/4_index/reflectance/Struizendam_2_MS_transparent_reflectance_blue.tif")
Struizendam_2_GREEN <-  rast("E:/Glenn/Botswana/Pix4d/Struizendam_2_MS/4_index/reflectance/Struizendam_2_MS_transparent_reflectance_green.tif")
Struizendam_2_RED<-  rast("E:/Glenn/Botswana/Pix4d/Struizendam_2_MS/4_index/reflectance/Struizendam_2_MS_transparent_reflectance_red.tif")
Struizendam_2_RedEdge <-  rast("E:/Glenn/Botswana/Pix4d/Struizendam_2_MS/4_index/reflectance/Struizendam_2_MS_transparent_reflectance_red edge.tif")
Struizendam_2_NIR <-  rast("E:/Glenn/Botswana/Pix4d/Struizendam_2_MS/4_index/reflectance/Struizendam_2_MS_transparent_reflectance_NIR.tif")

#Calculate Vegetation Indices

#--- Calculate SAVI,MSAVI, MSAVI2, MTVI-----

#Planet MSAVI

Struizendam_2_MSAVI = Struizendam_2_NIR + 0.5 - (0.5 * sqrt((2 * Struizendam_2_NIR + 1)^2 - 8 * (Struizendam_2_NIR - (2 * Struizendam_2_RED))))

plot (Struizendam_2_MSAVI)

writeRaster(Struizendam_2_MSAVI,"E:/Glenn/Botswana/Drone_Data_VI/Struizendam_2_MSAVI.tif",overwrite=TRUE)


#Struizendam_2 NDVI
Struizendam_2_NDVI = (Struizendam_2_NIR - Struizendam_2_RED)/(Struizendam_2_NIR +Struizendam_2_RED)

plot (Struizendam_2_NDVI)
writeRaster(Struizendam_2_NDVI,"E:/Glenn/Botswana/Drone_Data_VI/Struizendam_2_NDVI.tif", overwrite=TRUE)


#Struizendam_2 SAVI

L=0.5
Struizendam_2_SAVI= (1 + L)*(Struizendam_2_NIR - Struizendam_2_RED)/(Struizendam_2_NIR + Struizendam_2_RED + L)
plot (Struizendam_2_SAVI)
writeRaster(Struizendam_2_SAVI,"E:/Glenn/Botswana/Drone_Data_VI/Struizendam_2_SAVI.tif", overwrite=TRUE)

#MSAVI2

Struizendam_2_MSAVI2 = (2 * Struizendam_2_NIR + 1 - sqrt( (2 * Struizendam_2_NIR + 1)^2 - 8 * (Struizendam_2_NIR - Struizendam_2_RED) )) / 2 

plot(Struizendam_2_MSAVI2)

writeRaster(Struizendam_2_MSAVI2,"E:/Glenn/Botswana/Drone_Data_VI/Struizendam_2_MSAVI2.tif", overwrite=TRUE)

#MTVI
#Modified Triangular Vegetation Index 2 (MTVI)

Struizendam_2_MTVI = 1.5 * (1.2 * (Struizendam_2_NIR - Struizendam_2_GREEN) - 2.5 * (Struizendam_2_RED - Struizendam_2_GREEN)) /  sqrt( (2 * Struizendam_2_NIR + 1)^2 - (6 * Struizendam_2_NIR - 5 * sqrt(Struizendam_2_RED) - 0.5) )
plot(Struizendam_2_MTVI)

writeRaster(Struizendam_2_MTVI,"E:/Glenn/Botswana/Drone_Data_VI/Struizendam_2_MTVI.tif", overwrite=TRUE)


#----6. Struizendam_3 Drone survey Image Data - calculate Vegetation indices----

# Read in data
Struizendam_3_clipper <- read_sf(dsn = 'E:/Glenn/Botswana/Final_Drone_Survey_Data/Struizendam_3', layer = "Struizendam_3_clip")
Struizendam_3_clip <- vect(Struizendam_3_clipper)

Struizendam_3_BLUE <-  rast("E:/Glenn/Botswana/Pix4d/Struizendam_3_MS/4_index/reflectance/Struizendam_3_MS_transparent_reflectance_blue.tif")
Struizendam_3_GREEN <-  rast("E:/Glenn/Botswana/Pix4d/Struizendam_3_MS/4_index/reflectance/Struizendam_3_MS_transparent_reflectance_green.tif")
Struizendam_3_RED<-  rast("E:/Glenn/Botswana/Pix4d/Struizendam_3_MS/4_index/reflectance/Struizendam_3_MS_transparent_reflectance_red.tif")
Struizendam_3_RedEdge <-  rast("E:/Glenn/Botswana/Pix4d/Struizendam_3_MS/4_index/reflectance/Struizendam_3_MS_transparent_reflectance_red edge.tif")
Struizendam_3_NIR <-  rast("E:/Glenn/Botswana/Pix4d/Struizendam_3_MS/4_index/reflectance/Struizendam_3_MS_transparent_reflectance_NIR.tif")

#Calculate Vegetation Indices

#--- Calculate SAVI,MSAVI, MSAVI2, MTVI-----

#Planet MSAVI

Struizendam_3_MSAVI = Struizendam_3_NIR + 0.5 - (0.5 * sqrt((2 * Struizendam_3_NIR + 1)^2 - 8 * (Struizendam_3_NIR - (2 * Struizendam_3_RED))))

plot (Struizendam_3_MSAVI)

writeRaster(Struizendam_3_MSAVI,"E:/Glenn/Botswana/Drone_Data_VI/Struizendam_3_MSAVI.tif",overwrite=TRUE)


#Struizendam_3 NDVI
Struizendam_3_NDVI = (Struizendam_3_NIR - Struizendam_3_RED)/(Struizendam_3_NIR +Struizendam_3_RED)

plot (Struizendam_3_NDVI)
writeRaster(Struizendam_3_NDVI,"E:/Glenn/Botswana/Drone_Data_VI/Struizendam_3_NDVI.tif", overwrite=TRUE)


#Struizendam_3 SAVI

L=0.5
Struizendam_3_SAVI= (1 + L)*(Struizendam_3_NIR - Struizendam_3_RED)/(Struizendam_3_NIR + Struizendam_3_RED + L)
plot (Struizendam_3_SAVI)
writeRaster(Struizendam_3_SAVI,"E:/Glenn/Botswana/Drone_Data_VI/Struizendam_3_SAVI.tif", overwrite=TRUE)

#MSAVI2

Struizendam_3_MSAVI2 = (2 * Struizendam_3_NIR + 1 - sqrt( (2 * Struizendam_3_NIR + 1)^2 - 8 * (Struizendam_3_NIR - Struizendam_3_RED) )) / 2 

plot(Struizendam_3_MSAVI2)

writeRaster(Struizendam_3_MSAVI2,"E:/Glenn/Botswana/Drone_Data_VI/Struizendam_3_MSAVI2.tif", overwrite=TRUE)

#MTVI
#Modified Triangular Vegetation Index 2 (MTVI)

Struizendam_3_MTVI = 1.5 * (1.2 * (Struizendam_3_NIR - Struizendam_3_GREEN) - 2.5 * (Struizendam_3_RED - Struizendam_3_GREEN)) /  sqrt( (2 * Struizendam_3_NIR + 1)^2 - (6 * Struizendam_3_NIR - 5 * sqrt(Struizendam_3_RED) - 0.5) )
plot(Struizendam_3_MTVI)

writeRaster(Struizendam_3_MTVI,"E:/Glenn/Botswana/Drone_Data_VI/Struizendam_3_MTVI.tif", overwrite=TRUE)


#----7. Struizendam_4 Drone survey Image Data - calculate Vegetation indices----

# Read in data
Struizendam_4_clipper <- read_sf(dsn = 'E:/Glenn/Botswana/Final_Drone_Survey_Data/Struizendam_4', layer = "Struizendam_4_clip")
Struizendam_4_clip <- vect(Struizendam_4_clipper)

Struizendam_4_BLUE <-  rast("E:/Glenn/Botswana/Pix4d/Struizendam_4_MS/4_index/reflectance/Struizendam_4_MS_transparent_reflectance_blue.tif")
Struizendam_4_GREEN <-  rast("E:/Glenn/Botswana/Pix4d/Struizendam_4_MS/4_index/reflectance/Struizendam_4_MS_transparent_reflectance_green.tif")
Struizendam_4_RED<-  rast("E:/Glenn/Botswana/Pix4d/Struizendam_4_MS/4_index/reflectance/Struizendam_4_MS_transparent_reflectance_red.tif")
Struizendam_4_RedEdge <-  rast("E:/Glenn/Botswana/Pix4d/Struizendam_4_MS/4_index/reflectance/Struizendam_4_MS_transparent_reflectance_red edge.tif")
Struizendam_4_NIR <-  rast("E:/Glenn/Botswana/Pix4d/Struizendam_4_MS/4_index/reflectance/Struizendam_4_MS_transparent_reflectance_NIR.tif")

#Calculate Vegetation Indices

#--- Calculate SAVI,MSAVI, MSAVI2, MTVI-----

#Planet MSAVI

Struizendam_4_MSAVI = Struizendam_4_NIR + 0.5 - (0.5 * sqrt((2 * Struizendam_4_NIR + 1)^2 - 8 * (Struizendam_4_NIR - (2 * Struizendam_4_RED))))

plot (Struizendam_4_MSAVI)

writeRaster(Struizendam_4_MSAVI,"E:/Glenn/Botswana/Drone_Data_VI/Struizendam_4_MSAVI.tif",overwrite=TRUE)


#Struizendam_4 NDVI
Struizendam_4_NDVI = (Struizendam_4_NIR - Struizendam_4_RED)/(Struizendam_4_NIR +Struizendam_4_RED)

plot (Struizendam_4_NDVI)
writeRaster(Struizendam_4_NDVI,"E:/Glenn/Botswana/Drone_Data_VI/Struizendam_4_NDVI.tif", overwrite=TRUE)


#Struizendam_4 SAVI

L=0.5
Struizendam_4_SAVI= (1 + L)*(Struizendam_4_NIR - Struizendam_4_RED)/(Struizendam_4_NIR + Struizendam_4_RED + L)
plot (Struizendam_4_SAVI)
writeRaster(Struizendam_4_SAVI,"E:/Glenn/Botswana/Drone_Data_VI/Struizendam_4_SAVI.tif", overwrite=TRUE)

#MSAVI2

Struizendam_4_MSAVI2 = (2 * Struizendam_4_NIR + 1 - sqrt( (2 * Struizendam_4_NIR + 1)^2 - 8 * (Struizendam_4_NIR - Struizendam_4_RED) )) / 2 

plot(Struizendam_4_MSAVI2)

writeRaster(Struizendam_4_MSAVI2,"E:/Glenn/Botswana/Drone_Data_VI/Struizendam_4_MSAVI2.tif", overwrite=TRUE)

#MTVI
#Modified Triangular Vegetation Index 2 (MTVI)

Struizendam_4_MTVI = 1.5 * (1.2 * (Struizendam_4_NIR - Struizendam_4_GREEN) - 2.5 * (Struizendam_4_RED - Struizendam_4_GREEN)) /  sqrt( (2 * Struizendam_4_NIR + 1)^2 - (6 * Struizendam_4_NIR - 5 * sqrt(Struizendam_4_RED) - 0.5) )
plot(Struizendam_4_MTVI)

writeRaster(Struizendam_4_MTVI,"E:/Glenn/Botswana/Drone_Data_VI/Struizendam_4_MTVI.tif", overwrite=TRUE)




