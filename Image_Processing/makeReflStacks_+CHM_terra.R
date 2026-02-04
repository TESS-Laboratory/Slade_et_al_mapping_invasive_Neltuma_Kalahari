#Make rast reflectance stacks from Botswana/Pix4d outputs including calibrated reflectance and canopy Height Model

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

#Bokspits 1 stack

# read in data
Bokspits_1_clipper <- read_sf(dsn = 'E:/Glenn/Botswana/Final_Drone_Survey_Data/Bokspits_1', layer = "Bokspits_1_clip")
Bokspits_1_clip <- vect(Bokspits_1_clipper)

Bokspits_1_Blue_Refl <-  rast("E:/Glenn/Botswana/Pix4d/Bokspits_1_MS/4_index/reflectance/Bokspits_1_MS_transparent_reflectance_blue.tif")
Bokspits_1_Green_Refl <-  rast("E:/Glenn/Botswana/Pix4d/Bokspits_1_MS/4_index/reflectance/Bokspits_1_MS_transparent_reflectance_green.tif")
Bokspits_1_Red_Refl <-  rast("E:/Glenn/Botswana/Pix4d/Bokspits_1_MS/4_index/reflectance/Bokspits_1_MS_transparent_reflectance_red.tif")
Bokspits_1_RedEdge_Refl <-  rast("E:/Glenn/Botswana/Pix4d/Bokspits_1_MS/4_index/reflectance/Bokspits_1_MS_transparent_reflectance_red edge.tif")
Bokspits_1_NIR_Refl <-  rast("E:/Glenn/Botswana/Pix4d/Bokspits_1_MS/4_index/reflectance/Bokspits_1_MS_transparent_reflectance_NIR.tif")
Bokspits_1_CHM <- rast("E:/Glenn/Botswana/ReflStacks/Bokspits_1_CHM.tif")


#make stack

Bokspits_1_ReflStack <- c(Bokspits_1_Blue_Refl,Bokspits_1_Green_Refl,Bokspits_1_Red_Refl,Bokspits_1_RedEdge_Refl,Bokspits_1_NIR_Refl) 
#clip stack
Bokspits_1_ReflStackCr <- crop(Bokspits_1_ReflStack,Bokspits_1_clip )
Bokspits_1_ReflStackCrop <- mask(Bokspits_1_ReflStackCr,Bokspits_1_clip )
plot(Bokspits_1_ReflStackCrop)

Bokspits_1_CHMCr <- crop(Bokspits_1_CHM,Bokspits_1_clip )
Bokspits_1_CHMCrop <- mask(Bokspits_1_CHMCr,Bokspits_1_clip )
plot(Bokspits_1_CHMCrop)
Bokspits_1_CHMF <- resample (Bokspits_1_CHMCrop,Bokspits_1_ReflStackCrop,method = "bilinear")
Bokspits_1_Stack_CHM <- c(Bokspits_1_ReflStackCrop,Bokspits_1_CHMF)
plot(Bokspits_1_Stack_CHM)

#write tif files

#writeRaster(Bokspits_1_ReflStack,"E:/Glenn/Botswana/ReflStacks/Bokspits_1_Refl_Stack.tif")
writeRaster(Bokspits_1_Stack_CHM,"E:/Glenn/Botswana/ReflStacks/Bokspits_1_Refl_StackCrop_CHM.tif", overwrite=TRUE)

#Bokspits 2 stack

# read in data
Bokspits_2_clipper <- read_sf(dsn = 'E:/Glenn/Botswana/Final_Drone_Survey_Data/Bokspits_2', layer = "Bokspits_2_clip")
Bokspits_2_clip <- vect(Bokspits_2_clipper)


Bokspits_2_Blue_Refl <-  rast("E:/Glenn/Botswana/Pix4d/Bokspits_2_MS/4_index/reflectance/Bokspits_2_MS_transparent_reflectance_blue.tif")
Bokspits_2_Green_Refl <-  rast("E:/Glenn/Botswana/Pix4d/Bokspits_2_MS/4_index/reflectance/Bokspits_2_MS_transparent_reflectance_green.tif")
Bokspits_2_Red_Refl <-  rast("E:/Glenn/Botswana/Pix4d/Bokspits_2_MS/4_index/reflectance/Bokspits_2_MS_transparent_reflectance_red.tif")
Bokspits_2_RedEdge_Refl <-  rast("E:/Glenn/Botswana/Pix4d/Bokspits_2_MS/4_index/reflectance/Bokspits_2_MS_transparent_reflectance_red edge.tif")
Bokspits_2_NIR_Refl <-  rast("E:/Glenn/Botswana/Pix4d/Bokspits_2_MS/4_index/reflectance/Bokspits_2_MS_transparent_reflectance_NIR.tif")
Bokspits_2_CHM <- rast("E:/Glenn/Botswana/ReflStacks/Bokspits_2_CHM.tif")
#make stack

Bokspits_2_ReflStack <- c(Bokspits_2_Blue_Refl,Bokspits_2_Green_Refl,Bokspits_2_Red_Refl,Bokspits_2_RedEdge_Refl,Bokspits_2_NIR_Refl) 

#clip stack

Bokspits_2_ReflStackCr <- crop(Bokspits_2_ReflStack,Bokspits_2_clip )
Bokspits_2_ReflStackCrop <- mask(Bokspits_2_ReflStackCr,Bokspits_2_clip )
Bokspits_2_CHMCr <- crop(Bokspits_2_CHM,Bokspits_2_clip )
Bokspits_2_CHMCrop <- mask(Bokspits_2_CHMCr,Bokspits_2_clip )
plot(Bokspits_2_CHMCrop)
Bokspits_2_CHMF <- resample (Bokspits_2_CHMCrop,Bokspits_2_ReflStackCrop,method = "bilinear")
Bokspits_2_Stack_CHM <- c(Bokspits_2_ReflStackCrop,Bokspits_2_CHMF)
plot(Bokspits_2_Stack_CHM)



#write tif files

#writeRaster(Bokspits_2_ReflStack,"E:/Glenn/Botswana/ReflStacks/Bokspits_2_Refl_Stack.tif")
writeRaster(Bokspits_2_Stack_CHM,"E:/Glenn/Botswana/ReflStacks/Bokspits_2_Refl_StackCrop_CHM.tif", overwrite=TRUE)

#Bokspits 3 stack

Bokspits_3_clipper <- read_sf(dsn = 'E:/Glenn/Botswana/Final_Drone_Survey_Data/Bokspits_3', layer = "Bokspits_3_clip")
Bokspits_3_clip <- vect(Bokspits_3_clipper)

Bokspits_3_Blue_Refl <-  rast("E:/Glenn/Botswana/Pix4d/Bokspits_3_MS/4_index/reflectance/Bokspits_3_MS_transparent_reflectance_blue.tif")
Bokspits_3_Green_Refl <-  rast("E:/Glenn/Botswana/Pix4d/Bokspits_3_MS/4_index/reflectance/Bokspits_3_MS_transparent_reflectance_green.tif")
Bokspits_3_Red_Refl <-  rast("E:/Glenn/Botswana/Pix4d/Bokspits_3_MS/4_index/reflectance/Bokspits_3_MS_transparent_reflectance_red.tif")
Bokspits_3_RedEdge_Refl <-  rast("E:/Glenn/Botswana/Pix4d/Bokspits_3_MS/4_index/reflectance/Bokspits_3_MS_transparent_reflectance_red edge.tif")
Bokspits_3_NIR_Refl <-  rast("E:/Glenn/Botswana/Pix4d/Bokspits_3_MS/4_index/reflectance/Bokspits_3_MS_transparent_reflectance_NIR.tif")
Bokspits_3_CHM <- rast("E:/Glenn/Botswana/ReflStacks/Bokspits_3_CHM.tif")


#make stack

Bokspits_3_ReflStack <- c(Bokspits_3_Blue_Refl,Bokspits_3_Green_Refl,Bokspits_3_Red_Refl,Bokspits_3_RedEdge_Refl,Bokspits_3_NIR_Refl) 
#clip stack
Bokspits_3_ReflStackCr <- crop(Bokspits_3_ReflStack,Bokspits_3_clip )
Bokspits_3_ReflStackCrop <- mask(Bokspits_3_ReflStackCr,Bokspits_3_clip )
plot(Bokspits_3_ReflStackCrop)

Bokspits_3_CHMCr <- crop(Bokspits_3_CHM,Bokspits_3_clip )
Bokspits_3_CHMCrop <- mask(Bokspits_3_CHMCr,Bokspits_3_clip )
plot(Bokspits_3_CHMCrop)
Bokspits_3_CHMF <- resample (Bokspits_3_CHMCrop,Bokspits_3_ReflStackCrop,method = "bilinear")
Bokspits_3_Stack_CHM <- c(Bokspits_3_ReflStackCrop,Bokspits_3_CHMF)
plot(Bokspits_3_Stack_CHM)

#write tif files

#writeRaster(Bokspits_3_ReflStack,"E:/Glenn/Botswana/ReflStacks/Bokspits_3_Refl_Stack.tif")
writeRaster(Bokspits_3_Stack_CHM,"E:/Glenn/Botswana/ReflStacks/Bokspits_3_Refl_StackCrop_CHM.tif", overwrite=TRUE)

#Struizendam 1 stack

# read in data
Struizendam_1_clipper <- read_sf(dsn = 'E:/Glenn/Botswana/Final_Drone_Survey_Data/Struizendam_1', layer = "Struizendam_1_clip")
Struizendam_1_clip <- vect(Struizendam_1_clipper)

Struizendam_1_Blue_Refl <-  rast("E:/Glenn/Botswana/Pix4d/Struizendam_1_MS/4_index/reflectance/Struizendam_1_MS_transparent_reflectance_blue.tif")
Struizendam_1_Green_Refl <-  rast("E:/Glenn/Botswana/Pix4d/Struizendam_1_MS/4_index/reflectance/Struizendam_1_MS_transparent_reflectance_green.tif")
Struizendam_1_Red_Refl <-  rast("E:/Glenn/Botswana/Pix4d/Struizendam_1_MS/4_index/reflectance/Struizendam_1_MS_transparent_reflectance_red.tif")
Struizendam_1_RedEdge_Refl <-  rast("E:/Glenn/Botswana/Pix4d/Struizendam_1_MS/4_index/reflectance/Struizendam_1_MS_transparent_reflectance_red edge.tif")
Struizendam_1_NIR_Refl <-  rast("E:/Glenn/Botswana/Pix4d/Struizendam_1_MS/4_index/reflectance/Struizendam_1_MS_transparent_reflectance_NIR.tif")
Struizendam_1_CHM <- rast("E:/Glenn/Botswana/ReflStacks/Struizendam_1_CHM.tif")


#make stack

Struizendam_1_ReflStack <- c(Struizendam_1_Blue_Refl,Struizendam_1_Green_Refl,Struizendam_1_Red_Refl,Struizendam_1_RedEdge_Refl,Struizendam_1_NIR_Refl) 
#clip stack
Struizendam_1_ReflStackCr <- crop(Struizendam_1_ReflStack,Struizendam_1_clip )
Struizendam_1_ReflStackCrop <- mask(Struizendam_1_ReflStackCr,Struizendam_1_clip )
plot(Struizendam_1_ReflStackCrop)

Struizendam_1_CHMCr <- crop(Struizendam_1_CHM,Struizendam_1_clip )
Struizendam_1_CHMCrop <- mask(Struizendam_1_CHMCr,Struizendam_1_clip )
plot(Struizendam_1_CHMCrop)
Struizendam_1_CHMF <- resample (Struizendam_1_CHMCrop,Struizendam_1_ReflStackCrop,method = "bilinear")
Struizendam_1_Stack_CHM <- c(Struizendam_1_ReflStackCrop,Struizendam_1_CHMF)
plot(Struizendam_1_Stack_CHM)

#write tif files

#writeRaster(Struizendam_1_ReflStack,"E:/Glenn/Botswana/ReflStacks/Struizendam_1_Refl_Stack.tif")
writeRaster(Struizendam_1_Stack_CHM,"E:/Glenn/Botswana/ReflStacks/Struizendam_1_Refl_StackCrop_CHM.tif", overwrite=TRUE)

#Struizendam 2 stack

# read in data
Struizendam_2_clipper <- read_sf(dsn = 'E:/Glenn/Botswana/Final_Drone_Survey_Data/Struizendam_2', layer = "Struizendam_2_clip")
Struizendam_2_clip <- vect(Struizendam_2_clipper)

Struizendam_2_Blue_Refl <-  rast("E:/Glenn/Botswana/Pix4d/Struizendam_2_MS/4_index/reflectance/Struizendam_2_MS_transparent_reflectance_blue.tif")
Struizendam_2_Green_Refl <-  rast("E:/Glenn/Botswana/Pix4d/Struizendam_2_MS/4_index/reflectance/Struizendam_2_MS_transparent_reflectance_green.tif")
Struizendam_2_Red_Refl <-  rast("E:/Glenn/Botswana/Pix4d/Struizendam_2_MS/4_index/reflectance/Struizendam_2_MS_transparent_reflectance_red.tif")
Struizendam_2_RedEdge_Refl <-  rast("E:/Glenn/Botswana/Pix4d/Struizendam_2_MS/4_index/reflectance/Struizendam_2_MS_transparent_reflectance_red edge.tif")
Struizendam_2_NIR_Refl <-  rast("E:/Glenn/Botswana/Pix4d/Struizendam_2_MS/4_index/reflectance/Struizendam_2_MS_transparent_reflectance_NIR.tif")
Struizendam_2_CHM <- rast("E:/Glenn/Botswana/ReflStacks/Struizendam_2_CHM.tif")


#make stack

Struizendam_2_ReflStack <- c(Struizendam_2_Blue_Refl,Struizendam_2_Green_Refl,Struizendam_2_Red_Refl,Struizendam_2_RedEdge_Refl,Struizendam_2_NIR_Refl) 
#clip stack
Struizendam_2_ReflStackCr <- crop(Struizendam_2_ReflStack,Struizendam_2_clip )
Struizendam_2_ReflStackCrop <- mask(Struizendam_2_ReflStackCr,Struizendam_2_clip )
plot(Struizendam_2_ReflStackCrop)

Struizendam_2_CHMCr <- crop(Struizendam_2_CHM,Struizendam_2_clip )
Struizendam_2_CHMCrop <- mask(Struizendam_2_CHMCr,Struizendam_2_clip )
plot(Struizendam_2_CHMCrop)
Struizendam_2_CHMF <- resample (Struizendam_2_CHMCrop,Struizendam_2_ReflStackCrop,method = "bilinear")
Struizendam_2_Stack_CHM <- c(Struizendam_2_ReflStackCrop,Struizendam_2_CHMF)
plot(Struizendam_2_Stack_CHM)

#write tif files

#writeRaster(Struizendam_2_ReflStack,"E:/Glenn/Botswana/ReflStacks/Struizendam_2_Refl_Stack.tif")
writeRaster(Struizendam_2_Stack_CHM,"E:/Glenn/Botswana/ReflStacks/Struizendam_2_Refl_StackCrop_CHM.tif", overwrite=TRUE)

#Struizendam 3 stack

# read in data
Struizendam_3_clipper <- read_sf(dsn = 'E:/Glenn/Botswana/Final_Drone_Survey_Data/Struizendam_3', layer = "Struizendam_3_clip")
Struizendam_3_clip <- vect(Struizendam_3_clipper)

Struizendam_3_Blue_Refl <-  rast("E:/Glenn/Botswana/Pix4d/Struizendam_3_MS/4_index/reflectance/Struizendam_3_MS_transparent_reflectance_blue.tif")
Struizendam_3_Green_Refl <-  rast("E:/Glenn/Botswana/Pix4d/Struizendam_3_MS/4_index/reflectance/Struizendam_3_MS_transparent_reflectance_green.tif")
Struizendam_3_Red_Refl <-  rast("E:/Glenn/Botswana/Pix4d/Struizendam_3_MS/4_index/reflectance/Struizendam_3_MS_transparent_reflectance_red.tif")
Struizendam_3_RedEdge_Refl <-  rast("E:/Glenn/Botswana/Pix4d/Struizendam_3_MS/4_index/reflectance/Struizendam_3_MS_transparent_reflectance_red edge.tif")
Struizendam_3_NIR_Refl <-  rast("E:/Glenn/Botswana/Pix4d/Struizendam_3_MS/4_index/reflectance/Struizendam_3_MS_transparent_reflectance_NIR.tif")
Struizendam_3_CHM <- rast("E:/Glenn/Botswana/ReflStacks/Struizendam_3_CHM.tif")


#make stack

Struizendam_3_ReflStack <- c(Struizendam_3_Blue_Refl,Struizendam_3_Green_Refl,Struizendam_3_Red_Refl,Struizendam_3_RedEdge_Refl,Struizendam_3_NIR_Refl) 
#clip stack
Struizendam_3_ReflStackCr <- crop(Struizendam_3_ReflStack,Struizendam_3_clip )
Struizendam_3_ReflStackCrop <- mask(Struizendam_3_ReflStackCr,Struizendam_3_clip )
plot(Struizendam_3_ReflStackCrop)

Struizendam_3_CHMCr <- crop(Struizendam_3_CHM,Struizendam_3_clip )
Struizendam_3_CHMCrop <- mask(Struizendam_3_CHMCr,Struizendam_3_clip )
plot(Struizendam_3_CHMCrop)
Struizendam_3_CHMF <- resample (Struizendam_3_CHMCrop,Struizendam_3_ReflStackCrop,method = "bilinear")
Struizendam_3_Stack_CHM <- c(Struizendam_3_ReflStackCrop,Struizendam_3_CHMF)
plot(Struizendam_3_Stack_CHM)

#write tif files

#writeRaster(Struizendam_3_ReflStack,"E:/Glenn/Botswana/ReflStacks/Struizendam_3_Refl_Stack.tif")
writeRaster(Struizendam_3_Stack_CHM,"E:/Glenn/Botswana/ReflStacks/Struizendam_3_Refl_StackCrop_CHM.tif", overwrite=TRUE)

#Struizendam 4 stack

# read in data
Struizendam_4_clipper <- read_sf(dsn = 'E:/Glenn/Botswana/Final_Drone_Survey_Data/Struizendam_4', layer = "Struizendam_4_clip")
Struizendam_4_clip <- vect(Struizendam_4_clipper)

Struizendam_4_Blue_Refl <-  rast("E:/Glenn/Botswana/Pix4d/Struizendam_4_MS/4_index/reflectance/Struizendam_4_MS_transparent_reflectance_blue.tif")
Struizendam_4_Green_Refl <-  rast("E:/Glenn/Botswana/Pix4d/Struizendam_4_MS/4_index/reflectance/Struizendam_4_MS_transparent_reflectance_green.tif")
Struizendam_4_Red_Refl <-  rast("E:/Glenn/Botswana/Pix4d/Struizendam_4_MS/4_index/reflectance/Struizendam_4_MS_transparent_reflectance_red.tif")
Struizendam_4_RedEdge_Refl <-  rast("E:/Glenn/Botswana/Pix4d/Struizendam_4_MS/4_index/reflectance/Struizendam_4_MS_transparent_reflectance_red edge.tif")
Struizendam_4_NIR_Refl <-  rast("E:/Glenn/Botswana/Pix4d/Struizendam_4_MS/4_index/reflectance/Struizendam_4_MS_transparent_reflectance_NIR.tif")
Struizendam_4_CHM <- rast("E:/Glenn/Botswana/ReflStacks/Struizendam_4_CHM.tif")


#make stack

Struizendam_4_ReflStack <- c(Struizendam_4_Blue_Refl,Struizendam_4_Green_Refl,Struizendam_4_Red_Refl,Struizendam_4_RedEdge_Refl,Struizendam_4_NIR_Refl) 
#clip stack
Struizendam_4_ReflStackCr <- crop(Struizendam_4_ReflStack,Struizendam_4_clip )
Struizendam_4_ReflStackCrop <- mask(Struizendam_4_ReflStackCr,Struizendam_4_clip )
plot(Struizendam_4_ReflStackCrop)

Struizendam_4_CHMCr <- crop(Struizendam_4_CHM,Struizendam_4_clip )
Struizendam_4_CHMCrop <- mask(Struizendam_4_CHMCr,Struizendam_4_clip )
plot(Struizendam_4_CHMCrop)
Struizendam_4_CHMF <- resample (Struizendam_4_CHMCrop,Struizendam_4_ReflStackCrop,method = "bilinear")
Struizendam_4_Stack_CHM <- c(Struizendam_4_ReflStackCrop,Struizendam_4_CHMF)
plot(Struizendam_4_Stack_CHM)

#write tif files

#writeRaster(Struizendam_4_ReflStack,"E:/Glenn/Botswana/ReflStacks/Struizendam_4_Refl_Stack.tif")
writeRaster(Struizendam_4_Stack_CHM,"E:/Glenn/Botswana/ReflStacks/Struizendam_4_Refl_StackCrop_CHM.tif", overwrite=TRUE)