
# Co-registration of images to drone data

library(raster)
library(ggplot2)
library(reshape2)
library (RStoolbox)
library(terra)
library(sf)
library(tidyverse)

##read in WV2 image
WV2 <- rast("E:/Glenn/Botswana/Satellite_Data/WV2/2_m_mosaic/E_Boravast_2_m_wide.TIF")

#Lets see how good we can get a simple x,y shift

WV2_Corrected <- shift(WV2,  4,  -6)

writeRaster(WV2_Corrected,"E:/Glenn/Botswana/Satellite_Data/WV2/2_m_mosaic/E_Boravast_2_m_wide_Corrected.tif",overwrite=TRUE)




#NB coregister images requires same number of bands in both image stacks
#make image stack
Struizendam_4_clipper <- read_sf(dsn = 'E:/Glenn/Botswana/Final_Drone_Survey_Data/Struizendam_4', layer = "Struizendam_4_clip")
Struizendam_4_clip <- vect(Struizendam_4_clipper)
Struizendam_4_Blue_Refl <-  rast("E:/Glenn/Botswana/Pix4d/Struizendam_4_MS/4_index/reflectance/Struizendam_4_MS_transparent_reflectance_blue.tif")
Struizendam_4_Green_Refl <-  rast("E:/Glenn/Botswana/Pix4d/Struizendam_4_MS/4_index/reflectance/Struizendam_4_MS_transparent_reflectance_green.tif")
Struizendam_4_Red_Refl <-  rast("E:/Glenn/Botswana/Pix4d/Struizendam_4_MS/4_index/reflectance/Struizendam_4_MS_transparent_reflectance_red.tif")
Struizendam_4_NIR_Refl <-  rast("E:/Glenn/Botswana/Pix4d/Struizendam_4_MS/4_index/reflectance/Struizendam_4_MS_transparent_reflectance_NIR.tif")

Struizendam_4_ReflStack <- c(Struizendam_4_Blue_Refl,Struizendam_4_Green_Refl,Struizendam_4_Red_Refl,Struizendam_4_NIR_Refl) 
#clip stack
Struizendam_4_ReflStackCr <- crop(Struizendam_4_ReflStack,Struizendam_4_clip )
Struizendam_4_ReflStackCrop <- mask(Struizendam_4_ReflStackCr,Struizendam_4_clip )
#plot(Struizendam_4_ReflStackCrop)


reference <- Struizendam_4_ReflStackCrop



## Coregister images (and report statistics)
coreg <- coregisterImages(WV2_Corrected, ref = reference,
                          nSamples = 500, reportStats = TRUE)

## Plot mutual information per shift
plot2 <- ggplot(coreg$MI) + geom_raster(aes(x,y,fill=mi))

plot2
## Plot joint histograms per shift (x/y shift in facet labels)

df <- melt(coreg$jointHist)   
df$L1 <- factor(df$L1, levels = names(coreg$jointHist))
df[df$value == 0, "value"] <- NA ## don't display p = 0
ggplot(df) + geom_raster(aes(x = Var2, y = Var1,fill=value)) + facet_wrap(~L1) + 
  scale_fill_gradientn(name = "p", colours =  heat.colors(10), na.value = NA)


## Compare correction
ggR(reference, sat = 1, alpha = .5) +
  ggR(coreg$coregImg, sat = 1, hue = .5, alpha = 0.5, ggLayer=TRUE) 

