
# Co-registration of images to drone data

library(raster)
library(ggplot2)
library(reshape2)
library (RStoolbox)
library(terra)
library(sf)
library(tidyverse)

##read in WV2 image
WV2 <- rast("E:/Glenn/Botswana/Satellite_Data/WV2/1_6_m_mosaic/WV2_Corrected.TIF")
PL <- rast("E:/Glenn/Botswana/Satellite_Data/Planet/Planet_Scope_Boravast/Boravast_2022_09_09.TIF")
Struizendam_4_clipper <- read_sf(dsn = 'E:/Glenn/Botswana/Final_Drone_Survey_Data/Struizendam_4', layer = "Struizendam_4_clip")
Struizendam_4_clip <- vect(Struizendam_4_clipper)


WV2Cr <- crop(WV2,Struizendam_4_clip )
WV2Crop <- mask(WV2Cr,Struizendam_4_clip )

PLCr <- crop(PL,Struizendam_4_clip )
PLCrop <- mask(PLCr,Struizendam_4_clip )
#Lets see how good we can get a simple x,y shift

#PL_Corrected <- shift(PL,  4,  -6)
PL_Corrected <- shift(PL,  0,  1)
writeRaster(PL_Corrected,"E:/Glenn/Botswana/Satellite_Data/Planet/Planet_Scope_Boravast/Boravast_2022_09_09_Corrected.TIF",overwrite=TRUE)

WV2
WV2Crop
PLCrop

PL_Corrected <- shift(PLCrop,  0,  1)
#NB coregister images requires same number of bands in both image stacks

reference <- WV2Crop

## Coregister images (and report statistics)
coreg <- coregisterImages(PLCrop, ref = reference,
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

