library (terra)


#------Listing and mosaicing files WV2 files with 1.6m resolution
files <- list.files("E:/Glenn/Botswana/satellite_Data/WV2/050132961020_01/050132961020_01_P001_MUL", recursive = TRUE, full.names = TRUE, ".TIF") 
files   

files2 <- list.files("E:/Glenn/Botswana/satellite_Data/WV2/050132961010_01/050132961010_01_P001_MUL", recursive = TRUE, 
                     full.names = TRUE, ".TIF") 
files2

a1a <-rast( "E:/Glenn/Botswana/satellite_Data/WV2/050132961020_01/050132961020_01_P001_MUL/21OCT19084820-M2AS_R1C1-050132961020_01_P001.TIF")
a2a <-rast( "E:/Glenn/Botswana/satellite_Data/WV2/050132961020_01/050132961020_01_P001_MUL/21OCT19084820-M2AS_R1C2-050132961020_01_P001.TIF")
a3a <-rast("E:/Glenn/Botswana/satellite_Data/WV2/050132961020_01/050132961020_01_P001_MUL/21OCT19084820-M2AS_R1C3-050132961020_01_P001.TIF")
a4a <-rast("E:/Glenn/Botswana/satellite_Data/WV2/050132961020_01/050132961020_01_P001_MUL/21OCT19084820-M2AS_R2C1-050132961020_01_P001.TIF")
a5a <-rast("E:/Glenn/Botswana/satellite_Data/WV2/050132961020_01/050132961020_01_P001_MUL/21OCT19084820-M2AS_R2C2-050132961020_01_P001.TIF")
a6a <-rast("E:/Glenn/Botswana/satellite_Data/WV2/050132961020_01/050132961020_01_P001_MUL/21OCT19084820-M2AS_R2C3-050132961020_01_P001.TIF")
a7a <-rast("E:/Glenn/Botswana/satellite_Data/WV2/050132961020_01/050132961020_01_P001_MUL/21OCT19084820-M2AS_R3C1-050132961020_01_P001.TIF")
a8a <-rast("E:/Glenn/Botswana/satellite_Data/WV2/050132961020_01/050132961020_01_P001_MUL/21OCT19084820-M2AS_R3C2-050132961020_01_P001.TIF")
a9a <-rast("E:/Glenn/Botswana/satellite_Data/WV2/050132961020_01/050132961020_01_P001_MUL/21OCT19084820-M2AS_R3C3-050132961020_01_P001.TIF")
a10a <-rast("E:/Glenn/Botswana/satellite_Data/WV2/050132961020_01/050132961020_01_P001_MUL/21OCT19084820-M2AS_R4C1-050132961020_01_P001.TIF")
a11a <-rast( "E:/Glenn/Botswana/satellite_Data/WV2/050132961020_01/050132961020_01_P001_MUL/21OCT19084820-M2AS_R4C2-050132961020_01_P001.TIF")
a12a <-rast( "E:/Glenn/Botswana/satellite_Data/WV2/050132961020_01/050132961020_01_P001_MUL/21OCT19084820-M2AS_R4C3-050132961020_01_P001.TIF")
a13a <-rast( "E:/Glenn/Botswana/satellite_Data/WV2/050132961020_01/050132961020_01_P001_MUL/21OCT19084820-M2AS_R5C1-050132961020_01_P001.TIF")
a14a <-rast( "E:/Glenn/Botswana/satellite_Data/WV2/050132961020_01/050132961020_01_P001_MUL/21OCT19084820-M2AS_R5C2-050132961020_01_P001.TIF")
a15a <-rast( "E:/Glenn/Botswana/satellite_Data/WV2/050132961020_01/050132961020_01_P001_MUL/21OCT19084820-M2AS_R5C3-050132961020_01_P001.TIF")

a1a

rlist <- list(a1a,a2a,a3a,a4a,a5a,a6a,a7a,a8a,a9a,a10a,a11a,a12a,a13a,a14a,a15a)
rsrc <- sprc(rlist)
m <- mosaic(rsrc, fun="max")
m
writeRaster(m,"E:/Glenn/Botswana/satellite_Data/WV2/1_6_m_mosaic/W_Boravast_1_6_m.TIF", overwrite=TRUE)

#------Listing and mosaicing files WV2 files with 50cm resolution


a1b <-rast( "E:/Glenn/Botswana/satellite_Data/WV2/050132961010_01/050132961010_01_P001_MUL/21OCT19084743-M2AS_R1C1-050132961010_01_P001.TIF")
a2b <-rast("E:/Glenn/Botswana/satellite_Data/WV2/050132961010_01/050132961010_01_P001_MUL/21OCT19084743-M2AS_R2C1-050132961010_01_P001.TIF")
a3b <-rast("E:/Glenn/Botswana/satellite_Data/WV2/050132961010_01/050132961010_01_P001_MUL/21OCT19084743-M2AS_R3C1-050132961010_01_P001.TIF")
a4b <-rast("E:/Glenn/Botswana/satellite_Data/WV2/050132961010_01/050132961010_01_P001_MUL/21OCT19084743-M2AS_R4C1-050132961010_01_P001.TIF")

rlist2 <- list(a1b,a2b,a3b,a4b)
rsrc2 <- sprc(rlist2)
m2 <- mosaic(rsrc2, fun="max")

m2
writeRaster(m2,"E:/Glenn/Botswana/satellite_Data/WV2/2_m_mosaic/E_Boravast_2_m.TIF", overwrite=TRUE)

#----Mosaic two image files based on the higher resolution of the 1.6m image

m <- rast("E:/Glenn/Botswana/satellite_Data/WV2/1_6_m_mosaic/WV2_Corrected.TIF")
m2 <- rast("E:/Glenn/Botswana/satellite_Data/WV2/2_m_mosaic/E_Boravast_2_m_wide_Corrected.TIF")

xy <- vect("E:/Glenn/Botswana/satellite_Data/WV2/AOI/WV2_clip_wide.shp") # brings in vector file with extent of both images
#terra::project(xy, y="EPSG:32734")

#extend the higher resolution 1.6m spatraster tot he ful extent of the study area (vector file extent)
e <- extend(m, xy, snap="near", fill=NA, filename="E:/Glenn/Botswana/satellite_Data/WV2/1_6_m_mosaic/Boravast_1_6_extended.TIF", overwrite=TRUE) 

m2resamp <- resample(m2, e, method="bilinear")# resample the lower resolution image to the higher resolution
m2resamp

rlist3 <- list(m,m2resamp)
rsrc3 <- sprc(rlist3)
m3 <- mosaic(rsrc3, fun="max")
writeRaster(m3,"E:/Glenn/Botswana/satellite_Data/WV2/merged_mosaic/WV2_merged_corrected.TIF", overwrite=TRUE)

m4 <- mosaic(rsrc3, fun="mean")
writeRaster(m4,"E:/Glenn/Botswana/satellite_Data/WV2/WV2_merged_corrected_mean.TIF", overwrite=TRUE)
