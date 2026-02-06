### Script to Extract Data from WV2 image data for field collected training polygons
### matching pixels

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
library (raster)
library(dplyr)
library(sfheaders)


#----1. Read in files for WV2 ----


WV2 <- rast("E:/Glenn/Botswana/Satellite_Data/WV2/1_6_m_mosaic/WV2_Corrected.tif")
WV2_NDVI <- rast("E:/Glenn/Botswana/Satellite_Data/WV2/1_6_m_mosaic/WV2_NDVI.tif")
WV2_MSAVI <- rast("E:/Glenn/Botswana/Satellite_Data/WV2/1_6_m_mosaic/WV2_MSAVI.tif")
WV2_SAVI <- rast("E:/Glenn/Botswana/Satellite_Data/WV2/1_6_m_mosaic/WV2_SAVI.tif")
WV2_MSAVI2 <- rast("E:/Glenn/Botswana/Satellite_Data/WV2/1_6_m_mosaic/WV2_MSAVI2.tif")
WV2_MTVI <- rast("E:/Glenn/Botswana/Satellite_Data/WV2/1_6_m_mosaic/WV2_MTVI.tif")

## NB You cant use exact extract with SpatVector so re-importing it a a spatial DF
All_train_points <- read_sf(dsn = 'E:/Glenn/Botswana/Final_Drone_Survey_Data/', layer = "Boravast_all_train_val_combined_b30_WV2")
#NB training points may not be representative of the entire 1.6m pixle but its a start... need to get pixles with 100% coverage from classified drone image data
#---2. Extract data for WV2 Grid

All_train_points

WV2_Extract <- exact_extract(WV2$`21OCT19084820-M2AS_R1C1-050132273010_01_P002_1`,All_train_points,"mean")
names(WV2_Extract) <- c('blue')
WV2_Extract_DF <-bind_cols(All_train_points,WV2_Extract)

WV2_Extract <- exact_extract(WV2$`21OCT19084820-M2AS_R1C1-050132273010_01_P002_1`,All_train_points,"mean")
names(WV2_Extract) <- c('blue')
WV2_Extract_DF <- dplyr::mutate(WV2_Extract_DF, blue = WV2_Extract)

WV2_Extract <- exact_extract(WV2$`21OCT19084820-M2AS_R1C1-050132273010_01_P002_2`,All_train_points,"mean")
names(WV2_Extract) <- c('green')
WV2_Extract_DF <- dplyr::mutate(WV2_Extract_DF, green = WV2_Extract)

WV2_Extract <- exact_extract(WV2$`21OCT19084820-M2AS_R1C1-050132273010_01_P002_3`,All_train_points,"mean" )
names(WV2_Extract) <- c('red')
WV2_Extract_DF <- dplyr::mutate(WV2_Extract_DF, red = WV2_Extract)

WV2_Extract <- exact_extract(WV2$`21OCT19084820-M2AS_R1C1-050132273010_01_P002_4`,All_train_points,"mean" )
names(WV2_Extract) <- c('nir')
WV2_Extract_DF <- dplyr::mutate(WV2_Extract_DF, nir = WV2_Extract)

WV2_Extract <- exact_extract(WV2_NDVI, All_train_points,"mean" )
names(WV2_Extract) <- c('NDVI')
WV2_Extract_DF <- dplyr::mutate(WV2_Extract_DF, NDVI = WV2_Extract)

WV2_Extract <- exact_extract(WV2_MSAVI, All_train_points,"mean" )
names(WV2_Extract) <- c('MSAVI')
WV2_Extract_DF <- dplyr::mutate(WV2_Extract_DF, MSAVI = WV2_Extract)

WV2_Extract <- exact_extract(WV2_MTVI, All_train_points,"mean" )
names(WV2_Extract) <- c('MTVI')
WV2_Extract_DF <- dplyr::mutate(WV2_Extract_DF, MTVI = WV2_Extract)

WV2_Extract <- exact_extract(WV2_SAVI, All_train_points,"mean" )
names(WV2_Extract) <- c('SAVI')
WV2_Extract_DF <- dplyr::mutate(WV2_Extract_DF, SAVI = WV2_Extract)

WV2_Extract <- exact_extract(WV2_MSAVI2, All_train_points,"mean" )
names(WV2_Extract) <- c('MSAVI2')
WV2_Extract_DF <- dplyr::mutate(WV2_Extract_DF, MSAVI2 = WV2_Extract)


WV2_Extract_DF


WV2_Extract_DF$Type <- as.numeric(WV2_Extract_DF$Type)

WV2_Extract_DF

WV2_Extract_DF <- WV2_Extract_DF%>% st_drop_geometry()# turn sf to a df

WV2_Extract_DF

WV2_Extract_DF <- WV2_Extract_DF[ , unlist(lapply(WV2_Extract_DF,             # Remove non-numeric columns
                                                   is.numeric))]
WV2_Extract_DF
#WV2_Extract_DF <- WV2_Extract_DF[,-3]# removes the third column
#WV2_Extract_DF <- WV2_Extract_DF[,-3]
#WV2_Extract_DF <- WV2_Extract_DF[,-3]



names <- c('Type')
WV2_Extract_DF[,names] <- lapply(WV2_Extract_DF[,names] , factor)# makes the columns in names list into a factor - this enables you to group and colour on type etc

WV2_Extract_DF


# add another column
WV2_Extract_DF <- WV2_Extract_DF %>%
  add_column(Class = "Class")

# add descriptive text in class column which related to the Type identifier ie 1 = Prosopis
WV2_Extract_DF <- WV2_Extract_DF%>% mutate(Class = case_when(Type == 1 ~ "Prosopis",Type == 2 ~ "Bare Sand",Type == 3 ~ "Dry Grass",Type == 4~ "Gnidia",
                                                               Type == 5 ~ "Camel Thorn", Type == 6 ~ "Rhigosum Trichotomum",Type == 7 ~ "Senegalia mellifera",
                                                               Type == 8 ~ "Calcrete", Type == 9 ~ "Blue bush", Type==10 ~ "Boscia albitrunca"))


WV2_Extract_DF
#Script for performing a PCA on extracted data
library(ggbiplot) #package for displaying pca results

rds <-WV2_Extract_DF
rds.pca <- prcomp(rds[,c(4:7)], center = TRUE,scale. = TRUE) # enter the columns you want to perform the pca on (must be numerical and the first column in the df must be numerical)
rds.pca

library(ggbiplot)
# plots the PCA component in a cool plot
ggbiplot(rds.pca)
ggbiplot(rds.pca,ellipse=TRUE,  labels=rds$Type, groups=rds$Class)
ggbiplot(rds.pca,ellipse=TRUE, choices=c(3,4), labels=rds$Type, groups=rds$Class)


#save and plot PCA statistics
print(ggscreeplot(rds.pca))

write.csv(rds.pca$rotation, "data_out/pca_pcrcomp_rot.csv", row.names=TRUE)
write.csv(rds.pca$sdev, "data_out/pca_pcrcomp_std.csv", row.names=TRUE)

#Plots Standard Deviation against pca component
pca2 <- read.csv("data_out/pca2std.csv")
head (pca2)
bp <- ggplot(data = pca2, mapping = aes (x=X ,y=x))+ 
  xlab("Principal Component - Standard") +geom_bar(stat="identity")+ ylab("Standard Deviation")
bp


#Plots Percent variation against pca component
sd <- rds.pca$sdev
loadings <- rds.pca$rotation
rownames(loadings) <- colnames(rds.pca)
scores <- rds.pca$x
var <- sd^2
varPercent <- var/sum(var) * 100


bp2 <-ggplot(data = pca2, mapping = aes (x=X ,y=varPercent))+ 
  xlab("Principal Component") +geom_bar(stat="identity")+ ylab("Percent Variation")+# + abline(h=1/ncol(rds.pca)*100, col='red')
  geom_hline(yintercept=9.09, color="red", linewidth=0.5)
bp2


# Unused data wrangling
