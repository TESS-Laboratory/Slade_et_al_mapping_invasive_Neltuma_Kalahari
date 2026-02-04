### Script to Extract Data from RF Classifications based on PlanetScope ploygons
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
library(xlsx)
library(patchwork)

#-------0. SET Parameters to use for training data set

#Fraction  cover to use to define training class 1.0 = 100%
#Change this to vary the pixel definitions
A = "0.95"

#Fraction  cover to use to define training class for Prosopis 1.0 = 100%
#Change this to vary the pixel definitions
P = "0.95"

#Number of pixels from each Class to use in training data

N = "250"

#----1. Read in files for Planet ----


Planet_Predict <- rast("E:/Glenn/Botswana/Satellite_Data/Planet/Boravast_2022_09_Predict_Map_30_90.tif")
B1_Predict <-rast("E:/Glenn/Botswana/MLR3_classifications/Bokspits_1_Predict_Map.tif")
B2_Predict <- rast("E:/Glenn/Botswana/MLR3_classifications/Bokspits_2_Predict_Map (2).tif")
B3_Predict <- rast("E:/Glenn/Botswana/MLR3_classifications/Bokspits_3_Predict_Map (1).tif")
S1_Predict <- rast("E:/Glenn/Botswana/MLR3_classifications/Struizendam_1_Predict_Map (1).tif")
S2_Predict <- rast("E:/Glenn/Botswana/MLR3_classifications/Struizendam_2_Predict_Map (2).tif")
S3_Predict <- rast("E:/Glenn/Botswana/MLR3_classifications/Struizendam_3_Predict_Map.tif")
S4_Predict <- rast("E:/Glenn/Botswana/MLR3_classifications/Struizendam_4_Predict_Map (1).tif")



#NDVITrend<- rast("E:/Glenn/Botswana/Satellite_Data/Trend_Image_Data/Planet/Trend_Img_NDVI_2000_2022b.tif")
#Planet <-terra::project(NDVITrend, y="EPSG:32734")


## NB You cant use exact extract with SpatVector so re-importing it a a spatial DF
#PlanetWV_Grid_SF <- read_sf(dsn = 'E:/Glenn/Botswana/Satellite_Data/Planet/Planet_Grids', layer = "Planet_Planet_grid")
S1_Grid_SF <- read_sf(dsn = 'E:/Glenn/Botswana/Satellite_Data/Planet/Planet_Grids', layer = "Struizendam_1_Planet_grid")
S2_Grid_SF <- read_sf(dsn = 'E:/Glenn/Botswana/Satellite_Data/Planet/Planet_Grids', layer = "Struizendam_2_Planet_grid")
S3_Grid_SF <- read_sf(dsn = 'E:/Glenn/Botswana/Satellite_Data/Planet/Planet_Grids', layer = "Struizendam_3_Planet_grid")
S4_Grid_SF <- read_sf(dsn = 'E:/Glenn/Botswana/Satellite_Data/Planet/Planet_Grids', layer = "Struizendam_4_Planet_grid")
B1_Grid_SF <- read_sf(dsn = 'E:/Glenn/Botswana/Satellite_Data/Planet/Planet_Grids', layer = "Bokspits_1_Planet_grid")
B2_Grid_SF <- read_sf(dsn = 'E:/Glenn/Botswana/Satellite_Data/Planet/Planet_Grids', layer = "Bokspits_2_Planet_grid")
B3_Grid_SF <- read_sf(dsn = 'E:/Glenn/Botswana/Satellite_Data/Planet/Planet_Grids', layer = "Bokspits_3_Planet_grid")

#---2. Extract data for Planet image from Planet grid



#B1
B1_Predict_Extract <- exact_extract(B1_Predict,B1_Grid_SF,"majority")
names(B1_Predict_Extract) <- c('majority')
B1_Predict_Extract_DF <-bind_cols(B1_Grid_SF,majority = B1_Predict_Extract)

B1_Predict_Extract <- exact_extract(Planet_Predict,B1_Grid_SF,"majority" )
names(B1_Predict_Extract) <- c('Planet')
B1_Predict_Extract_DF <- dplyr::mutate(B1_Predict_Extract_DF, Planet = B1_Predict_Extract)

B1_Predict_Extract <- exact_extract(B1_Predict,B1_Grid_SF,"frac" )
B1_Predict_Extract <- B1_Predict_Extract%>% dplyr::select (frac_1)
B1_Predict_Extract_DF <- dplyr::mutate(B1_Predict_Extract_DF, B1_Predict_Extract)

#B2

B2_Predict_Extract <- exact_extract(B2_Predict,B2_Grid_SF,"majority")
names(B2_Predict_Extract) <- c('majority')
B2_Predict_Extract_DF <-bind_cols(B2_Grid_SF,majority = B2_Predict_Extract)

B2_Predict_Extract <- exact_extract(Planet_Predict,B2_Grid_SF,"majority" )
names(B2_Predict_Extract) <- c('Planet')
B2_Predict_Extract_DF <- dplyr::mutate(B2_Predict_Extract_DF, Planet = B2_Predict_Extract)

B2_Predict_Extract <- exact_extract(B2_Predict,B2_Grid_SF,"frac" )
B2_Predict_Extract <- B2_Predict_Extract%>% dplyr::select (frac_1)
B2_Predict_Extract_DF <- dplyr::mutate(B2_Predict_Extract_DF, B2_Predict_Extract)

#B3

B3_Predict_Extract <- exact_extract(B3_Predict,B3_Grid_SF,"majority")
names(B3_Predict_Extract) <- c('majority')
B3_Predict_Extract_DF <-bind_cols(B3_Grid_SF,majority = B3_Predict_Extract)

B3_Predict_Extract <- exact_extract(Planet_Predict,B3_Grid_SF,"majority" )
names(B3_Predict_Extract) <- c('Planet')
B3_Predict_Extract_DF <- dplyr::mutate(B3_Predict_Extract_DF, Planet = B3_Predict_Extract)
B3_Predict_Extract <- exact_extract(B3_Predict,B3_Grid_SF,"frac" )
B3_Predict_Extract <- B3_Predict_Extract%>% dplyr::select (frac_1)
B3_Predict_Extract_DF <- dplyr::mutate(B3_Predict_Extract_DF, B3_Predict_Extract)
#S1
S1_Predict_Extract <- exact_extract(S1_Predict,S1_Grid_SF,"majority")
names(S1_Predict_Extract) <- c('majority')
S1_Predict_Extract_DF <-bind_cols(S1_Grid_SF,majority = S1_Predict_Extract)

S1_Predict_Extract <- exact_extract(Planet_Predict,S1_Grid_SF,"majority" )
names(S1_Predict_Extract) <- c('Planet')
S1_Predict_Extract_DF <- dplyr::mutate(S1_Predict_Extract_DF, Planet = S1_Predict_Extract)

S1_Predict_Extract <- exact_extract(S1_Predict,S1_Grid_SF,"frac" )
S1_Predict_Extract <- S1_Predict_Extract%>% dplyr::select (frac_1)
S1_Predict_Extract_DF <- dplyr::mutate(S1_Predict_Extract_DF, S1_Predict_Extract)
#S2
S2_Predict_Extract <- exact_extract(S2_Predict,S2_Grid_SF,"majority")
names(S2_Predict_Extract) <- c('majority')
S2_Predict_Extract_DF <-bind_cols(S2_Grid_SF,majority = S2_Predict_Extract)

S2_Predict_Extract <- exact_extract(Planet_Predict,S2_Grid_SF,"majority" )
names(S2_Predict_Extract) <- c('Planet')
S2_Predict_Extract_DF <- dplyr::mutate(S2_Predict_Extract_DF, Planet = S2_Predict_Extract)

S2_Predict_Extract <- exact_extract(S2_Predict,S2_Grid_SF,"frac" )
S2_Predict_Extract <- S2_Predict_Extract%>% dplyr::select (frac_1)
S2_Predict_Extract_DF <- dplyr::mutate(S2_Predict_Extract_DF, S2_Predict_Extract)

#S3
S3_Predict_Extract <- exact_extract(S3_Predict,S3_Grid_SF,"majority")
names(S3_Predict_Extract) <- c('majority')
S3_Predict_Extract_DF <-bind_cols(S3_Grid_SF,majority = S3_Predict_Extract)

S3_Predict_Extract <- exact_extract(Planet_Predict,S3_Grid_SF,"majority" )
names(S3_Predict_Extract) <- c('Planet')
S3_Predict_Extract_DF <- dplyr::mutate(S3_Predict_Extract_DF, Planet = S3_Predict_Extract)

S3_Predict_Extract <- exact_extract(S3_Predict,S3_Grid_SF,"frac" )
S3_Predict_Extract <- S3_Predict_Extract%>% dplyr::select (frac_1)
S3_Predict_Extract_DF <- dplyr::mutate(S3_Predict_Extract_DF, S3_Predict_Extract)

#S4
S4_Predict_Extract <- exact_extract(S4_Predict,S4_Grid_SF,"majority")
names(S4_Predict_Extract) <- c('majority')
S4_Predict_Extract_DF <-bind_cols(S4_Grid_SF,majority = S4_Predict_Extract)

S4_Predict_Extract <- exact_extract(Planet_Predict,S4_Grid_SF,"majority" )
names(S4_Predict_Extract) <- c('Planet')
S4_Predict_Extract_DF <- dplyr::mutate(S4_Predict_Extract_DF, Planet = S4_Predict_Extract)

S4_Predict_Extract <- exact_extract(S4_Predict,S4_Grid_SF,"frac" )
S4_Predict_Extract <- S4_Predict_Extract%>% dplyr::select (frac_1)
S4_Predict_Extract_DF <- dplyr::mutate(S4_Predict_Extract_DF, S4_Predict_Extract)

Full_Planet <- rbind(B1_Predict_Extract_DF,B2_Predict_Extract_DF,B3_Predict_Extract_DF,S1_Predict_Extract_DF,S2_Predict_Extract_DF,S3_Predict_Extract_DF,S4_Predict_Extract_DF)



write_xlsx(Full_Planet, "E:/Glenn/Botswana/R_Scripts/Glenn-Prosopis-ML/data_out/Planet_Drone_extract_accuracy_assesment.xlsx")


# Now to plot

theme_fancy <- function() {
  theme_bw() +
    theme(
      text = element_text(family = "Helvetica"),
      axis.text = element_text(size = 6, color = "black"),
      axis.title = element_text(size = 7, color = "black"),
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
        size = 10,
        vjust = 1,
        hjust = 0.5,
        color = "black"
      ),
      legend.text = element_text(size = 10, color = "black"),
      legend.title = element_text(size = 10, color = "black"),
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


Prospis_predict <- filter(Full_Planet,Planet == "1")
Prospis_predict <- filter(Prospis_predict,majority <8)

p1 <- ggplot(aes(x=factor(majority)), data = Prospis_predict)+ geom_bar(stat = "count")+ 
  scale_x_discrete(labels=c("1" = "Prosopis", "2"="BG", "3" = "Grass", "4" = "F","5" = "VE","6" = "RT", "7" = "OW"))+
    xlab("Class")+ ylab("Number of pixels")+theme_fancy()

p1     

Prospis_predict2 <- filter(Full_Planet,majority == "1")
Prospis_predict3 <- filter(Prospis_predict2,Planet <8)

p2 <- ggplot(aes(x=factor(Planet)), data = Prospis_predict3)+ geom_bar(stat = "count")+ 
  scale_x_discrete(labels=c("1" = "Prosopis", "2"="BG", "3" = "Grass", "4" = "F","5" = "VE","6" = "RT","7" = "OW"))+
    xlab("Class")+ ylab("Number of pixels")+theme_fancy()

p2 

patchwork2 <- p1+p2

patchworkf<- patchwork2 + plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 10))

patchworkf

ggsave(
  patchworkf,
  # filename = "/plots/test.png",
  filename = "output_data/Planet accuracy comparison with drone.png",
  width = 16,
  height = 10,
  units = "cm"
)
