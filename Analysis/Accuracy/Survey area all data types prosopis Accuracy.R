### Script to Extract Data from RF Classifications based on S2Scope ploygons
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

#----1. Read in files for S2 ----


Sen2_Predict <- rast("E:/Glenn/Botswana/Satellite_Data/S2/S2_Predict_Map_60_65.tif")
WV2_Predict <- rast("E:/Glenn/Botswana/Satellite_Data/WV2/1_6_m_mosaic/RF_WV2_additional.tif")
Planet_Predict <- rast("E:/Glenn/Botswana/Satellite_Data/Planet/Boravast_2022_09_Predict_Map_30_90.tif")
LS8_Predict <-rast("E:/Glenn/Botswana/Satellite_Data/LST/LS8/LS8_Predict_Map.tif")
LS8_Time_Predict <- rast("E:/Glenn/Botswana/Satellite_Data/LST/LS8/LS8_Predict_Map_drone_Timeseries_10p.tif")
B1_Predict <-rast("E:/Glenn/Botswana/MLR3_classifications/Bokspits_1_Predict_Map.tif")
B2_Predict <- rast("E:/Glenn/Botswana/MLR3_classifications/Bokspits_2_Predict_Map (2).tif")
B3_Predict <- rast("E:/Glenn/Botswana/MLR3_classifications/Bokspits_3_Predict_Map (1).tif")
S1_Predict <- rast("E:/Glenn/Botswana/MLR3_classifications/Struizendam_1_Predict_Map (1).tif")
S2_Predict <- rast("E:/Glenn/Botswana/MLR3_classifications/Struizendam_2_Predict_Map (2).tif")
S3_Predict <- rast("E:/Glenn/Botswana/MLR3_classifications/Struizendam_3_Predict_Map.tif")
S4_Predict <- rast("E:/Glenn/Botswana/MLR3_classifications/Struizendam_4_Predict_Map (1).tif")



#NDVITrend<- rast("E:/Glenn/Botswana/Satellite_Data/Trend_Image_Data/S2/Trend_Img_NDVI_2000_2022b.tif")
#S2 <-terra::project(NDVITrend, y="EPSG:32734")


## NB You cant use exact extract with SpatVector so re-importing it a a spatial DF
#S2WV_Grid_SF <- read_sf(dsn = 'E:/Glenn/Botswana/Satellite_Data/S2/S2_Grids', layer = "S2_S2_grid")
S1_Grid_SF <- read_sf(dsn = 'E:/Glenn/Botswana/Satellite_Data/S2/S2_Grids', layer = "Struizendam_1_S2_grid")
S2_Grid_SF <- read_sf(dsn = 'E:/Glenn/Botswana/Satellite_Data/S2/S2_Grids', layer = "Struizendam_2_S2_grid")
S3_Grid_SF <- read_sf(dsn = 'E:/Glenn/Botswana/Satellite_Data/S2/S2_Grids', layer = "Struizendam_3_S2_grid")
S4_Grid_SF <- read_sf(dsn = 'E:/Glenn/Botswana/Satellite_Data/S2/S2_Grids', layer = "Struizendam_4_S2_grid")
B1_Grid_SF <- read_sf(dsn = 'E:/Glenn/Botswana/Satellite_Data/S2/S2_Grids', layer = "Bokspits_1_S2_grid")
B2_Grid_SF <- read_sf(dsn = 'E:/Glenn/Botswana/Satellite_Data/S2/S2_Grids', layer = "Bokspits_2_S2_grid")
B3_Grid_SF <- read_sf(dsn = 'E:/Glenn/Botswana/Satellite_Data/S2/S2_Grids', layer = "Bokspits_3_S2_grid")

B1 <-  read_sf(dsn = 'E:/Glenn/Botswana/Final_Drone_Survey_Data/Clips', layer = "Bokspits_1_clip")
B2 <-  read_sf(dsn = 'E:/Glenn/Botswana/Final_Drone_Survey_Data/Clips', layer = "Bokspits_2_clip")
B3 <- read_sf(dsn = 'E:/Glenn/Botswana/Final_Drone_Survey_Data/Clips', layer = "Bokspits_3_clip")
S1 <-read_sf(dsn = 'E:/Glenn/Botswana/Final_Drone_Survey_Data/Clips', layer = "Struizendam_1_clip")
S2 <-read_sf(dsn = 'E:/Glenn/Botswana/Final_Drone_Survey_Data/Clips', layer = "Struizendam_2_clip")
S3 <-read_sf(dsn = 'E:/Glenn/Botswana/Final_Drone_Survey_Data/Clips', layer = "Struizendam_3_clip")
S4 <-read_sf(dsn = 'E:/Glenn/Botswana/Final_Drone_Survey_Data/Clips', layer = "Struizendam_4_clip")
#---2. Extract data for S2 image from S2 grid



#B1
B1_Predict_Extract <- exact_extract(B1_Predict,B1_Grid_SF,"majority")
names(B1_Predict_Extract) <- c('majority')
B1_Predict_Extract_DF <-bind_cols(B1_Grid_SF,majority = B1_Predict_Extract)

B1_Predict_Extract <- exact_extract(Sen2_Predict,B1_Grid_SF,"majority" )
names(B1_Predict_Extract) <- c('S2')
B1_Predict_Extract_DF <- dplyr::mutate(B1_Predict_Extract_DF, S2 = B1_Predict_Extract)

B1_Predict_Extract <- exact_extract(B1_Predict,B1_Grid_SF,"frac" )
B1_Predict_Extract <- B1_Predict_Extract%>% dplyr::select (frac_1)
B1_Predict_Extract_DF <- dplyr::mutate(B1_Predict_Extract_DF, B1_Predict_Extract)

#B2

B2_Predict_Extract <- exact_extract(B2_Predict,B2_Grid_SF,"majority")
names(B2_Predict_Extract) <- c('majority')
B2_Predict_Extract_DF <-bind_cols(B2_Grid_SF,majority = B2_Predict_Extract)

B2_Predict_Extract <- exact_extract(Sen2_Predict,B2_Grid_SF,"majority" )
names(B2_Predict_Extract) <- c('S2')
B2_Predict_Extract_DF <- dplyr::mutate(B2_Predict_Extract_DF, S2 = B2_Predict_Extract)

B2_Predict_Extract <- exact_extract(B2_Predict,B2_Grid_SF,"frac" )
B2_Predict_Extract <- B2_Predict_Extract%>% dplyr::select (frac_1)
B2_Predict_Extract_DF <- dplyr::mutate(B2_Predict_Extract_DF, B2_Predict_Extract)

#B3

B3_Predict_Extract <- exact_extract(B3_Predict,B3_Grid_SF,"majority")
names(B3_Predict_Extract) <- c('majority')
B3_Predict_Extract_DF <-bind_cols(B3_Grid_SF,majority = B3_Predict_Extract)

B3_Predict_Extract <- exact_extract(Sen2_Predict,B3_Grid_SF,"majority" )
names(B3_Predict_Extract) <- c('S2')
B3_Predict_Extract_DF <- dplyr::mutate(B3_Predict_Extract_DF, S2 = B3_Predict_Extract)
B3_Predict_Extract <- exact_extract(B3_Predict,B3_Grid_SF,"frac" )
B3_Predict_Extract <- B3_Predict_Extract%>% dplyr::select (frac_1)
B3_Predict_Extract_DF <- dplyr::mutate(B3_Predict_Extract_DF, B3_Predict_Extract)
#S1
S1_Predict_Extract <- exact_extract(S1_Predict,S1_Grid_SF,"majority")
names(S1_Predict_Extract) <- c('majority')
S1_Predict_Extract_DF <-bind_cols(S1_Grid_SF,majority = S1_Predict_Extract)

S1_Predict_Extract <- exact_extract(Sen2_Predict,S1_Grid_SF,"majority" )
names(S1_Predict_Extract) <- c('S2')
S1_Predict_Extract_DF <- dplyr::mutate(S1_Predict_Extract_DF, S2 = S1_Predict_Extract)

S1_Predict_Extract <- exact_extract(S1_Predict,S1_Grid_SF,"frac" )
S1_Predict_Extract <- S1_Predict_Extract%>% dplyr::select (frac_1)
S1_Predict_Extract_DF <- dplyr::mutate(S1_Predict_Extract_DF, S1_Predict_Extract)
#S2
S2_Predict_Extract <- exact_extract(S2_Predict,S2_Grid_SF,"majority")
names(S2_Predict_Extract) <- c('majority')
S2_Predict_Extract_DF <-bind_cols(S2_Grid_SF,majority = S2_Predict_Extract)

S2_Predict_Extract <- exact_extract(Sen2_Predict,S2_Grid_SF,"majority" )
names(S2_Predict_Extract) <- c('S2')
S2_Predict_Extract_DF <- dplyr::mutate(S2_Predict_Extract_DF, S2 = S2_Predict_Extract)

S2_Predict_Extract <- exact_extract(S2_Predict,S2_Grid_SF,"frac" )
S2_Predict_Extract <- S2_Predict_Extract%>% dplyr::select (frac_1)
S2_Predict_Extract_DF <- dplyr::mutate(S2_Predict_Extract_DF, S2_Predict_Extract)

#S3
S3_Predict_Extract <- exact_extract(S3_Predict,S3_Grid_SF,"majority")
names(S3_Predict_Extract) <- c('majority')
S3_Predict_Extract_DF <-bind_cols(S3_Grid_SF,majority = S3_Predict_Extract)

S3_Predict_Extract <- exact_extract(Sen2_Predict,S3_Grid_SF,"majority" )
names(S3_Predict_Extract) <- c('S2')
S3_Predict_Extract_DF <- dplyr::mutate(S3_Predict_Extract_DF, S2 = S3_Predict_Extract)

S3_Predict_Extract <- exact_extract(S3_Predict,S3_Grid_SF,"frac" )
S3_Predict_Extract <- S3_Predict_Extract%>% dplyr::select (frac_1)
S3_Predict_Extract_DF <- dplyr::mutate(S3_Predict_Extract_DF, S3_Predict_Extract)

#S4
S4_Predict_Extract <- exact_extract(S4_Predict,S4_Grid_SF,"majority")
names(S4_Predict_Extract) <- c('majority')
S4_Predict_Extract_DF <-bind_cols(S4_Grid_SF,majority = S4_Predict_Extract)

S4_Predict_Extract <- exact_extract(Sen2_Predict,S4_Grid_SF,"majority" )
names(S4_Predict_Extract) <- c('S2')
S4_Predict_Extract_DF <- dplyr::mutate(S4_Predict_Extract_DF, S2 = S4_Predict_Extract)

S4_Predict_Extract <- exact_extract(S4_Predict,S4_Grid_SF,"frac" )
S4_Predict_Extract <- S4_Predict_Extract%>% dplyr::select (frac_1)
S4_Predict_Extract_DF <- dplyr::mutate(S4_Predict_Extract_DF, S4_Predict_Extract)

Full_S2 <- rbind(B1_Predict_Extract_DF,B2_Predict_Extract_DF,B3_Predict_Extract_DF,S1_Predict_Extract_DF,S2_Predict_Extract_DF,S3_Predict_Extract_DF,S4_Predict_Extract_DF)



write_xlsx(Full_S2, "E:/Glenn/Botswana/R_Scripts/Glenn-Prosopis-ML/data_out/Sen2_Drone_extract_accuracy_assesment.xlsx")


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


Prospis_predict <- filter(Full_S2,S2 == "1")
#Prospis_predict <- filter(Prospis_predict,majority <8)

p1 <- ggplot(aes(x=factor(majority)), data = Prospis_predict)+ geom_bar(stat = "count")+ 
  scale_x_discrete(labels=c("1" = "Prosopis", "2"="BG", "3" = "Grass", "4" = "F","5" = "VE","6" = "RT", "7" = "OW"))+
    xlab("Class")+ ylab("Number of pixels")+theme_fancy()

p1     

Prospis_predict2 <- filter(Full_S2,majority == "2")
#Prospis_predict3 <- filter(Prospis_predict2,S2 <8)

p2 <- ggplot(aes(x=factor(S2)), data = Prospis_predict2)+ geom_bar(stat = "count")+ 
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
  filename = "output_data/S2 accuracy comparison with drone V2.png",
  width = 16,
  height = 10,
  units = "cm"
)

#Summary results extracted for survey areas

#B1
B1_Predict_Extract <- exact_extract(B1_Predict,B1,"frac" )
B1_Predict_Extract <- B1_Predict_Extract%>% dplyr::select (frac_1)
B1_Predict_Extract<-B1_Predict_Extract %>% dplyr::rename(Drone = frac_1)
B1_Predict_Extract_DF <- bind_cols(B1, B1_Predict_Extract )

B1_Predict_Extract <- exact_extract(WV2_Predict,B1,"frac" )
B1_Predict_Extract <- B1_Predict_Extract%>% dplyr::select (frac_1)
B1_Predict_Extract<-B1_Predict_Extract %>% dplyr::rename(WV2 = frac_1)
B1_Predict_Extract_DF <- dplyr::mutate(B1_Predict_Extract_DF, B1_Predict_Extract)

B1_Predict_Extract <- exact_extract(Sen2_Predict,B1,"frac" )
B1_Predict_Extract <- B1_Predict_Extract%>% dplyr::select (frac_1)
B1_Predict_Extract<-B1_Predict_Extract %>% dplyr::rename(Sen2 = frac_1)
B1_Predict_Extract_DF <- dplyr::mutate(B1_Predict_Extract_DF, B1_Predict_Extract)

B1_Predict_Extract <- exact_extract(Planet_Predict,B1,"frac" )
B1_Predict_Extract <- B1_Predict_Extract%>% dplyr::select (frac_1)
B1_Predict_Extract<-B1_Predict_Extract %>% dplyr::rename(Planet = frac_1)
B1_Predict_Extract_DF <- dplyr::mutate(B1_Predict_Extract_DF, B1_Predict_Extract)

B1_Predict_Extract <- exact_extract(LS8_Predict,B1,"frac" )
B1_Predict_Extract <- B1_Predict_Extract%>% dplyr::select (frac_1)
B1_Predict_Extract<-B1_Predict_Extract %>% dplyr::rename(LS8 = frac_1)
B1_Predict_Extract_DF <- dplyr::mutate(B1_Predict_Extract_DF, B1_Predict_Extract)

B1_Predict_Extract <- exact_extract(LS8_Time_Predict,B1,"frac" )
B1_Predict_Extract <- B1_Predict_Extract%>% dplyr::select (frac_1)
B1_Predict_Extract<-B1_Predict_Extract %>% dplyr::rename(LS8T = frac_1)
B1_Predict_Extract_DF <- dplyr::mutate(B1_Predict_Extract_DF, B1_Predict_Extract)

#B2
B2_Predict_Extract <- exact_extract(B2_Predict,B2,"frac" )
B2_Predict_Extract <- B2_Predict_Extract%>% dplyr::select (frac_1)
B2_Predict_Extract<-B2_Predict_Extract %>% dplyr::rename(Drone = frac_1)
B2_Predict_Extract_DF <- bind_cols(B2, B2_Predict_Extract )

B2_Predict_Extract <- exact_extract(WV2_Predict,B2,"frac" )
B2_Predict_Extract <- B2_Predict_Extract%>% dplyr::select (frac_1)
B2_Predict_Extract<-B2_Predict_Extract %>% dplyr::rename(WV2 = frac_1)
B2_Predict_Extract_DF <- dplyr::mutate(B2_Predict_Extract_DF, B2_Predict_Extract)

B2_Predict_Extract <- exact_extract(Sen2_Predict,B2,"frac" )
B2_Predict_Extract <- B2_Predict_Extract%>% dplyr::select (frac_1)
B2_Predict_Extract<-B2_Predict_Extract %>% dplyr::rename(Sen2 = frac_1)
B2_Predict_Extract_DF <- dplyr::mutate(B2_Predict_Extract_DF, B2_Predict_Extract)

B2_Predict_Extract <- exact_extract(Planet_Predict,B2,"frac" )
B2_Predict_Extract <- B2_Predict_Extract%>% dplyr::select (frac_1)
B2_Predict_Extract<-B2_Predict_Extract %>% dplyr::rename(Planet = frac_1)
B2_Predict_Extract_DF <- dplyr::mutate(B2_Predict_Extract_DF, B2_Predict_Extract)

B2_Predict_Extract <- exact_extract(LS8_Predict,B2,"frac" )
B2_Predict_Extract <- B2_Predict_Extract%>% dplyr::select (frac_1)
B2_Predict_Extract<-B2_Predict_Extract %>% dplyr::rename(LS8 = frac_1)
B2_Predict_Extract_DF <- dplyr::mutate(B2_Predict_Extract_DF, B2_Predict_Extract)

B2_Predict_Extract <- exact_extract(LS8_Time_Predict,B2,"frac" )
B2_Predict_Extract <- B2_Predict_Extract%>% dplyr::select (frac_1)
B2_Predict_Extract<-B2_Predict_Extract %>% dplyr::rename(LS8T = frac_1)
B2_Predict_Extract_DF <- dplyr::mutate(B2_Predict_Extract_DF, B2_Predict_Extract)
#B3
B3_Predict_Extract <- exact_extract(B3_Predict,B3,"frac" )
B3_Predict_Extract <- B3_Predict_Extract%>% dplyr::select (frac_1)
B3_Predict_Extract<-B3_Predict_Extract %>% dplyr::rename(Drone = frac_1)
B3_Predict_Extract_DF <- bind_cols(B3, B3_Predict_Extract )

B3_Predict_Extract <- exact_extract(WV2_Predict,B3,"frac" )
B3_Predict_Extract <- B3_Predict_Extract%>% dplyr::select (frac_1)
B3_Predict_Extract<-B3_Predict_Extract %>% dplyr::rename(WV2 = frac_1)
B3_Predict_Extract_DF <- dplyr::mutate(B3_Predict_Extract_DF, B3_Predict_Extract)

B3_Predict_Extract <- exact_extract(Sen2_Predict,B3,"frac" )
B3_Predict_Extract <- B3_Predict_Extract%>% dplyr::select (frac_1)
B3_Predict_Extract<-B3_Predict_Extract %>% dplyr::rename(Sen2 = frac_1)
B3_Predict_Extract_DF <- dplyr::mutate(B3_Predict_Extract_DF, B3_Predict_Extract)

B3_Predict_Extract <- exact_extract(Planet_Predict,B3,"frac" )
B3_Predict_Extract <- B3_Predict_Extract%>% dplyr::select (frac_1)
B3_Predict_Extract<-B3_Predict_Extract %>% dplyr::rename(Planet = frac_1)
B3_Predict_Extract_DF <- dplyr::mutate(B3_Predict_Extract_DF, B3_Predict_Extract)

B3_Predict_Extract <- exact_extract(LS8_Predict,B3,"frac" )
B3_Predict_Extract <- B3_Predict_Extract%>% dplyr::select (frac_1)
B3_Predict_Extract<-B3_Predict_Extract %>% dplyr::rename(LS8 = frac_1)
B3_Predict_Extract_DF <- dplyr::mutate(B3_Predict_Extract_DF, B3_Predict_Extract)

B3_Predict_Extract <- exact_extract(LS8_Time_Predict,B3,"frac" )
B3_Predict_Extract <- B3_Predict_Extract%>% dplyr::select (frac_1)
B3_Predict_Extract<-B3_Predict_Extract %>% dplyr::rename(LS8T = frac_1)
B3_Predict_Extract_DF <- dplyr::mutate(B3_Predict_Extract_DF, B3_Predict_Extract)
#S1
S1_Predict_Extract <- exact_extract(S1_Predict,S1,"frac" )
S1_Predict_Extract <- S1_Predict_Extract%>% dplyr::select (frac_1)
S1_Predict_Extract<-S1_Predict_Extract %>% dplyr::rename(Drone = frac_1)
S1_Predict_Extract_DF <- bind_cols(S1, S1_Predict_Extract )

S1_Predict_Extract <- exact_extract(WV2_Predict,S1,"frac" )
S1_Predict_Extract <- S1_Predict_Extract%>% dplyr::select (frac_1)
S1_Predict_Extract<-S1_Predict_Extract %>% dplyr::rename(WV2 = frac_1)
S1_Predict_Extract_DF <- dplyr::mutate(S1_Predict_Extract_DF, S1_Predict_Extract)

S1_Predict_Extract <- exact_extract(Sen2_Predict,S1,"frac" )
S1_Predict_Extract <- S1_Predict_Extract%>% dplyr::select (frac_1)
S1_Predict_Extract<-S1_Predict_Extract %>% dplyr::rename(Sen2 = frac_1)
S1_Predict_Extract_DF <- dplyr::mutate(S1_Predict_Extract_DF, S1_Predict_Extract)

S1_Predict_Extract <- exact_extract(Planet_Predict,S1,"frac" )
S1_Predict_Extract <- S1_Predict_Extract%>% dplyr::select (frac_1)
S1_Predict_Extract<-S1_Predict_Extract %>% dplyr::rename(Planet = frac_1)
S1_Predict_Extract_DF <- dplyr::mutate(S1_Predict_Extract_DF, S1_Predict_Extract)

S1_Predict_Extract <- exact_extract(LS8_Predict,S1,"frac" )
S1_Predict_Extract <- S1_Predict_Extract%>% dplyr::select (frac_1)
S1_Predict_Extract<-S1_Predict_Extract %>% dplyr::rename(LS8 = frac_1)
S1_Predict_Extract_DF <- dplyr::mutate(S1_Predict_Extract_DF, S1_Predict_Extract)

S1_Predict_Extract <- exact_extract(LS8_Time_Predict,S1,"frac" )
S1_Predict_Extract <- S1_Predict_Extract%>% dplyr::select (frac_1)
S1_Predict_Extract<-S1_Predict_Extract %>% dplyr::rename(LS8T = frac_1)
S1_Predict_Extract_DF <- dplyr::mutate(S1_Predict_Extract_DF, S1_Predict_Extract)
#S2
S2_Predict_Extract <- exact_extract(S2_Predict,S2,"frac" )
S2_Predict_Extract <- S2_Predict_Extract%>% dplyr::select (frac_1)
S2_Predict_Extract<-S2_Predict_Extract %>% dplyr::rename(Drone = frac_1)
S2_Predict_Extract_DF <- bind_cols(S2, S2_Predict_Extract )

S2_Predict_Extract <- exact_extract(WV2_Predict,S2,"frac" )
S2_Predict_Extract <- S2_Predict_Extract%>% dplyr::select (frac_1)
S2_Predict_Extract<-S2_Predict_Extract %>% dplyr::rename(WV2 = frac_1)
S2_Predict_Extract_DF <- dplyr::mutate(S2_Predict_Extract_DF, S2_Predict_Extract)

S2_Predict_Extract <- exact_extract(Sen2_Predict,S2,"frac" )
S2_Predict_Extract <- S2_Predict_Extract%>% dplyr::select (frac_1)
S2_Predict_Extract<-S2_Predict_Extract %>% dplyr::rename(Sen2 = frac_1)
S2_Predict_Extract_DF <- dplyr::mutate(S2_Predict_Extract_DF, S2_Predict_Extract)

S2_Predict_Extract <- exact_extract(Planet_Predict,S2,"frac" )
S2_Predict_Extract <- S2_Predict_Extract%>% dplyr::select (frac_1)
S2_Predict_Extract<-S2_Predict_Extract %>% dplyr::rename(Planet = frac_1)
S2_Predict_Extract_DF <- dplyr::mutate(S2_Predict_Extract_DF, S2_Predict_Extract)

S2_Predict_Extract <- exact_extract(LS8_Predict,S2,"frac" )
S2_Predict_Extract <- S2_Predict_Extract%>% dplyr::select (frac_1)
S2_Predict_Extract<-S2_Predict_Extract %>% dplyr::rename(LS8 = frac_1)
S2_Predict_Extract_DF <- dplyr::mutate(S2_Predict_Extract_DF, S2_Predict_Extract)

S2_Predict_Extract <- exact_extract(LS8_Time_Predict,S2,"frac" )
S2_Predict_Extract <-S2_Predict_Extract %>% mutate(frac_1=0 )
S2_Predict_Extract <- S2_Predict_Extract%>% dplyr::select (frac_1)
S2_Predict_Extract<-S2_Predict_Extract %>% dplyr::rename(LS8T = frac_1)
S2_Predict_Extract_DF <- dplyr::mutate(S2_Predict_Extract_DF, S2_Predict_Extract)
#S3
S3_Predict_Extract <- exact_extract(S3_Predict,S3,"frac" )
S3_Predict_Extract <- S3_Predict_Extract%>% dplyr::select (frac_1)
S3_Predict_Extract<-S3_Predict_Extract %>% dplyr::rename(Drone = frac_1)
S3_Predict_Extract_DF <- bind_cols(S3, S3_Predict_Extract )

S3_Predict_Extract <- exact_extract(WV2_Predict,S3,"frac" )
S3_Predict_Extract <- S3_Predict_Extract%>% dplyr::select (frac_1)
S3_Predict_Extract<-S3_Predict_Extract %>% dplyr::rename(WV2 = frac_1)
S3_Predict_Extract_DF <- dplyr::mutate(S3_Predict_Extract_DF, S3_Predict_Extract)

S3_Predict_Extract <- exact_extract(Sen2_Predict,S3,"frac" )
S3_Predict_Extract <- S3_Predict_Extract%>% dplyr::select (frac_1)
S3_Predict_Extract<-S3_Predict_Extract %>% dplyr::rename(Sen2 = frac_1)
S3_Predict_Extract_DF <- dplyr::mutate(S3_Predict_Extract_DF, S3_Predict_Extract)

S3_Predict_Extract <- exact_extract(Planet_Predict,S3,"frac" )
S3_Predict_Extract <- S3_Predict_Extract%>% dplyr::select (frac_1)
S3_Predict_Extract<-S3_Predict_Extract %>% dplyr::rename(Planet = frac_1)
S3_Predict_Extract_DF <- dplyr::mutate(S3_Predict_Extract_DF, S3_Predict_Extract)

S3_Predict_Extract <- exact_extract(LS8_Predict,S3,"frac" )
S3_Predict_Extract <- S3_Predict_Extract%>% dplyr::select (frac_1)
S3_Predict_Extract<-S3_Predict_Extract %>% dplyr::rename(LS8 = frac_1)
S3_Predict_Extract_DF <- dplyr::mutate(S3_Predict_Extract_DF, S3_Predict_Extract)

S3_Predict_Extract <- exact_extract(LS8_Time_Predict,S3,"frac" )
S3_Predict_Extract <-S3_Predict_Extract %>% mutate(frac_1=0 )
S3_Predict_Extract <- S3_Predict_Extract%>% dplyr::select (frac_1)
S3_Predict_Extract<-S3_Predict_Extract %>% dplyr::rename(LS8T = frac_1)
S3_Predict_Extract_DF <- dplyr::mutate(S3_Predict_Extract_DF, S3_Predict_Extract)
#S4
S4_Predict_Extract <- exact_extract(S4_Predict,S4,"frac" )
S4_Predict_Extract <- S4_Predict_Extract%>% dplyr::select (frac_1)
S4_Predict_Extract<-S4_Predict_Extract %>% dplyr::rename(Drone = frac_1)
S4_Predict_Extract_DF <- bind_cols(S4, S4_Predict_Extract )

S4_Predict_Extract <- exact_extract(WV2_Predict,S4,"frac" )
S4_Predict_Extract <- S4_Predict_Extract%>% dplyr::select (frac_1)
S4_Predict_Extract<-S4_Predict_Extract %>% dplyr::rename(WV2 = frac_1)
S4_Predict_Extract_DF <- dplyr::mutate(S4_Predict_Extract_DF, S4_Predict_Extract)

S4_Predict_Extract <- exact_extract(Sen2_Predict,S4,"frac" )
S4_Predict_Extract <- S4_Predict_Extract%>% dplyr::select (frac_1)
S4_Predict_Extract<-S4_Predict_Extract %>% dplyr::rename(Sen2 = frac_1)
S4_Predict_Extract_DF <- dplyr::mutate(S4_Predict_Extract_DF, S4_Predict_Extract)

S4_Predict_Extract <- exact_extract(Planet_Predict,S4,"frac" )
S4_Predict_Extract <- S4_Predict_Extract%>% dplyr::select (frac_1)
S4_Predict_Extract<-S4_Predict_Extract %>% dplyr::rename(Planet = frac_1)
S4_Predict_Extract_DF <- dplyr::mutate(S4_Predict_Extract_DF, S4_Predict_Extract)

S4_Predict_Extract <- exact_extract(LS8_Predict,S4,"frac" )
S4_Predict_Extract <- S4_Predict_Extract%>% dplyr::select (frac_1)
S4_Predict_Extract<-S4_Predict_Extract %>% dplyr::rename(LS8 = frac_1)
S4_Predict_Extract_DF <- dplyr::mutate(S4_Predict_Extract_DF, S4_Predict_Extract)

S4_Predict_Extract <- exact_extract(LS8_Time_Predict,S4,"frac" )
S4_Predict_Extract <- S4_Predict_Extract%>% dplyr::select (frac_1)
S4_Predict_Extract<-S4_Predict_Extract %>% dplyr::rename(LS8T = frac_1)
S4_Predict_Extract_DF <- dplyr::mutate(S4_Predict_Extract_DF, S4_Predict_Extract)


Full_frac1_data <- rbind(B1_Predict_Extract_DF,B2_Predict_Extract_DF,B3_Predict_Extract_DF,S1_Predict_Extract_DF,S2_Predict_Extract_DF,S3_Predict_Extract_DF,S4_Predict_Extract_DF)

write_xlsx(Full_frac1_data, "E:/Glenn/Botswana/R_Scripts/slade-prosopis/output_data/Surveys all data prosopis _extract_assesment.xlsx")
