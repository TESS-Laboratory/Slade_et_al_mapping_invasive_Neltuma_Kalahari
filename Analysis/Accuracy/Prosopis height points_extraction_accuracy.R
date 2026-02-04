### Script to Extract Data from Drone Classifications based on Drone pixel grid
### matching pixels
### Imports classified drone image data set and calculates fractional cover for each 
### class in each Drone pixel and then samples that data to produce training data set
### for classification of Drone image.

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

#-------0. SET Parameters to use for training data set



# Now for separate prosopis points 


Bokspits_1_Predict <- rast("output_data/Bokspits_1_Predict_Map.tif")
Bokspits_1_Predict

## NB You cant use exact extract with SpatVector so importing it a a spatial DF
Bokspits_1_points<- read_sf(dsn = 'input_data/', layer = "Bokspits_1_Prosopis_height_points_20b")
Bokspits_1_points


#---2. Extract data for Bokspits_1 Grid

Bokspits_1_Drone_Extract <- exact_extract(Bokspits_1_Predict,Bokspits_1_points,"mode")
names(Bokspits_1_Drone_Extract) <- c('mode')
Bokspits_1_Drone_Extract_DF <-bind_cols(Bokspits_1_points,Bokspits_1_Drone_Extract)

Bokspits_1_Drone_Extract <- exact_extract(Bokspits_1_Predict,Bokspits_1_points,"majority" )
names(Bokspits_1_Drone_Extract) <- c('majority')
Bokspits_1_Drone_Extract_DF <- dplyr::mutate(Bokspits_1_Drone_Extract_DF, majority = Bokspits_1_Drone_Extract)

Bokspits_1_Drone_Extract <- exact_extract(Bokspits_1_Predict,Bokspits_1_points,"frac" )# calculates fraction cover for each classification value
#a column is added with the FVC for each vegetation type frac1, frac2 etc...
Bokspits_1_Drone_Extract_DF <- dplyr::mutate(Bokspits_1_Drone_Extract_DF, Bokspits_1_Drone_Extract)

Prosopis_df2 <- Bokspits_1_Drone_Extract_DF  %>% dplyr::select (fid,majority,frac_1, frac_2, frac_3,frac_4,frac_5,frac_6)



# Struizendam 1

Struizendam_1_Predict <- rast("output_data/Struizendam_1_Predict_Map.tif")
Struizendam_1_Predict

## NB You cant use exact extract with SpatVector so importing it a a spatial DF
Struizendam_1_points<- read_sf(dsn = 'input_data/', layer = "Struizendam_1_Prosopis_height_points_20b")
Struizendam_1_points


#---2. Extract data for Struizendam_1 Grid

Struizendam_1_Drone_Extract <- exact_extract(Struizendam_1_Predict,Struizendam_1_points,"mode")
names(Struizendam_1_Drone_Extract) <- c('mode')
Struizendam_1_Drone_Extract_DF <-bind_cols(Struizendam_1_points,Struizendam_1_Drone_Extract)

Struizendam_1_Drone_Extract <- exact_extract(Struizendam_1_Predict,Struizendam_1_points,"majority" )
names(Struizendam_1_Drone_Extract) <- c('majority')
Struizendam_1_Drone_Extract_DF <- dplyr::mutate(Struizendam_1_Drone_Extract_DF, majority = Struizendam_1_Drone_Extract)

Struizendam_1_Drone_Extract <- exact_extract(Struizendam_1_Predict,Struizendam_1_points,"frac" )# calculates fraction cover for each classification value
#a column is added with the FVC for each vegetation type frac1, frac2 etc...
Struizendam_1_Drone_Extract_DF <- dplyr::mutate(Struizendam_1_Drone_Extract_DF, Struizendam_1_Drone_Extract)

df5 <- Struizendam_1_Drone_Extract_DF  %>% dplyr::select (fid,majority,frac_1, frac_2, frac_3,frac_5,frac_6)
df5[,"frac_4"]=NA

Prosopis_height_df <- rbind(Prosopis_df2,df5)
#Prosopis_height_df[, 'majority'] <- as.factor(Prosopis_height_df[, 'majority'])
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


P5 <- ggplot(Prosopis_height_df, aes(x=factor(majority))) +  geom_histogram(stat= "count")+ scale_color_viridis(discrete=TRUE) +
scale_x_discrete(labels=c("1" = "Prosopis", "2"="Bare Ground", "3" = "Grass", "4" = "Gnidia","5" = "Vachellia erioloba","6" = "Other woody"))+
  xlab("Predicted")+ ylab("Number of predictions")+ ggtitle( "")+ theme_fancy()
plot (P5)

p6 <- ggplot(data=Prosopis_height_df, aes(x=factor(majority))) +geom_bar()+
  scale_x_discrete(labels=c("1" = "Prosopis", "2"="Bare Ground", "3" = "Grass", "4" = "Gnidia","5" = "Vachellia erioloba","6" = "Other woody"))+
  xlab("Predicted")+ ylab("Number of predictions")+ ggtitle( "")+
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.5, colour = "black",size=2) +theme_fancy()


plot(p6)

ggsave(
  p6,
  # filename = "/plots/test.png",
  filename = "output_data/Prosopis_predicted.png",
  width = 12,
  height = 8,
  units = "cm"
)  
write_xlsx(Prosopis_height_df,"output_data/prosopis_height_data_predicted.xlsx")  




plot(p7)
