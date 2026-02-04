### Script to Extract Data from WV2 Classifications based on WV2 pixel grid
### matching pixels
### Imports classified drone image data set and calculates fractional cover for each 
### class in each WV2 pixel and then samples that data to produce training data set
### for classification of WV2 image.

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
library(openxlsx)
library(reshape2)
library(sfheaders)
library(cowplot)

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


#-------0. SET Parameters to use for training data set

#Fraction  cover to use to define training class 1.0 = 100%
#Change this to vary the pixel definitions
A = "0.85"

#Fraction  cover to use to define training class for Neltuma 1.0 = 100%
#Change this to vary the pixel definitions
P = "0.85"

#Number of pixels from each Class to use in training data

N = "500"


#----1. Read in files for WV2 etc Classified image data ----


WV2_Predict <- rast("E:/Glenn/Botswana/Satellite_Data/WV2/1_6_m_mosaic/RF_WV2_additional.tif")
WV2_Predict

## NB You cant use exact extract with SpatVector so importing it a a spatial DF
roads_buffer_SF <- read_sf(dsn = "E:/Glenn/Botswana/Final_Drone_Survey_Data/Buffers", layer = "1km_concentric_buffers_road_clipped_border")
roads_buffer_SF

settlment_buffer_SF <- read_sf(dsn = "E:/Glenn/Botswana/Final_Drone_Survey_Data/Buffers", layer = "1km_concentric_buffers_clipped_border")
settlment_buffer_SF

#---2. Extract data for Settlements buffers


WV2_WV2_Extract <- exact_extract(WV2_Predict,settlment_buffer_SF,"mode")
names(WV2_WV2_Extract) <- c('mode')
WV2_WV2_Extract_DF <-bind_cols(settlment_buffer_SF,WV2_WV2_Extract)

WV2_WV2_Extract <- exact_extract(WV2_Predict,settlment_buffer_SF,"frac" )# calculates fraction cover for each classification value
#a column is added with the FVC for each vegetation type frac1, frac2 etc...
WV2_WV2_Extract_DF <- dplyr::mutate(WV2_WV2_Extract_DF, WV2_WV2_Extract)

WV2_WV2_Extract <- exact_extract(WV2_Predict,settlment_buffer_SF,"count" )# calculates number of pixels in buffer
WV2_WV2_Extract_DF <- dplyr::mutate(WV2_WV2_Extract_DF, WV2_WV2_Extract)



write.csv2 (WV2_WV2_Extract_DF, "E:/Glenn/Botswana/Final_Drone_Survey_Data/Buffers/settlement_buffers_extract_border.csv")
write_xlsx(WV2_WV2_Extract_DF, "E:/Glenn/Botswana/Final_Drone_Survey_Data/Buffers/settlement_buffers_extract_border.xlsx")
##---3. Roads buffers

WV2_WV2_Extract <- exact_extract(WV2_Predict,roads_buffer_SF,"mode")
names(WV2_WV2_Extract) <- c('mode')
WV2_WV2_Extract_DF2 <-bind_cols(roads_buffer_SF,WV2_WV2_Extract)

WV2_WV2_Extract <- exact_extract(WV2_Predict,roads_buffer_SF,"frac" )# calculates fraction cover for each classification value
#a column is added with the FVC for each vegetation type frac1, frac2 etc...
WV2_WV2_Extract_DF2 <- dplyr::mutate(WV2_WV2_Extract_DF2, WV2_WV2_Extract)

write.csv2(WV2_WV2_Extract_DF2, "E:/Glenn/Botswana/Final_Drone_Survey_Data/Buffers/roads_buffers_extract_border.csv")
write_xlsx(WV2_WV2_Extract_DF2, "E:/Glenn/Botswana/Final_Drone_Survey_Data/Buffers/roads_buffers_extract_border.xlsx")

#-----4. Plot results -------

#P1 <- ggplot(WV2_WV2_Extract_DF, aes(y=frac_1, x=mrb_dist)) +  geom_histogram(stat= "identity")+
#    xlab("Distance from settlement")+ ylab("Fractional Cover")+ ggtitle( "Fractional cover of Neltuma with distance from settlements ")
#plot (P1)

Xlabs <- c("1", "2", "3", "4","5", "6", "7","8","9","10","11","12", "13","14","15")

PP <- ggplot(WV2_WV2_Extract_DF, aes(y=frac_1, x=factor(mrb_dist, level=c('1000', '2000', '3000', '4000','5000','6000','7000','8000','9000','10000','11000','12000','13000','14000','15000')))) +  geom_col(fill = "green")+
  xlab("Distance from settlement (Km)")+ ylab("Fractional Cover")+ ggtitle( "Fractional cover of Neltuma")+theme_fancy()+
  scale_x_discrete(labels= Xlabs)
plot (PP)

PS<- ggplot(WV2_WV2_Extract_DF, aes(y=frac_2, x=factor(mrb_dist, level=c('1000', '2000', '3000', '4000','5000','6000','7000','8000','9000','10000','11000','12000','13000','14000','15000')))) +  geom_col(fill = "yellow")+
  xlab("Distance from settlement (Km)")+ ylab("Fractional Cover")+ ggtitle( "Fractional cover of Bare Sand")+theme_fancy()+
  scale_x_discrete(labels= Xlabs)
plot (PS)

PG<- ggplot(WV2_WV2_Extract_DF, aes(y=frac_3, x=factor(mrb_dist, level=c('1000', '2000', '3000', '4000','5000','6000','7000','8000','9000','10000','11000','12000','13000','14000','15000')))) +  geom_col(fill = "grey")+
  xlab("Distance from settlement (Km)")+ ylab("Fractional Cover")+ ggtitle( "Fractional cover of Grass")+theme_fancy()+
  scale_x_discrete(labels= Xlabs)
plot (PG)

PC<- ggplot(WV2_WV2_Extract_DF, aes(y=frac_5, x=factor(mrb_dist, level=c('1000', '2000', '3000', '4000','5000','6000','7000','8000','9000','10000','11000','12000','13000','14000','15000')))) +  geom_col(fill = "orange")+
  xlab("Distance from settlement (Km)")+ ylab("Fractional Cover")+ ggtitle( "Fractional cover of Vachellia erioloba ")+theme_fancy()+
  scale_x_discrete(labels= Xlabs)
plot (PC)

PR<- ggplot(WV2_WV2_Extract_DF, aes(y=frac_6, x=factor(mrb_dist, level=c('1000', '2000', '3000', '4000','5000','6000','7000','8000','9000','10000','11000','12000','13000','14000','15000')))) +  geom_col(fill = "blue")+
  xlab("Distance from settlement (Km)")+ ylab("Fractional Cover")+ ggtitle( "Fractional cover of Rhigozum Trichotomum ")+theme_fancy()+
  scale_x_discrete(labels= Xlabs)
plot (PR)

PM<- ggplot(WV2_WV2_Extract_DF, aes(y=frac_7, x=factor(mrb_dist, level=c('1000', '2000', '3000', '4000','5000','6000','7000','8000','9000','10000','11000','12000','13000','14000','15000')))) +  geom_col(fill = "purple")+
  xlab("Distance from settlement (Km)")+ ylab("Fractional Cover")+ ggtitle( "Fractional cover of Senegalia mellifera")+theme_fancy()+
  scale_x_discrete(labels= Xlabs)
plot (PM)

PB<- ggplot(WV2_WV2_Extract_DF, aes(y=frac_10, x=factor(mrb_dist, level=c('1000', '2000', '3000', '4000','5000','6000','7000','8000','9000','10000','11000','12000','13000','14000','15000')))) +  geom_col(fill = "black")+
  xlab("Distance from settlement (Km)")+ ylab("Fractional Cover")+ ggtitle( "Fractional cover of Boscia albitrunca ")+theme_fancy()+
  scale_x_discrete(labels= Xlabs)
plot (PB)

st_write(WV2_WV2_Extract_DF,"E:/Glenn/Botswana/Final_Drone_Survey_Data/Buffers/settlements_extract_border.shp")

# Making all the data plot on one graph
SetDF <- read.xlsx( "E:/Glenn/Botswana/Final_Drone_Survey_Data/Buffers/settlement_buffers_extract_border.xlsx")

df3 <- subset(SetDF, select = -c(2,3,4,11,12,13) )
df3 <- df3%>% rename(Neltuma = frac_1, Bare_Sand = frac_2, Rhigosum_t = frac_6, Grass = frac_3, Vachellia_e = frac_5, Senegalia_m = frac_7)#, Boscia_a = frac_10 )



df4 <- melt(df3 ,  id.vars = 'mrb_dist', variable.name = 'Type')

df4$mrb_dist <-as.numeric(df4$mrb_dist)
PA <- ggplot(df4, aes(x = mrb_dist, y = value, color = Type)) +
      geom_line()+  xlab("Distance from settlement (m)")+ ylab("Fractional Cover")+ scale_color_manual(values=c("green", "yellow","grey","orange","blue", "purple"))+
      ggtitle( "Fractional cover of main land cover types with distance from settlements ")+
      theme_fancy()+ theme(legend.position = c(0.9, 0.5))
plot(PA)

title <- ggdraw() + draw_label("Changes in Vegetation Cover with distance from settlements", fontface='bold')
top_row <- plot_grid(PA, ncol = 1)
row_2 <- plot_grid(PP, PS, ncol = 2)
row_3 <- plot_grid(PG,PC, ncol = 2)
row_4 <- plot_grid(PR,PM, ncol = 2)
p<- plot_grid(title, top_row, row_2, row_3,row_4, nrow = 5,rel_heights = c(0.1, 1.5, 1,1,1))
p

ggsave2(
  "E:/Glenn/Botswana/R_Scripts/slade-Neltuma/output_data/Plots/Settlement_buffer_border.png",
  plot = p,
  device = NULL,
  path = NULL,
  scale = 1,
  width = 160,
  height = 260,
  units =  "mm",
  dpi = 300,
  limitsize = TRUE
)


#-----6. Plotting data for distance from roads
Xlabs <- c("1", "2", "3", "4","5", "6", "7","8","9","10","11","12", "13","14","15")

PP <- ggplot(WV2_WV2_Extract_DF2, aes(y=frac_1, x=factor(mrb_dist, level=c('1000', '2000', '3000', '4000','5000','6000','7000','8000','9000','10000','11000','12000','13000','14000','15000')))) +  geom_col(fill = "green")+
  xlab("Distance from main roads (Km)")+ ylab("Fractional Cover")+ ggtitle( "Fractional cover of Neltuma")+theme_fancy()+
  scale_x_discrete(labels= Xlabs)
plot (PP)

PS<- ggplot(WV2_WV2_Extract_DF2, aes(y=frac_2, x=factor(mrb_dist, level=c('1000', '2000', '3000', '4000','5000','6000','7000','8000','9000','10000','11000','12000','13000','14000','15000')))) +  geom_col(fill = "yellow")+
  xlab("Distance from main roads (Km)")+ ylab("Fractional Cover")+ ggtitle( "Fractional cover of Bare Sand")+theme_fancy()+
  scale_x_discrete(labels= Xlabs)
plot (PS)

PG<- ggplot(WV2_WV2_Extract_DF2, aes(y=frac_3, x=factor(mrb_dist, level=c('1000', '2000', '3000', '4000','5000','6000','7000','8000','9000','10000','11000','12000','13000','14000','15000')))) +  geom_col(fill = "grey")+
  xlab("Distance from main roads (Km)")+ ylab("Fractional Cover")+ ggtitle( "Fractional cover of Grass")+theme_fancy()+
  scale_x_discrete(labels= Xlabs)
plot (PG)

PC<- ggplot(WV2_WV2_Extract_DF2, aes(y=frac_5, x=factor(mrb_dist, level=c('1000', '2000', '3000', '4000','5000','6000','7000','8000','9000','10000','11000','12000','13000','14000','15000')))) +  geom_col(fill = "orange")+
  xlab("Distance from main roads (Km)")+ ylab("Fractional Cover")+ ggtitle( "Fractional cover of Vachellia erioloba ")+theme_fancy()+
  scale_x_discrete(labels= Xlabs)
plot (PC)

PR<- ggplot(WV2_WV2_Extract_DF2, aes(y=frac_6, x=factor(mrb_dist, level=c('1000', '2000', '3000', '4000','5000','6000','7000','8000','9000','10000','11000','12000','13000','14000','15000')))) +  geom_col(fill = "blue")+
  xlab("Distance from main roads (Km)")+ ylab("Fractional Cover")+ ggtitle( "Fractional cover of Rhigozum Trichotomum ")+theme_fancy()+
  scale_x_discrete(labels= Xlabs)
plot (PR)

PM<- ggplot(WV2_WV2_Extract_DF2, aes(y=frac_7, x=factor(mrb_dist, level=c('1000', '2000', '3000', '4000','5000','6000','7000','8000','9000','10000','11000','12000','13000','14000','15000')))) +  geom_col(fill = "purple")+
  xlab("Distance from main roads (Km)")+ ylab("Fractional Cover")+ ggtitle( "Fractional cover of Senegalia mellifera")+theme_fancy()+
  scale_x_discrete(labels= Xlabs)
plot (PM)

PB<- ggplot(WV2_WV2_Extract_DF2, aes(y=frac_10, x=factor(mrb_dist, level=c('1000', '2000', '3000', '4000','5000','6000','7000','8000','9000','10000','11000','12000','13000','14000','15000')))) +  geom_col(fill = "black")+
  xlab("Distance from main roads (Km)")+ ylab("Fractional Cover")+ ggtitle( "Fractional cover of Boscia albitrunca ")+theme_fancy()+
  scale_x_discrete(labels= Xlabs)
plot (PB)



st_write(WV2_WV2_Extract_DF2,"E:/Glenn/Botswana/Final_Drone_Survey_Data/Buffers/roads_buffers_extract_border.shp")
# Making all the data plot on one graph
SetDF <- read.xlsx( "E:/Glenn/Botswana/Final_Drone_Survey_Data/Buffers/roads_buffers_extract_border.xlsx")

df3 <- subset(SetDF, select = -c(2,3,4,11,12) )
df3 <- df3%>% rename(Neltuma = frac_1, Bare_Sand = frac_2, Rhigosum_t = frac_6, Grass = frac_3, Vachellia_e = frac_5, Senegalia_m = frac_7)#, Boscia_a = frac_10 )



df4 <- melt(df3 ,  id.vars = 'mrb_dist', variable.name = 'Type')

df4$mrb_dist <-as.numeric(df4$mrb_dist)
PA <- ggplot(df4, aes(x = mrb_dist, y = value, color = Type)) +
  geom_line()+  xlab("Distance from main roads (m)")+ ylab("Fractional Cover")+ scale_color_manual(values=c("green", "yellow","grey","orange","blue", "purple"))+
  ggtitle( "Fractional cover of main land cover types with distance from main roads ")+
  theme_fancy()+ theme(legend.position = c(0.9, 0.5))
plot(PA)

title <- ggdraw() + draw_label("Changes in Vegetation Cover with distance from main roads", fontface='bold')
top_row <- plot_grid(PA, ncol = 1)
row_2 <- plot_grid(PP, PS, ncol = 2)
row_3 <- plot_grid(PG,PC, ncol = 2)
row_4 <- plot_grid(PR,PM, ncol = 2)
p<- plot_grid(title, top_row, row_2, row_3,row_4, nrow = 5,rel_heights = c(0.1, 1.5, 1,1,1))
p

ggsave2(
  "E:/Glenn/Botswana/R_Scripts/slade-Neltuma/output_data/Plots/main_roads_buffer_border.png",
  plot = p,
  device = NULL,
  path = NULL,
  scale = 1,
  width = 160,
  height = 260,
  units =  "mm",
  dpi = 300,
  limitsize = TRUE
)
