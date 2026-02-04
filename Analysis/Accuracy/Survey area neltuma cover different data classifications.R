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
library(xlsx)
library(patchwork)
library(read_xl)

#-------0. Import data set

Data <- read_xl("output_data/Surveys all data prosopis_extract_assesment3.xlsx")


#
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


P5 <- ggplot(Data, aes(x=factor(majority))) +  geom_histogram(stat= "count")+ scale_color_viridis(discrete=TRUE) +
scale_x_discrete(labels=c("1" = "Neltuma", "2"="Bare Ground", "3" = "Grass", "4" = "Gnidia","5" = "Vachellia erioloba","6" = "Other woody"))+
  xlab("Predicted")+ ylab("Number of predictions")+ ggtitle( "")+ theme_fancy()
plot (P5)

p6 <- ggplot(data=Neltuma_height_df, aes(x=factor(majority))) +geom_bar()+
  scale_x_discrete(labels=c("1" = "Neltuma", "2"="Bare Ground", "3" = "Grass", "4" = "Gnidia","5" = "Vachellia erioloba","6" = "Other woody"))+
  xlab("Predicted")+ ylab("Number of predictions")+ ggtitle( "")+
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.5, colour = "black",size=2) +theme_fancy()


plot(p6)

ggsave(
  p6,
  # filename = "/plots/test.png",
  filename = "output_data/Neltuma_predicted.png",
  width = 12,
  height = 8,
  units = "cm"
)  
write_xlsx(Neltuma_height_df,"output_data/Neltuma_height_data_predicted.xlsx")  




plot(p7)
