# Combined accuracy comparison owth drone figure
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
library(readxl)
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
        size = 7,
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


# Read in WV2 accuacy data compiled by seperate script
Full_WV2 <- read_xlsx ("E:/Glenn/Botswana/R_Scripts/Glenn-Prosopis-ML/data_out/Drone_extract_accuracy_assesment.xlsx")

Prospis_predict <- filter(Full_WV2,WV2 == "1")
Prospis_predict <- filter(Prospis_predict,majority <8)

p1 <- ggplot(aes(x=factor(majority)), data = Prospis_predict)+ geom_bar(stat = "count")+ 
  scale_x_discrete(labels=c("1" = "Prosopis", "2"="BG", "3" = "Grass", "4" = "F","5" = "VE","6" = "RT", "7" = "OW"))+
  xlab("Class")+ ylab("Drone Predictions")+ggtitle("WV2 Predicted Prosopis" )+theme_fancy()

p1     

Prospis_predict2 <- filter(Full_WV2,majority == "1")
Prospis_predict3 <- filter(Prospis_predict2,WV2 <8)

p2 <- ggplot(aes(x=factor(WV2)), data = Prospis_predict3)+ geom_bar(stat = "count")+ 
  scale_x_discrete(labels=c("1" = "Prosopis", "2"="BG", "3" = "Grass", "4" = "F","5" = "VE","6" = "RT","7" = "OW"))+
  xlab("Class")+ ylab("Wv2 Predictions")+ggtitle("Drone Predicted Prosopis\n for WV2 pixels" )+theme_fancy()

p2 

patchworkwv <- p1+p2

patchworkwvf<- patchworkwv + plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 10))

patchworkwvf

# Read in Planet accuracy data compiled by separate script

Full_Planet <- read_xlsx("E:/Glenn/Botswana/R_Scripts/Glenn-Prosopis-ML/data_out/Planet_Drone_extract_accuracy_assesment.xlsx")

Prospis_predict <- filter(Full_Planet,Planet == "1")
Prospis_predict <- filter(Prospis_predict,majority <8)

p3 <- ggplot(aes(x=factor(majority)), data = Prospis_predict)+ geom_bar(stat = "count")+ 
  scale_x_discrete(labels=c("1" = "Prosopis", "2"="BG", "3" = "Grass", "4" = "F","5" = "VE","6" = "RT", "7" = "OW"))+
  xlab("Class")+ ylab("Drone Predictions")+ggtitle("Planet Predicted Prosopis" )+theme_fancy()

p3     

Prospis_predict2 <- filter(Full_Planet,majority == "1")
Prospis_predict3 <- filter(Prospis_predict2,Planet <8)

p4 <- ggplot(aes(x=factor(Planet)), data = Prospis_predict3)+ geom_bar(stat = "count")+ 
  scale_x_discrete(labels=c("1" = "Prosopis", "2"="BG", "3" = "Grass", "4" = "F","5" = "VE","6" = "RT","7" = "OW"))+
  xlab("Class")+ ylab("Planet Predictions")+ggtitle("Drone Predicted Prosopis\n for Planet pixels" )+theme_fancy()

p4 

patchworkpl <- p3+p4

patchworkplf<- patchworkpl + plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 10))

patchworkplf

#REad in S2 accuacy data compiled by seperate script
Full_S2 <- read_xlsx("E:/Glenn/Botswana/R_Scripts/Glenn-Prosopis-ML/data_out/Sen2_Drone_extract_accuracy_assesment.xlsx")
# Now to plot
Prospis_predict <- filter(Full_S2,S2 == "1")
#Prospis_predict <- filter(Prospis_predict,majority <8)

{p5 <- ggplot(aes(x=factor(majority)), data = Prospis_predict)+ geom_bar(stat = "count")+ 
  scale_x_discrete(labels=c("1" = "Prosopis", "2"="BG", "3" = "Grass", "4" = "F","5" = "VE","6" = "RT", "7" = "OW"))+
  xlab("Class")+ ylab("Drone Predictions")+ggtitle("Sentinel-2 Predicted Prosopis" )+theme_fancy()

p5    

Prospis_predict2 <- filter(Full_S2,majority == "1")
#Prospis_predict3 <- filter(Prospis_predict2,S2 <8)

p6 <- ggplot(aes(x=factor(S2)), data = Prospis_predict2)+ geom_bar(stat = "count")+ 
  scale_x_discrete(labels=c("1" = "Prosopis", "2"="BG", "3" = "Grass", "4" = "F","5" = "VE","6" = "RT","7" = "OW"))+
  xlab("Class")+ ylab("Sentinel-2 predicitions")+ggtitle("Drone predicted Prosopis\n for Sentinel-2 pixels")+theme_fancy()

p6 

patchwork2 <- p5+p6

patchworkf<- patchwork2 + plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 10))

patchworkf
}


patchwork6 <- p1+p2+p3+p4+p5+p6+plot_layout(ncol = 2)
patchwork6

patchworkf<- patchwork6 + plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 10))

patchworkf

ggsave(
  patchworkf,
  # filename = "/plots/test.png",
  filename = "output_data/All data sets accuracy comparison with drone.png",
  width = 16,
  height = 24,
  units = "cm"
)
