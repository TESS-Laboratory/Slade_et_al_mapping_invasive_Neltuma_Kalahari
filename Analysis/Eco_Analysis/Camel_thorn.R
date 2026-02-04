# Analysis of Camle Thorn distribution and the impact of Prosopis

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
library(cowplot)
library (bbplot)
library(readxl)
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

# read in shp files with CT location

CTB1_sf <- read_sf(dsn = "E:/Glenn/Botswana/Camel_Thorn/", layer = "Bokspits_1_CT_points")
CTB2_sf <- read_sf(dsn = "E:/Glenn/Botswana/Camel_Thorn/", layer = "Bokspits_2_CT_points")
CTB3_sf <- read_sf(dsn = "E:/Glenn/Botswana/Camel_Thorn/", layer = "Bokspits_3_CT_points")
CTS1_sf <- read_sf(dsn = "E:/Glenn/Botswana/Camel_Thorn/", layer = "Struizendam_1_CT_points")
CTS2_sf <- read_sf(dsn = "E:/Glenn/Botswana/Camel_Thorn/", layer = "Struizendam_2_CT_points")
CTS4_sf <- read_sf(dsn = "E:/Glenn/Botswana/Camel_Thorn/", layer = "Struizendam_4_CT_points")

CT_DF <- bind_rows(CTB1_sf,CTB2_sf,CTB3_sf,CTS1_sf,CTS2_sf,CTS4_sf)

CT_DF$Prosopis 

CT_DF$Prosopis <- factor(CT_DF$Prosopis, levels = c("0", "1"))


write_xlsx(CT_DF, temp.xlsx)

CT_DF <- read_xlsx ("C:/Workspace/R_Scripts/slade-prosopis/output_data/temp.xlsx")

P1 <- ggplot(CT_DF, aes(x=Prosopis, y=Number)) +  geom_bar(stat= "identity")+
  ylab("Number of Trees")+xlab("")+
  theme_fancy() + theme(legend.position = "none") + geom_text(x=2, y=30, label="27%")+ geom_text(x=1, y=70, label="63%")
plot (P1)

ggsave2(
  "C:/Workspace/R_Scripts/slade-prosopis/output_data/Plots/Camel_thorn_V3.png",
  plot = P1,
  device = NULL,
  path = NULL,
  scale = 1,
  width = 100,
  height = 80,
  units =  "mm",
  dpi = 300,
  limitsize = TRUE
)







P1 <- ggplot(CT_DF, aes(x=Prosopis, fill=Prosopis)) +  geom_histogram(stat= "count")+scale_x_discrete(labels=c("1" = "Prosopis growing under Camel Thorn", "0"="No Prosopis"))+
     ylab("Number of Trees")+ ggtitle( "Proportion of Camel Thorn Trees in drone survey areas \n with Neltuma growing under the canopy")+
    theme_fancy() + theme(legend.position = "none") + geom_text(x=1, y=30, label="27%")+ geom_text(x=2, y=70, label="63%")
plot (P1)

ggsave2(
  "E:/Glenn/Botswana/R_Scripts/slade-prosopis/output_data/Plots/Camel_thorn_v2.png",
  plot = P1,
  device = NULL,
  path = NULL,
  scale = 1,
  width = 100,
  height = 80,
  units =  "mm",
  dpi = 300,
  limitsize = TRUE
)

P1 <- ggplot(CT_DF, aes(x=Prosopis, fill=Prosopis)) +  geom_histogram(stat= "count")+scale_x_discrete(labels=c("1" = "Neltuma Proximal (1m)", "0"="No Neltuma"))+
  ylab("Number of Trees")+ 
  theme_fancy() + theme(legend.position = "none") + geom_text(x=1, y=30, label="27%")+ geom_text(x=2, y=70, label="63%")
plot (P1)

ggsave2(
  "E:/Glenn/Botswana/R_Scripts/slade-prosopis/output_data/Plots/Camel_thorn_V3.png",
  plot = P1,
  device = NULL,
  path = NULL,
  scale = 1,
  width = 100,
  height = 80,
  units =  "mm",
  dpi = 300,
  limitsize = TRUE
)



