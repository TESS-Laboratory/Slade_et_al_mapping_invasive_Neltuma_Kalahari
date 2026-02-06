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

#-------0. SET Parameters to use for training data set


cover <- read_xlsx("input_data/Drone_extract_prosopis_cover_S2Grid.xlsx")


p4 <- ggplot(aes(x = frac_1 ), data = cover) + 
  geom_histogram(aes(fill = frac_1 ), binwidth=0.05, colour="grey20", lwd=0.2) +
  stat_bin(binwidth=0.05, geom="text", colour="black", size=2,
           aes(label=..count..,vjust = -1))+theme_fancy() + xlab("Fractional cover of Prosopis")+ ylab("Number of S2 pixels")

p4

ggsave("output_data/Fraction cover Prosopis S2 pixels.png",
       p4,
       width = 16,
       height = 8,
       dpi = 1200)
