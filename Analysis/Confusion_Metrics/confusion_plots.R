
# Libraries
library(viridis)
library(lubridate)
library(RColorBrewer)
library(ggplot2)
library(MASS)
library(splines)
library(gridExtra)
library(DescTools)
library(sf)
library(writexl)
library(cowplot)
library(tidyverse)
library(viridis)
library(readxl)
library(ggpubr)
library(patchwork)

## Plotting theme
theme_fancy <- function() {
  theme_bw() +
    theme(
      text = element_text(family = "Helvetica"),
      axis.text = element_text(size = 6, color = "black"),
      axis.title = element_text(size = 8, color = "black"),
      axis.line.x = element_line(size = 0.3, color = "black"),
      axis.line.y = element_line(size = 0.3, color = "black"),
      axis.ticks = element_line(size = 0.3, color = "black"),
      axis.text.x = element_text(angle = 0),
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
      legend.text = element_text(size = 7, color = "black"),
      legend.title = element_text(size = 7, color = "black"),
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

#-------1. Read in data in CSV Files --------

setwd ("C:/Workspace/R_Scripts/slade-prosopis")
CONFUSION <- read_xlsx("input_data/confusion_master_simplified_Prosopis.xlsx")

#-------2. Preparing data --------

dim (CONFUSION)
summary (CONFUSION)
head (CONFUSION)
str(CONFUSION)
#as.factor (CONFUSION$Type)

#-----3.   Plotting Data -------
#
# input_data
input_data_bp_prosopis <- ggplot(data = CONFUSION, mapping = aes (x=input_data, y=Accuracy, group = input_data))+ geom_jitter(size=0.25)+ 
  xlab("Input data") + stat_boxplot(fill=c("mediumorchid2","orange","yellow","green4"),outlier.shape = NA)+theme_fancy()
plot(input_data_bp_prosopis)


ggplot2::ggsave(
  input_data_bp_prosopis,
  # filename = "/plots/test.png",
  filename = paste0("output_data/input_data_accuracy_prosopis.jpg"),
  width = 10,
  height = 10,
  units = "cm"
)

# input_data
input_data_bp_prosopis2 <- ggplot(data = CONFUSION, mapping = aes (x=input_data, y=Accuracy, group = input_data))+ geom_jitter(size=0.25)+ 
  xlab("Input data") + stat_boxplot(fill=c("grey28","grey38","grey60","grey80"),outlier.shape = NA)+theme_fancy()
plot(input_data_bp_prosopis2)


ggplot2::ggsave(
  input_data_bp_prosopis2,
  # filename = "/plots/test.png",
  filename = paste0("output_data/input_data_accuracy_prosopis_grey.jpg"),
  width = 10,
  height = 10,
  units = "cm"
)
