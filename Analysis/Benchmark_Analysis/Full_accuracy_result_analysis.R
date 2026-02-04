
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
BENCH <- read_xlsx("input_data/full/Resampling_results_accuracy_Planet_svm.xlsx")











#-------2. Preparing data --------

dim (BENCH)
summary (BENCH)
head (BENCH)
str(BENCH)
#as.factor (BENCH$Type)







#-----3.   Plotting Data -------
# learner
Learner_bp <- ggplot(data = BENCH, mapping = aes (x=task_id, y=(1-classif.ce), group = task_id))+ geom_jitter(size=0.25)+ 
  xlab("Sensor") + stat_boxplot(fill=c("mediumorchid2"),outlier.shape = NA)+theme_fancy()
plot(Learner_bp)









# input_data
input_data_bp <- ggplot(data = BENCH, mapping = aes (x=input_data, y=Accuracy, group = input_data))+ geom_jitter(size=0.25)+ 
  xlab("Input data set") + stat_boxplot(fill=c("mediumorchid2","orange","yellow","green4"),outlier.shape = NA)+theme_fancy()
plot(input_data_bp)

# Just look at input data for SVM

df_SVM<- filter(BENCH,learner == "SVM") 

input_data_svm <- ggplot(data = df_SVM, mapping = aes (x=input_data, y=Accuracy, group = input_data))+ geom_jitter(size=0.25)+ 
  xlab("Input data set") + stat_boxplot(fill=c("mediumorchid2","orange","yellow","green4"),outlier.shape = NA)+theme_fancy()
plot(input_data_svm)

ggplot2::ggsave(
  Learner_bp,
  # filename = "/plots/test.png",
  filename = paste0("output_data/learner_accuracy.jpg"),
  width = 8,
  height = 8,
  units = "cm"
)

ggplot2::ggsave(
  input_data_bp,
  # filename = "/plots/test.png",
  filename = paste0("output_data/input_data_accuracy.jpg"),
  width = 10,
  height = 10,
  units = "cm"
)

ggplot2::ggsave(
  input_data_svm,
  # filename = "/plots/test.png",
  filename = paste0("output_data/svm_input_data_accuracy.jpg"),
  width = 10,
  height = 10,
  units = "cm"
)

Learner_bp_grey <- ggplot(data = BENCH, mapping = aes (x=learner, y=Accuracy, group = learner))+ geom_jitter(size=0.25)+ 
  xlab("Learner") + stat_boxplot(fill=c("grey28","grey38","grey60","grey80", "grey90"),outlier.shape = NA)+theme_fancy()
plot(Learner_bp_grey)

# input_data
input_data_bp_grey <- ggplot(data = BENCH, mapping = aes (x=input_data, y=Accuracy, group = input_data))+ geom_jitter(size=0.25)+ 
  xlab("Input data set") + stat_boxplot(fill=c("grey28","grey38","grey60","grey80"),outlier.shape = NA)+theme_fancy()
plot(input_data_bp_grey)

# Just look at input data for SVM

df_SVM<- filter(BENCH,learner == "SVM") 

input_data_svm <- ggplot(data = df_SVM, mapping = aes (x=input_data, y=Accuracy, group = input_data))+ geom_jitter(size=0.25)+ 
  xlab("Input data set") + stat_boxplot(fill=c("grey28","grey38","grey60","grey80"),outlier.shape = NA)+theme_fancy()
plot(input_data_svm)

ggplot2::ggsave(
  Learner_bp_grey,
  # filename = "/plots/test.png",
  filename = paste0("output_data/learner_accuracy_grey.jpg"),
  width = 8,
  height = 8,
  units = "cm"
)

ggplot2::ggsave(
  input_data_bp_grey,
  # filename = "/plots/test.png",
  filename = paste0("output_data/input_data_accuracy_grey.jpg"),
  width = 10,
  height = 10,
  units = "cm"
)

ggplot2::ggsave(
  input_data_svm,
  # filename = "/plots/test.png",
  filename = paste0("output_data/svm_input_data_accuracy_grey.jpg"),
  width = 10,
  height = 10,
  units = "cm"
)
