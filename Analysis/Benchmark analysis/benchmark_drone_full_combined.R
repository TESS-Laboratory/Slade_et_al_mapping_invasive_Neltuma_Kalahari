
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
BENCH <- read_xlsx("input_data/bench_master_simplified_drone.xlsx")

#-------2. Preparing data --------

dim (BENCH)
summary (BENCH)
head (BENCH)
str(BENCH)
#as.factor (BENCH$Type)

#-----3.   Plotting Data -------
# learner
# Learner_bp <- ggplot(data = BENCH, mapping = aes (x=learner, y=Accuracy, group = learner))+ geom_jitter(size=0.25)+ 
#   xlab("Learner") + stat_boxplot(fill=c("mediumorchid2","orange","burlywood1","yellow","green4"),outlier.shape = NA)+theme_fancy()
# plot(Learner_bp)
# 
# # input_data
# input_data_bp <- ggplot(data = BENCH, mapping = aes (x=input_data, y=Accuracy, group = input_data))+ geom_jitter(size=0.25)+ 
#   xlab("Input data set") + stat_boxplot(fill=c("mediumorchid2","orange","yellow","green4"),outlier.shape = NA)+theme_fancy()
# plot(input_data_bp)
# 
# # Just look at input data for SVM
# 
# df_SVM<- filter(BENCH,learner == "SVM") 
# 
# input_data_svm <- ggplot(data = df_SVM, mapping = aes (x=input_data, y=Accuracy, group = input_data))+ geom_jitter(size=0.25)+ 
#   xlab("Input data set") + stat_boxplot(fill=c("mediumorchid2","orange","yellow","green4"),outlier.shape = NA)+theme_fancy()
# plot(input_data_svm)

# ggplot2::ggsave(
#   Learner_bp,
#   # filename = "/plots/test.png",
#   filename = paste0("output_data/learner_accuracy.jpg"),
#   width = 8,
#   height = 8,
#   units = "cm"
# )
# 
# ggplot2::ggsave(
#   input_data_bp,
#   # filename = "/plots/test.png",
#   filename = paste0("output_data/input_data_accuracy.jpg"),
#   width = 10,
#   height = 10,
#   units = "cm"
# )
# 
# ggplot2::ggsave(
#   input_data_svm,
#   # filename = "/plots/test.png",
#   filename = paste0("output_data/svm_input_data_accuracy.jpg"),
#   width = 10,
#   height = 10,
#   units = "cm"
# )

ilabels <- c("G1", "G2", "G3", "G4")
learner_labels <- c("Ens", "RF", "RF NT", "SVM", "XGB")

Learner_bp_grey <- ggplot(data = BENCH, mapping = aes (x=learner, y=Accuracy, group = learner))+ #geom_jitter(size=0.25)+ 
  xlab("Learner") + stat_boxplot(fill=c("grey28","grey38","grey60","grey80", "grey90"),outlier.shape = NA)+theme_fancy()+
  ylim(0.65,1)+ scale_x_discrete(label = learner_labels) + labs(x = "Learner", y = "Overall Accuracy") 
p1 <-plot(Learner_bp_grey)

# input_data
input_data_bp_grey <- ggplot(data = BENCH, mapping = aes (x=input_data, y=Accuracy, group = input_data))+ #geom_jitter(size=0.25)+ 
  xlab("Input data set") + stat_boxplot(fill=c("grey28","grey38","grey60","grey80"),outlier.shape = NA)+theme_fancy()+
  ylim(0.65,1)+ scale_x_discrete(label = ilabels) + labs(x = "Input data set", y = "Overall Accuracy") 
p2 <- plot(input_data_bp_grey)

# Just look at input data for SVM

# df_SVM<- filter(BENCH,learner == "SVM") 
# 
# input_data_svm <- ggplot(data = df_SVM, mapping = aes (x=input_data, y=Accuracy, group = input_data))+ geom_jitter(size=0.25)+ 
#   xlab("Input data set") + stat_boxplot(fill=c("grey28","grey38","grey60","grey80"),outlier.shape = NA)+theme_fancy()
# plot(input_data_svm)

# ggplot2::ggsave(
#   Learner_bp_grey,
#   # filename = "/plots/test.png",
#   filename = paste0("output_data/learner_accuracy_grey.jpg"),
#   width = 8,
#   height = 8,
#   units = "cm"
# )
# 
# ggplot2::ggsave(
#   input_data_bp_grey,
#   # filename = "/plots/test.png",
#   filename = paste0("output_data/input_data_accuracy_grey.jpg"),
#   width = 10,
#   height = 10,
#   units = "cm"
# )
# 
# ggplot2::ggsave(
#   input_data_svm,
#   # filename = "/plots/test.png",
#   filename = paste0("output_data/svm_input_data_accuracy_grey.jpg"),
#   width = 10,
#   height = 10,
#   units = "cm"
# )
# 
#-------4. Read in data in XLSX Files -------- 4

 CONFUSION <- read_xlsx("input_data/confusion_master_simplified_Prosopis.xlsx")

#-------5. Preparing data --------

dim (CONFUSION)
summary (CONFUSION)
head (CONFUSION)
str(CONFUSION)
#as.factor (CONFUSION$Type)

#-----3.   Plotting Data -------
#
# input_data
# input_data_bp_prosopis <- ggplot(data = CONFUSION, mapping = aes (x=input_data, y=Accuracy, group = input_data))+ geom_jitter(size=0.25)+ 
#   xlab("Input data") + stat_boxplot(fill=c("mediumorchid2","orange","yellow","green4"),outlier.shape = NA)+theme_fancy()
# plot(input_data_bp_prosopis)


# ggplot2::ggsave(
#   input_data_bp_prosopis,
#   # filename = "/plots/test.png",
#   filename = paste0("output_data/input_data_accuracy_prosopis.jpg"),
#   width = 10,
#   height = 10,
#   units = "cm"
# )

# input_data

# Custom X-axis labels 


input_data_bp_prosopis2 <- ggplot(data = CONFUSION, mapping = aes (x=input_data, y=Accuracy, group = input_data))+ #geom_jitter(size=0.25)+ 
  xlab("Input data") + stat_boxplot(fill=c("grey28","grey38","grey60","grey80"),outlier.shape = NA)+theme_fancy()+
  ylim(0.65,1)+ scale_x_discrete(label = ilabels) + labs(x = "Input data set", y = "Prosopis Overall Accuracy") 

p3 <-plot(input_data_bp_prosopis2)



patchwork2 <- p1+p2+p3

P_All3<- patchwork2 + plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 10))


P_All3


ggplot2::ggsave(
  P_All3,
  # filename = "/plots/test.png",
  filename = paste0("output_data/Manuscript figures/Combined_drone_accuracy_figure_v2.jpg"),
  width = 16,
  height = 8,
  units = "cm"
) 

