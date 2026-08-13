# ===========================================================================
# IMPORTED - not part of the original published archive.
#
#   source repo   slade-prosopis
#   source path   Manuscript figures/All surveys prosopis extract.R
#   ref           main @ 671e56f
#   sha256        b197aa2d4f361316c9f0e5cee480e9777ed013885a3109cd999299e22c045cfa
#
#   why           Per-survey-area Neltuma cover across sensors; feeds the Fig 5 / Table S10 comparisons.
#
# Content below is VERBATIM and does not run as-is: it depends on Windows
# absolute paths under E:/Glenn/Botswana/ and on archived packages (rgeos,
# rgdal), and calls windowsFonts(). It is kept as the reference
# implementation to port into the targets pipeline, not to execute.
# See refactor-findings.md and audit/source-recovery-map.md.
# ===========================================================================


# Libraries
library(viridis)
library(tidyverse)
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
library (dplyr)
library(writexl)  
library(readxl)
library(data.table)
library(tm)
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
#library(wordcloud2)
library(quanteda)
library (quanteda.textplots)
library (bbplot)
library (ggplot2)
library(patchwork)

## Plotting theme
theme_fancy <- function() {
  theme_bw() +
    theme(
      text = element_text(family = "Helvetica"),
      axis.text = element_text(size = 9, color = "black"),
      axis.title = element_text(size = 9, color = "black"),
      axis.line.x = element_line(size = 0.3, color = "black"),
      axis.line.y = element_line(size = 0.3, color = "black"),
      axis.ticks = element_line(size = 0.3, color = "black"),
      axis.text.x = element_text(angle = 90),
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
      legend.text = element_text(size = 9, color = "black"),
      legend.title = element_text(size = 9, color = "black"),
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


# Reading in Xcel file with data 
Q <-read_xlsx("C:/Workspace/R_Scripts/slade-prosopis/output_data/Surveys all data prosopis_extract_assesment3.xlsx") 

# Plots
PB1 <- ggplot(Q, aes(x=factor(Sensor,level=(c("Drone","WV2","Planet","Sen2"))), y=B1)) +  geom_bar(stat = "identity", fill=c("#FFC20A", "#0C7bdc", "#1aff1a","#4b0092"))+
  xlab("Sensor")+ ylab("Prosopis FVC")+theme_fancy()
plot (PB1)


PB2 <- ggplot(Q, aes(x=factor(Sensor,level=(c("Drone","WV2","Planet","Sen2"))), y=B2)) +  geom_bar(stat = "identity", fill=c("#FFC20A", "#0C7bdc", "#1aff1a","#4b0092"))+
  xlab("Sensor")+ ylab("Prosopis FVC")+theme_fancy()
plot (PB2)

PB3 <- ggplot(Q, aes(x=factor(Sensor,level=(c("Drone","WV2","Planet","Sen2"))), y=B3)) +  geom_bar(stat = "identity", fill=c("#FFC20A", "#0C7bdc", "#1aff1a","#4b0092"))+
  xlab("Sensor")+ ylab("Prosopis FVC")+theme_fancy()
plot (PB3)

PS1 <- ggplot(Q, aes(x=factor(Sensor,level=(c("Drone","WV2","Planet","Sen2"))), y=S1)) +  geom_bar(stat = "identity", fill=c("#FFC20A", "#0C7bdc", "#1aff1a","#4b0092"))+
  xlab("Sensor")+ ylab("Prosopis FVC")+theme_fancy()
plot (PS1)

PS2 <- ggplot(Q, aes(x=factor(Sensor,level=(c("Drone","WV2","Planet","Sen2"))), y=S2)) +  geom_bar(stat = "identity", fill=c("#FFC20A", "#0C7bdc", "#1aff1a","#4b0092"))+
  xlab("Sensor")+ ylab("Prosopis FVC")+theme_fancy()
plot (PS2)

PS3 <- ggplot(Q, aes(x=factor(Sensor,level=(c("Drone","WV2","Planet","Sen2"))), y=S3)) +  geom_bar(stat = "identity", fill=c("#FFC20A", "#0C7bdc", "#1aff1a","#4b0092"))+
  xlab("Sensor")+ ylab("Prosopis FVC")+theme_fancy()
plot (PS3)

PS4 <- ggplot(Q, aes(x=factor(Sensor,level=(c("Drone","WV2","Planet","Sen2"))), y=S4)) +  geom_bar(stat = "identity", fill=c("#FFC20A", "#0C7bdc", "#1aff1a","#4b0092"))+
  xlab("Sensor")+ ylab("Prosopis FVC")+theme_fancy()
plot (PS4)

#Create patchwork Plot


patchwork2 <- wrap_plots (PB1,PB2,PB3,PS1,PS2,PS3,PS4)
P_All2<- patchwork2+ plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 10))

P_All2

ggplot2::ggsave(
  P_All2,
  # filename = "/plots/test.png",
  filename = paste0("output_data/Manuscript figures/All surveys prosopis extract.jpg"),
  width = 16,
  height = 16,
  units = "cm"
) 

# Plots
PB1 <- ggplot(Q, aes(x=factor(Sensor,level=(c("Drone","WV2","Planet","Sen2"))), y=B1)) +  geom_bar(stat = "identity", fill=c("grey28","grey38","grey60","grey80"))+
  xlab("Sensor")+ ylab("Prosopis FVC")+theme_fancy()
plot (PB1)


PB2 <- ggplot(Q, aes(x=factor(Sensor,level=(c("Drone","WV2","Planet","Sen2"))), y=B2)) +  geom_bar(stat = "identity", fill=c("grey28","grey38","grey60","grey80"))+
  xlab("Sensor")+ ylab("Prosopis FVC")+theme_fancy()
plot (PB2)

PB3 <- ggplot(Q, aes(x=factor(Sensor,level=(c("Drone","WV2","Planet","Sen2"))), y=B3)) +  geom_bar(stat = "identity", fill=c("grey28","grey38","grey60","grey80"))+
  xlab("Sensor")+ ylab("Prosopis FVC")+theme_fancy()
plot (PB3)

PS1 <- ggplot(Q, aes(x=factor(Sensor,level=(c("Drone","WV2","Planet","Sen2"))), y=S1)) +  geom_bar(stat = "identity", fill=c("grey28","grey38","grey60","grey80"))+
  xlab("Sensor")+ ylab("Prosopis FVC")+theme_fancy()
plot (PS1)

PS2 <- ggplot(Q, aes(x=factor(Sensor,level=(c("Drone","WV2","Planet","Sen2"))), y=S2)) +  geom_bar(stat = "identity", fill=c("grey28","grey38","grey60","grey80"))+
  xlab("Sensor")+ ylab("Prosopis FVC")+theme_fancy()
plot (PS2)

PS3 <- ggplot(Q, aes(x=factor(Sensor,level=(c("Drone","WV2","Planet","Sen2"))), y=S3)) +  geom_bar(stat = "identity", fill=c("grey28","grey38","grey60","grey80"))+
  xlab("Sensor")+ ylab("Prosopis FVC")+theme_fancy()
plot (PS3)

PS4 <- ggplot(Q, aes(x=factor(Sensor,level=(c("Drone","WV2","Planet","Sen2"))), y=S4)) +  geom_bar(stat = "identity", fill=c("grey28","grey38","grey60","grey80"))+
  xlab("Sensor")+ ylab("Prosopis FVC")+theme_fancy()
plot (PS4)

#Create patchwork Plot


patchwork2 <- wrap_plots (PB1,PB2,PB3,PS1,PS2,PS3,PS4)
P_All2<- patchwork2+ plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 10))

P_All2

ggplot2::ggsave(
  P_All2,
  # filename = "/plots/test.png",
  filename = paste0("output_data/Manuscript figures/All surveys prosopis extract Bandw.jpg"),
  width = 16,
  height = 16,
  units = "cm"
) 
