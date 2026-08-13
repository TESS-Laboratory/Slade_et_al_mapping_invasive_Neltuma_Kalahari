# ===========================================================================
# IMPORTED - not part of the original published archive.
#
#   source repo   slade-prosopis
#   source path   Prosopis_height/Prosopis_height_analysis.R
#   ref           main @ 671e56f
#   sha256        aa1ba872327b365aa270362591a5208d848e9c35e454a8b14e4318c10134cfa3
#
#   why           Companion analysis for the height points.
#
# Content below is VERBATIM and does not run as-is: it depends on Windows
# absolute paths under E:/Glenn/Botswana/ and on archived packages (rgeos,
# rgdal), and calls windowsFonts(). It is kept as the reference
# implementation to port into the targets pipeline, not to execute.
# See refactor-findings.md and audit/source-recovery-map.md.
# ===========================================================================

# Load ggplot2
library(ggplot2)
# Libraries
library(tidyverse)
library(viridis)
## Plotting theme
theme_fancy <- function() {
  theme_bw() +
    theme(
      text = element_text(family = "Helvetica"),
      axis.text = element_text(size = 8, color = "black"),
      axis.title = element_text(size = 10, color = "black"),
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

#-------1. Read in data in CSV Files --------

setwd ("E:/Glenn/Botswana//Prosopis Height/")
PROSOPIS <- read.csv("E:/Glenn/Botswana//Prosopis Height/all_prosopis_measurements.csv")

#-------2. Preparing data --------

#dim (PROSOPIS)
#summary (PROSOPIS)
head (PROSOPIS)
#view (PROSOPIS)

#-----3.Plotting data---------


p <- ggplot(PROSOPIS, aes(x=Prosopis.Height)) +
  geom_histogram(aes(y=..count..),     
                 binwidth=25,
                 colour="black", fill="white") +
  geom_density(alpha=0.3, colour="darkblue", fill="lightblue") 

p + labs(x = "Prosopis height (cm)", y = "Number of trees", title = "Prosopis Height Distribution") + coord_fixed(ratio = 100) 

ggsave("D:/Botswana_Research/Prosopis Height/histogram.png",
       p,
       width = 16,
       height = 8,
       dpi = 1200)




p2<- ggplot(PROSOPIS, aes(x=Prosopis.Height)) + 
  geom_histogram(aes(y = ..density..),binwidth = 25,
                 colour = 1, fill = "white") +
  geom_density(alpha=0.3, colour="darkblue", fill="lightblue")
p2

ggsave("D:/Botswana_Research/Prosopis Height/density.png",
       p2,
       width = 16,
       height = 8,
       dpi = 1200)
