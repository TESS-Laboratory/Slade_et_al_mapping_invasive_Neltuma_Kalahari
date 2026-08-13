# ===========================================================================
# IMPORTED - not part of the original published archive.
#
#   source repo   slade-prosopis
#   source path   Profile_Plots_4.R
#   ref           main @ 671e56f
#   sha256        a78e93a9d430a092803cd9df2b7ec011d318abeeb2bc07c0c465e8cf1e037221
#
#   why           Latest of four Profile_Plots variants.
#
# Content below is VERBATIM and does not run as-is: it depends on Windows
# absolute paths under E:/Glenn/Botswana/ and on archived packages (rgeos,
# rgdal), and calls windowsFonts(). It is kept as the reference
# implementation to port into the targets pipeline, not to execute.
# See refactor-findings.md and audit/source-recovery-map.md.
# ===========================================================================

#
#-----0. Library-----
{
#  library(terra)
  library(tidyverse)
  library(viridis)
  library(rgdal)
  library(lubridate)
  library(RColorBrewer)
  library(ggplot2)
  library(raster)
  library(MASS)
  library(splines)
  library(rgeos)
  library(gridExtra)
  library(DescTools)
  library(sf)
 # library(exactextractr)
  library(writexl)  
  library(reshape2)
  library(cowplot)
}

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

## Read in data 



DRONE <- read.table("E:/Glenn/Botswana/Analysis/Profile_lines/Profile_4_Drone.txt",sep='\t', header=TRUE)
PLANET <- read.table("E:/Glenn/Botswana/Analysis/Profile_lines//Profile_4_Planet.txt",sep='\t',header=TRUE)


#Plot of NDVI across feature

#ggplot 2 Version

#Need to get the data in the right format


df1 <- data.frame(DRONE)
df <- melt(df1,id.vars = "distance")
df3 <- data.frame(PLANET)
df4 <- melt(df3,id.vars = "distance")
df5 <- rbind(df4,df)

df6 <- subset(df5, (variable %in% c("NDVI_DRONE","NDVI_PLANET")))

P2 <-  ggplot(df6, aes(x = distance, y = value, color = variable)) + geom_line()+
  ggtitle("Comparison of Drone NDVI with Planet NDVI Plotted \n across Profile 4 (Prosopis and Camel Thorn)")+
  xlab('Distance')+
  ylab('NDVI')+
  scale_y_continuous(limits = c(0, 0.7))+
  theme_fancy() 
  
  plot(P2)

df7 <- subset(df5, (variable %in% c("MSAVI_DRONE","MSAVI_PLANET")))
  

P4 <- ggplot(df7, aes(x = distance, y = value, color = variable)) + geom_line()+
  ggtitle("Comparison of Drone MSAVI2 with Planet MSAVI2 Plotted\n across Profile 4 (Prosopis and Camel Thorn)")+
  xlab('Distance')+
  ylab('MSAVI2')+
  scale_y_continuous(limits = c(0, 0.7))+
  theme_fancy() 

plot(P4)

ggsave2(
  P2,
  filename = "E:/Glenn/Botswana/R_Scripts/slade-prosopis/output_data/plots/Profile_4_Drone_vs_Planet_NDVI.jpg",
  width =16,
  height = 8,
  units = "cm"
)


ggsave2(
  P4,
  filename = "E:/Glenn/Botswana/R_Scripts/slade-prosopis/output_data/plots/Profile_4_Drone_vs_Planet_MSAVI.jpg",
  width =16,
  height = 8,
  units = "cm"
)
