# ===========================================================================
# IMPORTED - not part of the original published archive.
#
#   source repo   slade-prosopis
#   source path   Reflectance_Boxplots.R
#   ref           main @ 671e56f
#   sha256        a6cf871ce3634131672fd08c03a84f96fdfc48a28be41d9a6b32f7895abaa413
#
#   why           Figure S9, per-band reflectance boxplots by vegetation class. Reads Vegpoly_DFB1.csv from a fifth project root (C:/Workspace/R_Scripts/Kgalagadi) but that input is regenerable via build_ml_df(df_type='point').
#
# Content below is VERBATIM and does not run as-is: it depends on Windows
# absolute paths under E:/Glenn/Botswana/ and on archived packages (rgeos,
# rgdal), and calls windowsFonts(). It is kept as the reference
# implementation to port into the targets pipeline, not to execute.
# See refactor-findings.md and audit/source-recovery-map.md.
# ===========================================================================


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

## Plotting theme
theme_fancy <- function() {
  theme_bw() +
    theme(
      text = element_text(family = "Helvetica"),
      axis.text = element_text(size = 7, color = "black"),
      axis.title = element_text(size = 8, color = "black"),
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

setwd ("C:/Workspace/R_Scripts/Kgalagadi")
REFL <- read.csv("C:/Workspace/R_Scripts/Kgalagadi/output_data/Vegpoly_DFB1.csv")

#-------2. Preparing data --------

dim (REFL)
summary (REFL)
head (REFL)
str(REFL)
#as.factor (REFL$Type)

#-----3.   Plotting Data -------
# NDVI
NDVIbp <- ggplot(data = REFL, mapping = aes (x=Type, y=NDVI, group = Type))+ geom_jitter(size=0.25)+ 
  xlab("Land Cover Class") + stat_boxplot(fill=c("mediumorchid2","orange","burlywood1","yellow","green4",
                                                 "brown"),outlier.shape = NA)
NDVIbp2 <- NDVIbp + scale_x_discrete(limits= c(1,2,3,4,5,6), labels = c("Prosopis", "Bare Sand", "Grass", "Gnidia",
                                                                        "Camel Thorn", "Rhig Trig"))
NDVI_BP <- NDVIbp2 + ylab("Calibrated Reflectance NDVI") + ggtitle("Mean NDVI Extracted from Drone Image Data") + theme(plot.title = element_text(hjust = 0.5)) + stat_boxplot(geom = "errorbar", width = 0.5)+ theme_fancy()
plot (NDVI_BP)

# Blue

Bluebp <- ggplot(data = REFL, mapping = aes (x=Type, y=Blue, group = Type))+ geom_jitter(size=0.25)+ 
  xlab("Land Cover Class") + stat_boxplot(fill=c("mediumorchid2","orange","burlywood1","yellow","green4",
                                                 "brown"),outlier.shape = NA)
Bluebp2 <- Bluebp + scale_x_discrete(limits= c(1,2,3,4,5,6), labels = c("Prosopis", "Bare Sand", "Grass", "Gnidia",
                                                                        "Camel Thorn", "Rhig Trig"))
Blue_BP <- Bluebp2 + ylab("Calibrated Reflectance Blue") + ggtitle("Mean Calibrated Reflectance Blue \n Band Extracted from Drone Image Data") + theme(plot.title = element_text(hjust = 0.5)) + stat_boxplot(geom = "errorbar", width = 0.5)+ theme_fancy()
plot (Blue_BP)

# Green

Greenbp <- ggplot(data = REFL, mapping = aes (x=Type, y=Green, group = Type))+ geom_jitter(size=0.25)+ 
  xlab("Land Cover Class") + stat_boxplot(fill=c("mediumorchid2","orange","burlywood1","yellow","green4",
                                                 "brown"),outlier.shape = NA)
Greenbp2 <- Greenbp + scale_x_discrete(limits= c(1,2,3,4,5,6), labels = c("Prosopis", "Bare Sand", "Grass", "Gnidia",
                                                                          "Camel Thorn", "Rhig Trig"))
Green_BP <- Greenbp2 + ylab("Calibrated Reflectance Green") + ggtitle("Mean Calibrated Reflectance Green \n Band Extracted from Drone Image Data") + theme(plot.title = element_text(hjust = 0.5)) + stat_boxplot(geom = "errorbar", width = 0.5)+ theme_fancy()
plot (Green_BP)

# Red


Redbp <- ggplot(data = REFL, mapping = aes (x=Type, y=Red, group = Type))+ geom_jitter(size=0.25)+ 
  xlab("Land Cover Class") + stat_boxplot(fill=c("mediumorchid2","orange","burlywood1","yellow","green4",
                                                 "brown"),outlier.shape = NA)
Redbp2 <- Redbp + scale_x_discrete(limits= c(1,2,3,4,5,6), labels = c("Prosopis", "Bare Sand", "Grass", "Gnidia",
                                                                      "Camel Thorn", "Rhig Trig"))
Red_BP <- Redbp2 + ylab("Calibrated Reflectance Red") + ggtitle("Mean Calibrated Reflectance Red \n Band Extracted from Drone Image Data") + theme(plot.title = element_text(hjust = 0.5)) + stat_boxplot(geom = "errorbar", width = 0.5)+ theme_fancy()
plot (Red_BP)



# Red Edge
RedEdgebp <- ggplot(data = REFL, mapping = aes (x=Type, y=RedEdge, group = Type))+ geom_jitter(size=0.25)+ 
  xlab("Land Cover Class") + stat_boxplot(fill=c("mediumorchid2","orange","burlywood1","yellow","green4",
                                                 "brown"),outlier.shape = NA)
RedEdgebp2 <- RedEdgebp + scale_x_discrete(limits= c(1,2,3,4,5,6), labels = c("Prosopis", "Bare Sand", "Grass", "Gnidia",
                                                                              "Camel Thorn", "Rhig Trig"))
RedEdge_BP <- RedEdgebp2 + ylab("Calibrated Reflectance RedEdge") + ggtitle("Mean Calibrated Reflectance RedEdge \n Band Extracted from Drone Image Data") + theme(plot.title = element_text(hjust = 0.5)) + stat_boxplot(geom = "errorbar", width = 0.5)+ theme_fancy()
plot (RedEdge_BP)


# NIR
NIRbp <- ggplot(data = REFL, mapping = aes (x=Type, y=NIR, group = Type))+ geom_jitter(size=0.25)+ 
  xlab("Land Cover Class") + stat_boxplot(fill=c("mediumorchid2","orange","burlywood1","yellow","green4",
                                                 "brown"),outlier.shape = NA)
NIRbp2 <- NIRbp + scale_x_discrete(limits= c(1,2,3,4,5,6), labels = c("Prosopis", "Bare Sand", "Grass", "Gnidia",
                                                                      "Camel Thorn", "Rhig Trig"))
NIR_BP <- NIRbp2 + ylab("Calibrated Reflectance NIR") + ggtitle("Mean Calibrated Reflectance NIR \n Band Extracted from Drone Image Data") + theme(plot.title = element_text(hjust = 0.5)) + stat_boxplot(geom = "errorbar", width = 0.5)+ theme_fancy()
plot (NIR_BP)

#----4. Saving plots and combining----

Plotcombined <-  plot_grid(Blue_BP,Green_BP,Red_BP, RedEdge_BP,NIR_BP,NDVI_BP, nrow = 3, rel_heights = c(0.33,0.33,0.33))

ggsave(
  Plotcombined,
  filename = "C:/Workspace/R_Scripts/Kgalagadi/output_data/plots/B1_Reflectance_plots.png",
  width =17,
  height = 22,
  units = "cm"
)

#<-grid.arrange(pMRE1v2blue, pMRE2v3blue, pMRE1v3blue, nrow = 3)
