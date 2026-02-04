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
library (cowplot)
library(ggpubr)
library(patchwork)


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
p2 <- ggplot(aes(x = frac_1 ), data = cover) + 
  geom_histogram(aes(fill = frac_1 ), binwidth=0.1, colour="grey20", lwd=0.2) +
  stat_bin(binwidth=0.1, geom="text", colour="black", size=2,
           aes(label=..count..,vjust = -1))+theme_fancy() + xlab("Sub-pixel proportion of Neltuma")+ ylab("Number of S2 pixels")+ ggtitle("10m resolution")

p2

ggsave(
         p2,
         # filename = "/plots/test.png",
         filename = "output_data/Fraction cover Prosopis S2 pixels10.png",
         width = 10,
         height = 8,
         units = "cm"
       )  



coverLST <- read_xlsx("input_data/Drone_extract_prosopis_cover_LSTGrid.xlsx")
p4 <- ggplot(aes(x = frac_1 ), data = coverLST) + 
  geom_histogram(aes(fill = frac_1 ), binwidth=0.1, colour="grey20", lwd=0.2) +
  stat_bin(binwidth=0.1, geom="text", colour="black", size=2,
           aes(label=..count..,vjust = -1))+theme_fancy() + xlab("Sub-pixel proportion of Neltuma")+ ylab("Number of LST pixels")+xlim(-0.1,1)+ggtitle("30m resolution")

p4

ggsave(
  p4,
  # filename = "/plots/test.png",
  filename = "output_data/Fraction cover Prosopis LST pixels10.png",
  width = 10,
  height = 8,
  units = "cm"
)  

coverP <- read_xlsx("input_data/Drone_extract_prosopis_cover_PlanetGrid.xlsx")
p3 <- ggplot(aes(x = frac_1 ), data = coverP) + 
  geom_histogram(aes(fill = frac_1 ), binwidth=0.1, colour="grey20", lwd=0.2) +
  stat_bin(binwidth=0.1, geom="text", colour="black", size=2,
           aes(label=..count..,vjust = -1))+theme_fancy() + xlab("Sub-pixel proportion of Neltuma")+ ylab("Number of Planet pixels")+ggtitle("3m resolution")

p3

ggsave(
  p3,
  # filename = "/plots/test.png",
  filename = "output_data/Fraction cover Prosopis Planetpixels10.png",
  width = 10,
  height = 8,
  units = "cm"
)  

coverW <- read_xlsx("input_data/Drone_extract_prosopis_cover_WV2Grid.xlsx")
p1 <- ggplot(aes(x = frac_1 ), data = coverW) + 
  geom_histogram(aes(fill = frac_1 ), binwidth=0.1, colour="grey20", lwd=0.2) +
  stat_bin(binwidth=0.1, geom="text", colour="black", size=2,
           aes(label=..count..,vjust = -1))+theme_fancy() + xlab("Sub-pixel proportion of Neltuma")+ ylab("Number of WV2 pixels")+ggtitle("1.6m resolution")

p1



ggsave(
  p1,
  # filename = "/plots/test.png",
  filename = "output_data/Fraction cover Prosopis WV2pixels10.png",
  width = 10,
  height = 8,
  units = "cm"
)  

cover_all <- read_xlsx("input_data/Prosopis_cover_all.xlsx")
p5 <- ggplot(aes(x = Cover,y = WV2 ), data = cover_all)+ geom_bar(stat = "identity")+ 
  xlab("Sub-pixel proportion of Neltuma")+ ylab("Number of WV2 pixels")+theme_fancy()+ggtitle("1.6m resolution")

p5         
p6 <- ggplot(aes(x = Cover,y = Planet ), data = cover_all)+ geom_bar(stat = "identity")+ 
  xlab("Sub-pixel proportion of Neltuma")+ ylab("Number of Planet pixels")+theme_fancy()+ggtitle("3m resolution")

p6   
p7 <- ggplot(aes(x = Cover,y = S2 ), data = cover_all)+ geom_bar(stat = "identity")+ 
  xlab("Sub-pixel proportion of Neltuma")+ ylab("Number of S2 pixels")+theme_fancy()+ggtitle("10m resolution")

p7   
p8 <- ggplot(aes(x = Cover,y = Landsat ), data = cover_all)+ geom_bar(stat = "identity")+ 
  xlab("Sub-pixel proportion of Neltuma")+ ylab("Number of Landsat pixels")+theme_fancy()+ggtitle("30m resolution")

p8   

patchwork4 <- p5+p6+p7+p8

patchworkf<- patchwork4 + plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 10))


patchworkf

ggsave(
  patchworkf,
  # filename = "/plots/test.png",
  filename = "output_data/Fraction cover Prosopis All combined simple.png",
  width = 16,
  height = 16,
  units = "cm"
) 
patchwork5 <- p5+p6+p7

patchworkg<- patchwork5 + plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 10))


patchworkg

ggsave(
  patchworkg,
  # filename = "/plots/test.png",
  filename = "output_data/Fraction cover Neltuma All combined simple.png",
  width = 16,
  height = 8,
  units = "cm"
) 



patchwork1 <- p1+p3+p2+p4

patchwork<- patchwork1 + plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 10))


patchwork
ggsave(
  patchwork,
  # filename = "/plots/test.png",
  filename = "output_data/Fraction cover Prosopis All combined.png",
  width = 16,
  height = 16,
  units = "cm"
) 


### Density plots


coverP <- coverP %>%
  mutate(text="Planet")
coverP <- dplyr::select(coverP ,-c(geometry))

coverW <-coverW %>%
  mutate(text="WV2")
coverW <- dplyr::select(coverW ,-c(geometry))

coverS2 <- cover %>%
  mutate(text="S2")
coverS2 <- dplyr::select(coverS2 ,-c(geometry,FID))

coverLST<-coverLST %>%
  mutate(text="LST")
coverLST <- dplyr::select(coverLST ,-c(geometry,FID))


cover_master <- rbind(coverP,coverW,coverS2,coverLST)

# A dataframe for annotations
annot <- data.frame(
  text = c("WV2", "S2", "Planet", "LST"),
  x = c(5, 53, 65, 79),
  y = c(0.15, 0.4, 0.06, 0.1)
)


p_density <-  
  ggplot( aes(x=frac_1, color=text, fill=text),data = cover_master) +
  geom_density(alpha=0.4) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  #geom_text( data=annot, aes(x=x, y=y, label=text, color=text), hjust=0, size=4.5) +
  #theme_ipsum() +
  ylim(0, 100)+xlim(0,1)+
  ylab("") +
  xlab("Sub-pixel proportion of Neltuma")+theme_fancy()
p_density


ggsave(
  p_density,
  # filename = "/plots/test.png",
  filename = "output_data/Fraction cover Prosopis simple density.png",
  width = 16,
  height = 16,
  units = "cm"
) 


cover_simple <- read_xlsx("input_data/Drone_extract_prosopis_cover_resolution_simple.xlsx")

p_line <- ggplot( aes(x=Fractional_cover, y= Proportion, color=Sensor_resolution, fill=Sensor_resolution),data = cover_simple) +
  geom_line() +ylim (0,100)+theme_fancy()
p_line


ggsave(
  p_line,
  # filename = "/plots/test.png",
  filename = "output_data/Fraction cover Prosopis simple line.png",
  width = 16,
  height = 16,
  units = "cm"
) 
