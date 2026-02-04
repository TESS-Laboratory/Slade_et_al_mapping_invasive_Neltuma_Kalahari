# PCA analysis of Planet data extracted for different vegetation types

#Script for performing a PCA on csv file
library(ggbiplot)
# Load ggplot2
library(ggplot2)
# Libraries
library(tidyverse)
library(viridis)
library(DescTools)

# perform PCA

rds <-read.csv("E:/Glenn/Botswana/R_Scripts/slade-prosopis/output_data/Boravast_Planet_NDVI_Extracted_95.csv")


rds.pca <- prcomp(rds[,c(18,24)], center = TRUE,scale. = TRUE)
rds.pca

# Plot PCA elipses in 2 phase space
library(ggbiplot)

ggbiplot(rds.pca)
ggbiplot(rds.pca,ellipse=TRUE,  labels=rds$majority, groups=rds$majority)
ggbiplot(rds.pca,ellipse=TRUE, choices=c(3,4), labels=rds$majority, groups=rds$X___6)


write.csv(rds.pca$rotation, "E:/Glenn/Botswana/R_Scripts/slade-prosopis/output_data/pca_pcrcomp_rot.csv", row.names=TRUE)
write.csv(rds.pca$sdev, "E:/Glenn/Botswana/R_Scripts/slade-prosopis/output_data/pca_pcrcomp_std.csv", row.names=TRUE)

# plot histograms of PCA
pca2 <- read.csv("E:/Glenn/Botswana/R_Scripts/slade-prosopis/output_data/pca2std.csv")
head (pca2)
bp <- ggplot(data = pca2, mapping = aes (x=X ,y=x))+ 
  xlab("Principal Component - Standard") +geom_bar(stat="identity")+ ylab("Standard Deviation")
bp


sd <- rds.pca$sdev
loadings <- rds.pca$rotation
rownames(loadings) <- colnames(rds.pca)
scores <- rds.pca$x
var <- sd^2
varPercent <- var/sum(var) * 100


bp2 <-ggplot(data = pca2, mapping = aes (x=X ,y=varPercent))+ 
  xlab("Principal Component") +geom_bar(stat="identity")+ ylab("Percent Variation")+# + abline(h=1/ncol(rds.pca)*100, col='red')
  geom_hline(yintercept=9.09, color="red", linewidth=0.5)
bp2


# Plot correlation between factors

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
      axis.text.x = element_text(angle = 90),
      panel.border = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank(),
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
      plot.title = element_text(
        size = 9,
        vjust = 1,
        hjust = 0.5,
        color = "black"
      ),
      legend.text = element_text(size =7, color = "black"),
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


# Assign axis
x <- as.vector(rds$majority)
y <- as.vector(rds$Boravast_2022_04_NDVI)
# Make Data Frame
df2 <- data.frame(x = x, y = y,
                  d = densCols(x, y, colramp = colorRampPalette(rev(c('yellow','orange','turquoise4','dodgerblue4')))))#colorRampPalette(rev(rainbow(10, end = 4/6)))))

# Calculate Total Least Squares Regression (extracted from base-R PCA function)
pca <- prcomp(~x+y,df2)
tls_slp <- with(pca, rotation[2,1] / rotation[1,1]) # compute slope
tls_int <- with(pca, center[2] - tls_slp*center[1]) # compute y-intercept
equation <- paste("y = ", round(tls_int, 3), "+", round(tls_slp, 3), "x")

# Compute the Lin's  correlation concordance coefficient
ccc_result <- CCC(x, y, ci = "z-transform",conf.level = 0.95)
ccc <- paste("CCC = ", round(ccc_result$rho.c[1], 3))

# Calculate OLS
MADval <- mean(abs(x-y))

MADrel <- MADval/mean(x)*100
lmres <- lm(y~x)
r2val <- summary(lmres)$r.squared

# Plot
psndvi <- ggplot(df2) +
  geom_smooth(aes(x, y,col='grey',weight=0.01),method='lm',formula=y ~ x,se=FALSE) +
  geom_point(aes(x, y), alpha=0.3, size = 1) +
  geom_text(aes(x=0.0,y=0.5),label=paste0('MAD: ',round(MADval,3)),hjust='left',size=2.5)+
  geom_text(aes(x=0.0,y=0.47),label=paste0('R2: ',round(r2val,2)),hjust='left',size=2.5)+
  geom_text(aes(x=0.0,y=0.44),label=ccc,hjust='left', size=2.5)+
  geom_text(aes(x=0.0,y=0.41),label=equation,hjust='left', size=2.5)+
  
  #theme(text = element_text(size=20))+
  scale_color_identity() +
  theme_fancy() +
  
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("Comparison of Tramway Mean Data with SEQ \n Survey NDVI")+
  #theme(aspect.ratio=1)+
  xlab('Tramway Relectance (resampled for SEQ) NDVI')+
  ylab('SEQ NDVI')#+
  #coord_equal(ratio=1)
  #coord_fixed(xlim=c(0,0.5),ylim=c(0,0.5))
plot(psndvi)

