### Script to produce shapefiles of WV2Scope ploygons
### matching pixels

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
library(spatialEco)

#----1. Read in files----

WV2 <- rast("E:/Glenn/Botswana/Satellite_Data/WV2/1_6_m_mosaic/WV2_Corrected.tif")

Bokspits_1_clipper <- read_sf(dsn = 'E:/Glenn/Botswana/Final_Drone_Survey_Data/Bokspits_1', layer = "Bokspits_1_clip")
Bokspits_1_clip <- vect(Bokspits_1_clipper)

WV21 <- crop(WV2,Bokspits_1_clip )
WV2_Bokspits_1 <- mask(WV21,Bokspits_1_clip )
plot(WV2_Bokspits_1)

Bokspits_1_WV2_grid <- as.polygons(WV2_Bokspits_1 , dissolve = FALSE, values = FALSE,extent=FALSE)
Bokspits_1_WV2_grid
g <-st_as_sf(Bokspits_1_WV2_grid)
c<-st_as_sf(Bokspits_1_clip)
u1 <-spatial.select(c, g, predicate = "contains")
write_sf(u1,"E:/Glenn/Botswana/Satellite_Data/WV2/WV2_Grids/Bokspits_1_WV2_grid.shp",overwrite=TRUE )
plot (u1)
 
#Making Grid for Bokspits_2

Bokspits_2_clipper <- read_sf(dsn = 'E:/Glenn/Botswana/Final_Drone_Survey_Data/Bokspits_2', layer = "Bokspits_2_clip")
Bokspits_2_clip <- vect(Bokspits_2_clipper)

WV22 <- crop(WV2,Bokspits_2_clip )
WV2_Bokspits_2 <- mask(WV22,Bokspits_2_clip )
plot(WV2_Bokspits_2)

Bokspits_2_WV2_grid <- as.polygons(WV2_Bokspits_2 , dissolve = FALSE, values = FALSE,extent=FALSE)
plot (Bokspits_2_WV2_grid)
g <-st_as_sf(Bokspits_2_WV2_grid)
c<-st_as_sf(Bokspits_2_clip)
u2 <-spatial.select(c, g, predicate = "contains")
write_sf(u2,"E:/Glenn/Botswana/Satellite_Data/WV2/WV2_Grids/Bokspits_2_WV2_grid.shp",overwrite=TRUE )
plot (u2)
 
#Making Grid for Bokspits_3

Bokspits_3_clipper <- read_sf(dsn = 'E:/Glenn/Botswana/Final_Drone_Survey_Data/Bokspits_3', layer = "Bokspits_3_clip")
Bokspits_3_clip <- vect(Bokspits_3_clipper)

WV23 <- crop(WV2,Bokspits_3_clip )
WV2_Bokspits_3 <- mask(WV23,Bokspits_3_clip )
plot(WV2_Bokspits_3)

Bokspits_3_WV2_grid <- as.polygons(WV2_Bokspits_3 , dissolve = FALSE, values = FALSE,extent=FALSE)
plot (Bokspits_3_WV2_grid)
g <-st_as_sf(Bokspits_3_WV2_grid)
c<-st_as_sf(Bokspits_3_clip)
u3 <-spatial.select(c, g, predicate = "contains")
write_sf(u3,"E:/Glenn/Botswana/Satellite_Data/WV2/WV2_Grids/Bokspits_3_WV2_grid.shp",overwrite=TRUE )
plot (u3)
 
#Making Grid for Struizendam_1


Struizendam_1_clipper <- read_sf(dsn = 'E:/Glenn/Botswana/Final_Drone_Survey_Data/Struizendam_1', layer = "Struizendam_1_clip")
Struizendam_1_clip <- vect(Struizendam_1_clipper)

WV21 <- crop( WV2,Struizendam_1_clip )
 WV2truizendam_1 <- mask(WV21,Struizendam_1_clip )
plot( WV2truizendam_1)

Struizendam_1_WV2_grid <- as.polygons( WV2truizendam_1 , dissolve = FALSE, values = FALSE,extent=FALSE)
plot (Struizendam_1_WV2_grid)
g <-st_as_sf(Struizendam_1_WV2_grid)
c<-st_as_sf(Struizendam_1_clip)
u4 <-spatial.select(c, g, predicate = "contains")
write_sf(u4,"E:/Glenn/Botswana/Satellite_Data/WV2/WV2_Grids/Struizendam_1_WV2_grid.shp",overwrite=TRUE )
plot (u4)
 
#Making Grid for Struizendam_2

Struizendam_2_clipper <- read_sf(dsn = 'E:/Glenn/Botswana/Final_Drone_Survey_Data/Struizendam_2', layer = "Struizendam_2_clip")
Struizendam_2_clip <- vect(Struizendam_2_clipper)

WV22 <- crop( WV2,Struizendam_2_clip )
 WV2truizendam_2 <- mask(WV22,Struizendam_2_clip )
plot( WV2truizendam_2)

Struizendam_2_WV2_grid <- as.polygons( WV2truizendam_2 , dissolve = FALSE, values = FALSE,extent=FALSE)
plot (Struizendam_2_WV2_grid)
g <-st_as_sf(Struizendam_2_WV2_grid)
c<-st_as_sf(Struizendam_2_clip)
u5 <-spatial.select(c, g, predicate = "contains")
write_sf(u5,"E:/Glenn/Botswana/Satellite_Data/WV2/WV2_Grids/Struizendam_2_WV2_grid.shp",overwrite=TRUE )
plot (u5)
 
#Making Grid for Struizendam_3

Struizendam_3_clipper <- read_sf(dsn = 'E:/Glenn/Botswana/Final_Drone_Survey_Data/Struizendam_3', layer = "Struizendam_3_clip")
Struizendam_3_clip <- vect(Struizendam_3_clipper)

WV23 <- crop( WV2,Struizendam_3_clip )
 WV2truizendam_3 <- mask(WV23,Struizendam_3_clip )
plot( WV2truizendam_3)

Struizendam_3_WV2_grid <- as.polygons( WV2truizendam_3 , dissolve = FALSE, values = FALSE,extent=FALSE)
plot (Struizendam_3_WV2_grid)
g <-st_as_sf(Struizendam_3_WV2_grid)
c<-st_as_sf(Struizendam_3_clip)
u6 <-spatial.select(c, g, predicate = "contains")
write_sf(u6,"E:/Glenn/Botswana/Satellite_Data/WV2/WV2_Grids/Struizendam_3_WV2_grid.shp",overwrite=TRUE )
plot (u6)
 
#Making Grid for Struizendam_4

Struizendam_4_clipper <- read_sf(dsn = 'E:/Glenn/Botswana/Final_Drone_Survey_Data/Struizendam_4', layer = "Struizendam_4_clip")
Struizendam_4_clip <- vect(Struizendam_4_clipper)

WV24 <- crop( WV2,Struizendam_4_clip )
 WV2truizendam_4 <- mask(WV24,Struizendam_4_clip )
plot( WV2truizendam_4)

Struizendam_4_WV2_grid <- as.polygons( WV2truizendam_4 , dissolve = FALSE, values = FALSE,extent=FALSE)
plot (Struizendam_4_WV2_grid)
g <-st_as_sf(Struizendam_4_WV2_grid)
c<-st_as_sf(Struizendam_4_clip)
u7 <-spatial.select(c, g, predicate = "contains")
write_sf(u7,"E:/Glenn/Botswana/Satellite_Data/WV2/WV2_Grids/Struizendam_4_WV2_grid.shp",overwrite=TRUE )
plot (u7)
 

#Making Combined Grid for all surveys



