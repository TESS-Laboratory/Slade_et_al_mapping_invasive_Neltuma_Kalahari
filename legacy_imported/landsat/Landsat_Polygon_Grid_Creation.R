# ===========================================================================
# IMPORTED - not part of the original published archive.
#
#   source repo   slade-prosopis
#   source path   Landsat/Landsat_Polygon_Grid_Creation.R
#   ref           main @ 671e56f
#   sha256        16c534a737770394cb50435ab986358815631daf0b4d1f984c7c58ee6e806ded
#
#   why           Landsat arm. Produces the 30 m cover table used by the optional fourth panel of the Figure 5 script.
#
# Content below is VERBATIM and does not run as-is: it depends on Windows
# absolute paths under E:/Glenn/Botswana/ and on archived packages (rgeos,
# rgdal), and calls windowsFonts(). It is kept as the reference
# implementation to port into the targets pipeline, not to execute.
# See refactor-findings.md and audit/source-recovery-map.md.
# ===========================================================================

### Script to produce shapefiles of LSTScope ploygons
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

LSTWGS84 <- rast("E:/Glenn/Botswana/satellite_Data/LST/LS8/LS8_Large_stack.tif")
LST <- LSTWGS84
#LST <-terra::project(LSTWGS84, y="EPSG:32734")
LST

WV2_clipper <- read_sf(dsn = 'E:/Glenn/Botswana/Satellite_Data/WV2/AOI', layer = "WV2_clip")
WV2_clip <- vect(WV2_clipper)

LST1 <- crop(LST,WV2_clip )
LST_WV2<- mask(LST1,WV2_clip )
plot(LST_WV2)

WV2_LST_grid <- as.polygons(LST_WV2 , dissolve = FALSE, values = FALSE,extent=FALSE)
WV2_LST_grid
plot(WV2_LST_grid)
g <-st_as_sf(WV2_LST_grid)
c<-st_as_sf(WV2_clip)
u1 <-spatial.select(c, g, predicate = "contains")
write_sf(u1,"E:/Glenn/Botswana/Satellite_Data/LST/LST_Grids/WV2_LST_grid.shp",overwrite=TRUE )
plot (u1)









# Grid for Bokspits_1

Bokspits_1_clipper <- read_sf(dsn = 'E:/Glenn/Botswana/Final_Drone_Survey_Data/Bokspits_1', layer = "Bokspits_1_clip")
Bokspits_1_clip <- vect(Bokspits_1_clipper)

LST1 <- crop(LST,Bokspits_1_clip )
LST_Bokspits_1 <- mask(LST1,Bokspits_1_clip )
plot(LST_Bokspits_1)

Bokspits_1_LST_grid <- as.polygons(LST_Bokspits_1 , dissolve = FALSE, values = FALSE,extent=FALSE)
Bokspits_1_LST_grid
g <-st_as_sf(Bokspits_1_LST_grid)
c<-st_as_sf(Bokspits_1_clip)
u1 <-spatial.select(c, g, predicate = "contains")
write_sf(u1,"E:/Glenn/Botswana/Satellite_Data/LST/LST_Grids/Bokspits_1_LST_grid.shp",overwrite=TRUE )
plot (u1)
 
#Making Grid for Bokspits_2

Bokspits_2_clipper <- read_sf(dsn = 'E:/Glenn/Botswana/Final_Drone_Survey_Data/Bokspits_2', layer = "Bokspits_2_clip")
Bokspits_2_clip <- vect(Bokspits_2_clipper)

LST2 <- crop(LST,Bokspits_2_clip )
LST_Bokspits_2 <- mask(LST2,Bokspits_2_clip )
plot(LST_Bokspits_2)

Bokspits_2_LST_grid <- as.polygons(LST_Bokspits_2 , dissolve = FALSE, values = FALSE,extent=FALSE)
plot (Bokspits_2_LST_grid)
g <-st_as_sf(Bokspits_2_LST_grid)
c<-st_as_sf(Bokspits_2_clip)
u2 <-spatial.select(c, g, predicate = "contains")
write_sf(u2,"E:/Glenn/Botswana/Satellite_Data/LST/LST_Grids/Bokspits_2_LST_grid.shp",overwrite=TRUE )
plot (u2)
 
#Making Grid for Bokspits_3

Bokspits_3_clipper <- read_sf(dsn = 'E:/Glenn/Botswana/Final_Drone_Survey_Data/Bokspits_3', layer = "Bokspits_3_clip")
Bokspits_3_clip <- vect(Bokspits_3_clipper)

LST3 <- crop(LST,Bokspits_3_clip )
LST_Bokspits_3 <- mask(LST3,Bokspits_3_clip )
plot(LST_Bokspits_3)

Bokspits_3_LST_grid <- as.polygons(LST_Bokspits_3 , dissolve = FALSE, values = FALSE,extent=FALSE)
plot (Bokspits_3_LST_grid)
g <-st_as_sf(Bokspits_3_LST_grid)
c<-st_as_sf(Bokspits_3_clip)
u3 <-spatial.select(c, g, predicate = "contains")
write_sf(u3,"E:/Glenn/Botswana/Satellite_Data/LST/LST_Grids/Bokspits_3_LST_grid.shp",overwrite=TRUE )
plot (u3)
 
#Making Grid for Struizendam_1


Struizendam_1_clipper <- read_sf(dsn = 'E:/Glenn/Botswana/Final_Drone_Survey_Data/Struizendam_1', layer = "Struizendam_1_clip")
Struizendam_1_clip <- vect(Struizendam_1_clipper)

LST1 <- crop( LST,Struizendam_1_clip )
 LSTtruizendam_1 <- mask(LST1,Struizendam_1_clip )
plot( LSTtruizendam_1)

Struizendam_1_LST_grid <- as.polygons( LSTtruizendam_1 , dissolve = FALSE, values = FALSE,extent=FALSE)
plot (Struizendam_1_LST_grid)
g <-st_as_sf(Struizendam_1_LST_grid)
c<-st_as_sf(Struizendam_1_clip)
u4 <-spatial.select(c, g, predicate = "contains")
write_sf(u4,"E:/Glenn/Botswana/Satellite_Data/LST/LST_Grids/Struizendam_1_LST_grid.shp",overwrite=TRUE )
plot (u4)
 
#Making Grid for Struizendam_2

Struizendam_2_clipper <- read_sf(dsn = 'E:/Glenn/Botswana/Final_Drone_Survey_Data/Struizendam_2', layer = "Struizendam_2_clip")
Struizendam_2_clip <- vect(Struizendam_2_clipper)

LST2 <- crop( LST,Struizendam_2_clip )
 LSTtruizendam_2 <- mask(LST2,Struizendam_2_clip )
plot( LSTtruizendam_2)

Struizendam_2_LST_grid <- as.polygons( LSTtruizendam_2 , dissolve = FALSE, values = FALSE,extent=FALSE)
plot (Struizendam_2_LST_grid)
g <-st_as_sf(Struizendam_2_LST_grid)
c<-st_as_sf(Struizendam_2_clip)
u5 <-spatial.select(c, g, predicate = "contains")
write_sf(u5,"E:/Glenn/Botswana/Satellite_Data/LST/LST_Grids/Struizendam_2_LST_grid.shp",overwrite=TRUE )
plot (u5)
 
#Making Grid for Struizendam_3

Struizendam_3_clipper <- read_sf(dsn = 'E:/Glenn/Botswana/Final_Drone_Survey_Data/Struizendam_3', layer = "Struizendam_3_clip")
Struizendam_3_clip <- vect(Struizendam_3_clipper)

LST3 <- crop( LST,Struizendam_3_clip )
 LSTtruizendam_3 <- mask(LST3,Struizendam_3_clip )
plot( LSTtruizendam_3)

Struizendam_3_LST_grid <- as.polygons( LSTtruizendam_3 , dissolve = FALSE, values = FALSE,extent=FALSE)
plot (Struizendam_3_LST_grid)
g <-st_as_sf(Struizendam_3_LST_grid)
c<-st_as_sf(Struizendam_3_clip)
u6 <-spatial.select(c, g, predicate = "contains")
write_sf(u6,"E:/Glenn/Botswana/Satellite_Data/LST/LST_Grids/Struizendam_3_LST_grid.shp",overwrite=TRUE )
plot (u6)
 
#Making Grid for Struizendam_4

Struizendam_4_clipper <- read_sf(dsn = 'E:/Glenn/Botswana/Final_Drone_Survey_Data/Struizendam_4', layer = "Struizendam_4_clip")
Struizendam_4_clip <- vect(Struizendam_4_clipper)

LST4 <- crop( LST,Struizendam_4_clip )
 LSTtruizendam_4 <- mask(LST4,Struizendam_4_clip )
plot( LSTtruizendam_4)

Struizendam_4_LST_grid <- as.polygons( LSTtruizendam_4 , dissolve = FALSE, values = FALSE,extent=FALSE)
plot (Struizendam_4_LST_grid)
g <-st_as_sf(Struizendam_4_LST_grid)
c<-st_as_sf(Struizendam_4_clip)
u7 <-spatial.select(c, g, predicate = "contains")
write_sf(u7,"E:/Glenn/Botswana/Satellite_Data/LST/LST_Grids/Struizendam_4_LST_grid.shp",overwrite=TRUE )
plot (u7)
 

#Making Combined Grid for all surveys
