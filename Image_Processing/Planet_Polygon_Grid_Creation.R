### Script to produce shapefiles of PlanetScope ploygons
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


### Script to produce shapefiles of S2Scope ploygons
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

Planet <- rast("E:/Glenn/Botswana/satellite_Data/Planet/Planet_Scope_Boravast/Boravast_2022_09_09.tif")
Planet

PL <- spatSample (Planet, 250000,method="random") 
PL

WV2_clipper <- read_sf(dsn = 'E:/Glenn/Botswana/Satellite_Data/WV2/AOI', layer = "WV2_clip")
WV2_clip <- vect(WV2_clipper)
WV2_clip
PL1 <- crop(PL,WV2_clip )
PL_WV2<- mask(PL1,WV2_clip )
plot(PL_WV2)

WV2_PL_grid <- as.polygons(PL_WV2 , dissolve = FALSE, values = TRUE,extent=FALSE)
WV2_PL_grid
#plot(WV2_PL_grid)
g <-st_as_sf(WV2_PL_grid)
c<-st_as_sf(WV2_clip)
u1 <-spatial.select(c, g, predicate = "contains")

#u1 <-crop(WV2_PL_grid,WV2_clip)
#writeVector(u1,"E:/Glenn/Botswana/Satellite_Data/S2/S2_Grids/WV2_S2_grid.shp",overwrite=TRUE )

write_sf(u1,"E:/Glenn/Botswana/Satellite_Data/Planet/Planet_Grids/WV2_Planet_grid.shp",overwrite=TRUE )
#plot (u1)



















Planet <- rast("E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope/Bokspits_2022_01_01.tif")

Bokspits_1_clipper <- read_sf(dsn = 'E:/Glenn/Botswana/Final_Drone_Survey_Data/Bokspits_1', layer = "Bokspits_1_clip")
Bokspits_1_clip <- vect(Bokspits_1_clipper)

Planet1 <- crop(Planet,Bokspits_1_clip )
Planet_Bokspits_1 <- mask(Planet1,Bokspits_1_clip )
plot(Planet_Bokspits_1)

Bokspits_1_Planet_grid <- as.polygons(Planet_Bokspits_1 , dissolve = FALSE, values = TRUE,extent=FALSE)
plot (Bokspits_1_Planet_grid)
g <-st_as_sf(Bokspits_1_Planet_grid)
c<-st_as_sf(Bokspits_1_clip)
u <-spatial.select(c, g, predicate = "contains")
write_sf(u,"E:/Glenn/Botswana/Satellite_Data/Planet/Planet_Grids/Bokspits_1_Planet_grid.shp",overwrite=TRUE )


#Making Grid for Bokspits_2

Bokspits_2_clipper <- read_sf(dsn = 'E:/Glenn/Botswana/Final_Drone_Survey_Data/Bokspits_2', layer = "Bokspits_2_clip")
Bokspits_2_clip <- vect(Bokspits_2_clipper)

Planet2 <- crop(Planet,Bokspits_2_clip )
Planet_Bokspits_2 <- mask(Planet2,Bokspits_2_clip )
plot(Planet_Bokspits_2)

Bokspits_2_Planet_grid <- as.polygons(Planet_Bokspits_2 , dissolve = FALSE, values = TRUE,extent=FALSE)
plot (Bokspits_2_Planet_grid)
g <-st_as_sf(Bokspits_2_Planet_grid)
c<-st_as_sf(Bokspits_2_clip)
u <-spatial.select(c, g, predicate = "contains")
write_sf(u,"E:/Glenn/Botswana/Satellite_Data/Planet/Planet_Grids/Bokspits_2_Planet_grid.shp",overwrite=TRUE )

#Making Grid for Bokspits_3

Bokspits_3_clipper <- read_sf(dsn = 'E:/Glenn/Botswana/Final_Drone_Survey_Data/Bokspits_3', layer = "Bokspits_3_clip")
Bokspits_3_clip <- vect(Bokspits_3_clipper)

Planet3 <- crop(Planet,Bokspits_3_clip )
Planet_Bokspits_3 <- mask(Planet3,Bokspits_3_clip )
plot(Planet_Bokspits_3)

Bokspits_3_Planet_grid <- as.polygons(Planet_Bokspits_3 , dissolve = FALSE, values = TRUE,extent=FALSE)
plot (Bokspits_3_Planet_grid)
g <-st_as_sf(Bokspits_3_Planet_grid)
c<-st_as_sf(Bokspits_3_clip)
u <-spatial.select(c, g, predicate = "contains")
write_sf(u,"E:/Glenn/Botswana/Satellite_Data/Planet/Planet_Grids/Bokspits_3_Planet_grid.shp",overwrite=TRUE )

#Making Grid for Struizendam_1

Planet_S <- rast("E:/Glenn/Botswana/Satellite_Data/Planet/2022_Planetscope/Struizendam_2022_09_07.tif")

Struizendam_1_clipper <- read_sf(dsn = 'E:/Glenn/Botswana/Final_Drone_Survey_Data/Struizendam_1', layer = "Struizendam_1_clip")
Struizendam_1_clip <- vect(Struizendam_1_clipper)

Planet1 <- crop(Planet_S,Struizendam_1_clip )
Planet_Struizendam_1 <- mask(Planet1,Struizendam_1_clip )
plot(Planet_Struizendam_1)

Struizendam_1_Planet_grid <- as.polygons(Planet_Struizendam_1 , dissolve = FALSE, values = TRUE,extent=FALSE)
plot (Struizendam_1_Planet_grid)
g <-st_as_sf(Struizendam_1_Planet_grid)
c<-st_as_sf(Struizendam_1_clip)
u <-spatial.select(c, g, predicate = "contains")
write_sf(u,"E:/Glenn/Botswana/Satellite_Data/Planet/Planet_Grids/Struizendam_1_Planet_grid.shp",overwrite=TRUE )

#Making Grid for Struizendam_2

Struizendam_2_clipper <- read_sf(dsn = 'E:/Glenn/Botswana/Final_Drone_Survey_Data/Struizendam_2', layer = "Struizendam_2_clip")
Struizendam_2_clip <- vect(Struizendam_2_clipper)

Planet2 <- crop(Planet_S,Struizendam_2_clip )
Planet_Struizendam_2 <- mask(Planet2,Struizendam_2_clip )
plot(Planet_Struizendam_2)

Struizendam_2_Planet_grid <- as.polygons(Planet_Struizendam_2 , dissolve = FALSE, values = TRUE,extent=FALSE)
plot (Struizendam_2_Planet_grid)
g <-st_as_sf(Struizendam_2_Planet_grid)
c<-st_as_sf(Struizendam_2_clip)
u <-spatial.select(c, g, predicate = "contains")
write_sf(u,"E:/Glenn/Botswana/Satellite_Data/Planet/Planet_Grids/Struizendam_2_Planet_grid.shp",overwrite=TRUE )

#Making Grid for Struizendam_3

Struizendam_3_clipper <- read_sf(dsn = 'E:/Glenn/Botswana/Final_Drone_Survey_Data/Struizendam_3', layer = "Struizendam_3_clip")
Struizendam_3_clip <- vect(Struizendam_3_clipper)

Planet3 <- crop(Planet_S,Struizendam_3_clip )
Planet_Struizendam_3 <- mask(Planet3,Struizendam_3_clip )
plot(Planet_Struizendam_3)

Struizendam_3_Planet_grid <- as.polygons(Planet_Struizendam_3 , dissolve = FALSE, values = TRUE,extent=FALSE)
plot (Struizendam_3_Planet_grid)
g <-st_as_sf(Struizendam_3_Planet_grid)
c<-st_as_sf(Struizendam_3_clip)
u <-spatial.select(c, g, predicate = "contains")
write_sf(u,"E:/Glenn/Botswana/Satellite_Data/Planet/Planet_Grids/Struizendam_3_Planet_grid.shp",overwrite=TRUE )

#Making Grid for Struizendam_4

Struizendam_4_clipper <- read_sf(dsn = 'E:/Glenn/Botswana/Final_Drone_Survey_Data/Struizendam_4', layer = "Struizendam_4_clip")
Struizendam_4_clip <- vect(Struizendam_4_clipper)

Planet4 <- crop(Planet_S,Struizendam_4_clip )
Planet_Struizendam_4 <- mask(Planet4,Struizendam_4_clip )
plot(Planet_Struizendam_4)

Struizendam_4_Planet_grid <- as.polygons(Planet_Struizendam_4 , dissolve = FALSE, values = TRUE,extent=FALSE)
plot (Struizendam_4_Planet_grid)
g <-st_as_sf(Struizendam_4_Planet_grid)
c<-st_as_sf(Struizendam_4_clip)
u <-spatial.select(c, g, predicate = "contains")
write_sf(u,"E:/Glenn/Botswana/Satellite_Data/Planet/Planet_Grids/Struizendam_4_Planet_grid.shp",overwrite=TRUE )


#u <- terra::intersect(Bokspits_2_clip,Bokspits_2_Planet_grid) #clips grid cells into small sections at edges

