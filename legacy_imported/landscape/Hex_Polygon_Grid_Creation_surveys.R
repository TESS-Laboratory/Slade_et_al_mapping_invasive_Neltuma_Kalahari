# ===========================================================================
# IMPORTED - not part of the original published archive.
#
#   source repo   slade-prosopis
#   source path   Aggregate/Hex_Polygon_Grid_Creation_surveys.R
#   ref           main @ 671e56f
#   sha256        02d397f8ec9effe005d86ccfbf0f97a5827326b7242655b6974e5dc9a6edf146
#
#   why           Hexagonal aggregation grid construction (100 m / 250 m / 500 m).
#
# Content below is VERBATIM and does not run as-is: it depends on Windows
# absolute paths under E:/Glenn/Botswana/ and on archived packages (rgeos,
# rgdal), and calls windowsFonts(). It is kept as the reference
# implementation to port into the targets pipeline, not to execute.
# See refactor-findings.md and audit/source-recovery-map.md.
# ===========================================================================

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

hex250_Grid_SF <- read_sf(dsn = 'E:/Glenn/Botswana/GIS_aggregate', layer = "hex_100m_wv2_clip")

Bokspits_1_clipper <- read_sf(dsn = 'E:/Glenn/Botswana/Final_Drone_Survey_Data/Bokspits_1', layer = "Bokspits_1_clip")
Bokspits_1_clip <- vect(Bokspits_1_clipper)

g <-st_as_sf(hex250_Grid_SF)
c<-st_as_sf(Bokspits_1_clip)
u1 <-spatial.select(c, g, predicate = "contains")
write_sf(u1,"E:/Glenn/Botswana/Satellite_Data/hex/hex_grids/Bokspits_1_hex_grid.shp",overwrite=TRUE )
plot (u1)
 
#Making Grid for Bokspits_2

Bokspits_2_clipper <- read_sf(dsn = 'E:/Glenn/Botswana/Final_Drone_Survey_Data/Bokspits_2', layer = "Bokspits_2_clip")
Bokspits_2_clip <- vect(Bokspits_2_clipper)

g <-st_as_sf(hex250_Grid_SF)
c<-st_as_sf(Bokspits_2_clip)
u1 <-spatial.select(c, g, predicate = "contains")
write_sf(u1,"E:/Glenn/Botswana/Satellite_Data/hex/hex_grids/Bokspits_2_hex_grid.shp",overwrite=TRUE )
plot (u1)
 
#Making Grid for Bokspits_3

Bokspits_3_clipper <- read_sf(dsn = 'E:/Glenn/Botswana/Final_Drone_Survey_Data/Bokspits_3', layer = "Bokspits_3_clip")
Bokspits_3_clip <- vect(Bokspits_3_clipper)

g <-st_as_sf(hex250_Grid_SF)
c<-st_as_sf(Bokspits_3_clip)
u1 <-spatial.select(c, g, predicate = "contains")
write_sf(u1,"E:/Glenn/Botswana/Satellite_Data/hex/hex_grids/Bokspits_3_hex_grid.shp",overwrite=TRUE )
plot (u1)

 
#Making Grid for Struizendam_1

Struizendam_1_clipper <- read_sf(dsn = 'E:/Glenn/Botswana/Final_Drone_Survey_Data/Struizendam_1', layer = "Struizendam_1_clip")
Struizendam_1_clip <- vect(Struizendam_1_clipper)

g <-st_as_sf(hex250_Grid_SF)
c<-st_as_sf(Struizendam_1_clip)
u1 <-spatial.select(c, g, predicate = "contains")
write_sf(u1,"E:/Glenn/Botswana/Satellite_Data/hex/hex_grids/Struizendam_1_hex_grid.shp",overwrite=TRUE )
plot (u1)

 
#Making Grid for Struizendam_2

Struizendam_2_clipper <- read_sf(dsn = 'E:/Glenn/Botswana/Final_Drone_Survey_Data/Struizendam_2', layer = "Struizendam_2_clip")
Struizendam_2_clip <- vect(Struizendam_2_clipper)

g <-st_as_sf(hex250_Grid_SF)
c<-st_as_sf(Struizendam_2_clip)
u1 <-spatial.select(c, g, predicate = "contains")
write_sf(u1,"E:/Glenn/Botswana/Satellite_Data/hex/hex_grids/Struizendam_2_hex_grid.shp",overwrite=TRUE )
plot (u1)
 
#Making Grid for Struizendam_3

Struizendam_3_clipper <- read_sf(dsn = 'E:/Glenn/Botswana/Final_Drone_Survey_Data/Struizendam_3', layer = "Struizendam_3_clip")
Struizendam_3_clip <- vect(Struizendam_3_clipper)

g <-st_as_sf(hex250_Grid_SF)
c<-st_as_sf(Struizendam_3_clip)
u1 <-spatial.select(c, g, predicate = "contains")
write_sf(u1,"E:/Glenn/Botswana/Satellite_Data/hex/hex_grids/Struizendam_3_hex_grid.shp",overwrite=TRUE )
plot (u1)
 
#Making Grid for Struizendam_4

Struizendam_4_clipper <- read_sf(dsn = 'E:/Glenn/Botswana/Final_Drone_Survey_Data/Struizendam_4', layer = "Struizendam_4_clip")
Struizendam_4_clip <- vect(Struizendam_4_clipper)

g <-st_as_sf(hex250_Grid_SF)
c<-st_as_sf(Struizendam_4_clip)
u1 <-spatial.select(c, g, predicate = "contains")
write_sf(u1,"E:/Glenn/Botswana/Satellite_Data/hex/hex_grids/Struizendam_4_hex_grid.shp",overwrite=TRUE )
plot (u1)
 

#Making Combined Grid for all surveys
