setwd("/raid/home/hg499/Slade_et_al_mapping_invasive_Neltuma_Kalahari")
suppressMessages({library(osmdata); library(sf); library(terra)})
dir.create("data-in/osm", showWarnings = FALSE, recursive = TRUE)
# study-area bbox with a small pad (lon/lat)
bb <- c(20.60, -26.93, 20.79, -26.62)
roads <- osmdata_sf(add_osm_feature(opq(bb, timeout = 180), "highway"))
rl <- roads$osm_lines
keep <- intersect(c("osm_id", "highway", "name"), names(rl))
rl32 <- st_transform(rl[, keep], 32734)
st_write(rl32, "data-in/osm/roads.fgb", delete_dsn = TRUE, quiet = TRUE)
places <- osmdata_sf(add_osm_feature(opq(bb, timeout = 180), "place",
                                     c("city", "town", "village", "hamlet", "suburb")))
pp <- places$osm_points
keepp <- intersect(c("osm_id", "place", "name"), names(pp))
pp32 <- st_transform(pp[, keepp], 32734)
st_write(pp32, "data-in/osm/settlements.fgb", delete_dsn = TRUE, quiet = TRUE)
cat("ROADS:", nrow(rl32), " types:\n"); print(sort(table(rl32$highway), decreasing = TRUE))
cat("\nSETTLEMENTS:", nrow(pp32), "\n"); print(pp32$name[!is.na(pp32$name)])
cat("OSM_DONE\n")
