# Test-drive kNNDM (CAST and blockCV) on our real training points, against the
# spcv_coords design we used, for two prediction situations:
#   WV2 archived task  -> prediction domain = the 445 km2 study area
#   drone bokspits_1   -> prediction domain = that site's AOI
suppressMessages({
  library(sf); library(mlr3); library(mlr3spatiotempcv); library(CAST) })
cat("CAST", as.character(packageVersion("CAST")), "
")
set.seed(5446)
run <- function(label, task, domain, k = 10) {
  xy <- task$coordinates(); pts <- st_transform(st_as_sf(data.frame(xy), coords = c("x","y"), crs = 32734), st_crs(domain))
  site <- if ("site" %in% names(task$data())) task$data()$site else NA
  t0 <- Sys.time(); kc <- CAST::knndm(pts, modeldomain = domain, k = k, maxp = 0.5, clustering = "hierarchical", samplesize = 2000)
  tc <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  kb <- NULL
  # spcv_coords for comparison: NND ECDF distance (Wasserstein-like) computed the same way as CAST
  rs <- rsmp("spcv_coords", folds = k); rs$instantiate(task)
  fold_coords <- integer(task$nrow); for (i in 1:k) fold_coords[rs$test_set(i)] <- i
  cat(sprintf("\n== %s: n=%d, k=%d ==\n", label, task$nrow, k))
  cat(sprintf("CAST::knndm    W = %.1f  (%.1fs)  fold sizes: %s\n", kc$W, tc, paste(table(kc$clusters), collapse = " ")))
  cat("spcv_coords     fold sizes:", paste(table(fold_coords), collapse = " "), "\n")
  # how do folds relate to sites (satellite case)?
  if (!all(is.na(site))) { cat("CAST folds x site (rows=fold):\n"); print(table(kc$clusters, site)) }
  invisible(list(cast = kc, block = kb, coords = fold_coords))
}
aoi <- st_read("data-out/wv2/wv2_aoi.fgb", quiet = TRUE)
wv2 <- targets::tar_read(wv2_task_archived)
# archived task rows carry no site column; recover site by nearest drone AOI
sites <- targets::tar_read(sites)
aois <- do.call(rbind, lapply(sites$site, function(s) { a <- st_read(file.path("data-in/drone", s, "aoi.shp"), quiet = TRUE); st_sf(site = s, geometry = st_union(st_geometry(a))) }))
xy <- st_as_sf(data.frame(wv2$coordinates()), coords = c("x","y"), crs = 32734)
wv2_site <- aois$site[st_nearest_feature(xy, aois)]
d <- wv2$data(); d$site <- wv2_site
wv2b <- as_task_classif_st(cbind(d, wv2$coordinates()), target = "Type", coordinate_names = c("x","y"), crs = "EPSG:32734", coords_as_features = FALSE, id = "wv2_sites")
res_wv2 <- run("WV2 archived, domain = study area", wv2b, aoi, k = 10)
res_wv2_k5 <- run("WV2 archived, domain = study area", wv2b, aoi, k = 5)
b1 <- targets::tar_read(task_bokspits_1_5_CHM_ALLVI); b1aoi <- aois[aois$site == "bokspits_1", ]
res_b1 <- run("drone bokspits_1, domain = site AOI", b1, b1aoi, k = 10)
saveRDS(list(wv2 = res_wv2, wv2_k5 = res_wv2_k5, b1 = res_b1), file.path(Sys.getenv("SCRATCH_DIR"), "knndm_results.rds"))
cat("\nDONE\n")
