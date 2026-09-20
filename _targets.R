# Reproduction pipeline for Slade et al., mapping invasive Neltuma in the Kalahari.
# refactor-3.0: the graph is GENERATED from inst/config/sensors.yml (R/graph.R).
#
# Run:
#   source tools/uvr-env.sh          # required: forces source builds, wires pandoc
#   NELTUMA_PROFILE=fast NELTUMA_STORE=_targets_fast \
#       R -e 'targets::tar_make(store = "_targets_fast")'
#   NELTUMA_PROFILE=full  R -e 'targets::tar_make()'
#
# GIVE THE FAST PROFILE ITS OWN STORE (store = "_targets_fast"). The profiles
# share target names but not settings. NELTUMA_STORE tells the manuscript's
# Quarto subprocess which store to read (it defaults to _targets).
#
# Design rules, from refactor-findings.md and docs/refactor-3.0-plan.md:
#   - No number that appears in the methods section is hardcoded here.
#   - Missing or malformed inputs stop the pipeline with an acquisition note.
#   - Predictor stacks are assembled on demand (VRT), never stored pre-combined.
#   - One code path for every sensor: sensor x unit x stack x source x learner.
#   - The fast profile is the default, so an accidental run costs minutes.

library(targets)
library(tarchetypes)

# ---------------------------------------------------------------------------
# COMPUTE PROFILES. Two crew controllers: "general" (cheap, numerous) and "ml"
# (fits, predictions). NELTUMA_FUTURE parallelises INSIDE tuning and
# evaluation (multisession; measured 4.4x at 8 workers, 7.24). Keep
# ML_WORKERS x NELTUMA_FUTURE within the cores left by other users.
GENERAL_WORKERS <- as.integer(Sys.getenv("NELTUMA_WORKERS", "16"))
ML_WORKERS      <- as.integer(Sys.getenv("NELTUMA_ML_WORKERS", "28"))
FUTURE_WORKERS  <- as.integer(Sys.getenv("NELTUMA_FUTURE", "1"))
if (ML_WORKERS * FUTURE_WORKERS > 64L) {
  warning("ML_WORKERS x NELTUMA_FUTURE = ", ML_WORKERS * FUTURE_WORKERS,
          " exceeds the 64 cores on this machine.", call. = FALSE)
}
# Landscape predictions fork NELTUMA_PREDICT_CORES terra workers EACH, so they
# get their own controller with a worker cap: seven concurrent predictions x
# 8 cores put the load average at 90 on 64 cores (Phase A gate, 2026-09-16).
PREDICT_WORKERS <- as.integer(Sys.getenv("NELTUMA_PREDICT_WORKERS", "3"))
PREDICT_CORES   <- as.integer(Sys.getenv("NELTUMA_PREDICT_CORES", "8"))
if (ML_WORKERS * FUTURE_WORKERS + PREDICT_WORKERS * PREDICT_CORES > 64L) {
  warning("ML + prediction cores = ", ML_WORKERS * FUTURE_WORKERS + PREDICT_WORKERS * PREDICT_CORES,
          " exceeds the 64 cores on this machine.", call. = FALSE)
}
controller_general <- crew::crew_controller_local(name = "general", workers = GENERAL_WORKERS, seconds_idle = 60)
controller_ml      <- crew::crew_controller_local(name = "ml", workers = ML_WORKERS, seconds_idle = 300)
controller_predict <- crew::crew_controller_local(name = "predict", workers = PREDICT_WORKERS, seconds_idle = 300)
ml_resources      <- tar_resources(crew = tar_resources_crew(controller = "ml"))
predict_resources <- tar_resources(crew = tar_resources_crew(controller = "predict"))

tar_option_set(
  packages = c("terra", "sf", "exactextractr",
               "mlr3", "mlr3learners", "mlr3spatiotempcv", "mlr3pipelines",
               "mlr3tuning", "mlr3tuningspaces", "mlr3filters", "paradox",
               "mlr3extralearners", "mlr3mbo",
               "dplyr", "tidyr", "purrr", "jsonlite", "yaml"),
  format = "qs",
  controller = crew::crew_controller_group(controller_general, controller_ml, controller_predict),
  resources = tar_resources(crew = tar_resources_crew(controller = "general")),
  # Rasters move between targets as file paths, never as SpatRaster objects.
  memory = "transient", garbage_collection = TRUE,
  # Timestamp-based change detection: the mirrored rasters total ~80 GB and
  # nothing edits them in place.
  trust_timestamps = TRUE
)

tar_source()

# ---------------------------------------------------------------------------
# BUILD-TIME CONSTANTS. Static branching needs the grids when the graph is
# constructed; the profile comes from NELTUMA_PROFILE.
PROFILE     <- active_profile()
SITES       <- site_ids(PROFILE)
TAGS        <- read_stacks()$tag
LEARNER_IDS <- vapply(read_resampling()$learners, function(x) x$id, character(1))
TUNED_IDS   <- vapply(Filter(function(x) isTRUE(x$tuned), read_resampling()$learners),
                      function(x) x$id, character(1))
PREDCFG     <- read_prediction()
PRED_TAG    <- PREDCFG$stack
PRED_AGG    <- if (PROFILE == "fast") as.integer(PREDCFG$fast_aggregate) else 1L
PRED_SMOOTH <- as.integer(PREDCFG$smooth_window %||% 0L)

SENSORS     <- read_sensors_yml()
SAT_SENSORS <- setdiff(names(SENSORS), "drone")
CG <- cube_grid(SENSORS, SITES, TAGS)
TG <- task_grid(SENSORS, SITES, TAGS)
FG <- fit_grid(TG, LEARNER_IDS)
PG <- pred_grid(TG, SENSORS, PRED_TAG, TUNED_IDS)
PG$window <- ifelse(PG$sensor == "drone", PRED_SMOOTH, PG$smooth_window)

# ---------------------------------------------------------------------------
# DRONE INPUTS: per-site rasters and vectors, tracked and validated.
per_site <- tar_map(
  values = list(site = SITES),
  names = site,
  tar_target(refl_path, file.path("data-in/drone", site, "refl_stack.tif"), format = "file"),
  tar_target(chm_path,  file.path("data-in/drone", site, "chm.tif"), format = "file"),
  # FlatGeobuf: one file, CRS inside (refactor-3.0 4.3)
  tar_target(field_paths, file.path("data-in/drone", site, "field_points.fgb"), format = "file"),
  tar_target(aoi_paths,   file.path("data-in/drone", site, "aoi.fgb"), format = "file"),
  tar_target(refl_check,  validate_raster(refl_path, site, expect_bands = 5L, sites = sites)),
  tar_target(chm_check,   validate_raster(chm_path, site, expect_bands = 1L, sites = sites)),
  tar_target(field_check, validate_vector(field_paths[1], site, expect_geometry = "POLYGON", sites = sites)),
  tar_target(aoi_check,   validate_vector(aoi_paths[1], site,
                                          expect_geometry = c("POLYGON", "MULTIPOLYGON"), sites = sites))
)

# ---------------------------------------------------------------------------
# CUBES. Drone: per site x stack from stacks.csv. Satellite: base + VIs
# (shipped or computed) per scene. Names: cube_<sensor>_<unit>_<tag>.
drone_cg <- CG[CG$cube_kind == "drone", c("sensor", "unit", "tag")]
drone_cg$n_bands <- read_stacks()$n_bands[match(drone_cg$tag, TAGS)]
drone_cubes <- tar_map(
  values = drone_cg, names = c("sensor", "unit", "tag"),
  tar_target(cube, { inputs_validated; build_cube(unit, tag, stacks = stacks, sites = sites) },
             format = "file"),
  tar_target(cube_info, assert_cube_grid(cube, unit, tag, expect_bands = n_bands, sites = sites))
)

sat_cg <- CG[CG$cube_kind == "satellite", c("sensor", "unit", "tag")]
sat_cubes <- tar_map(
  values = sat_cg, names = c("sensor", "unit", "tag"),
  tar_target(sat_cfg, sensors_cfg[[sensor]]),
  tar_target(base_path, file.path(sat_cfg$dir, sat_cfg$base), format = "file"),
  tar_target(vi_paths, { base_path; sat_vi_files(sat_cfg, sensor) }, format = "file"),
  tar_target(cube, { spec <- sat_cube_spec(sat_cfg, vi_paths)
                     build_satellite_cube(spec$srcs, spec$bands, sensor) },
             format = "file")
)

# ---------------------------------------------------------------------------
# TRAINING LAYERS shared across tasks.
# Archived extractions: one file target per sensor that has one.
arch <- unique(TG[TG$source_type == "archived", c("sensor", "layer")])
archived_layers <- tar_map(
  values = arch, names = sensor,
  tar_target(layer_archived, layer, format = "file")
)

# Satellite field layers: the seven sites' field polygons re-buffered to the
# manuscript's radius (section 2.5), restricted to the sensor's class roster.
fld <- unique(TG[TG$source_type == "field" & TG$sensor != "drone", c("sensor", "buffer_m")])
field_layers <- lapply(seq_len(nrow(fld)), function(i) {
  s <- fld$sensor[i]
  targets::tar_target_raw(
    paste0("field_layer_", s),
    rlang::call2("build_field_layer",
                 rlang::call2("setNames",
                              rlang::call2("list", !!!rlang::syms(paste0("field_paths_", SITES))), SITES),
                 fld$buffer_m[i],
                 rlang::call2("[[", rlang::call2("[[", quote(sensors_cfg), s), "classes"),
                 file.path("data-out", s, "train_field.fgb")),
    format = "file")
})

# Purity extraction: satellite pixel grids over each drone site x each drone
# surface (raw and smoothed), combined per sensor x surface into a layer.
pur <- unique(TG[TG$source_type == "purity", c("sensor", "surface", "purity")])
# Extraction runs against BOTH drone surfaces for every satellite sensor: the
# raw one feeds training (D4); the filtered one is kept only for the smoothing
# sensitivity analysis and the v2.0-style S10 comparison (D5).
ext_base <- data.frame(sensor = rep(SAT_SENSORS, each = 2), surface = rep(c("raw", "smoothed"), length(SAT_SENSORS)),
                       stringsAsFactors = FALSE)
ext_base$grids <- vapply(ext_base$sensor, function(s) SENSORS[[s]]$sources$purity_raw$grids, "")
ext_grid <- merge(ext_base, data.frame(site = SITES, stringsAsFactors = FALSE), by = NULL)
ext_grid$grid_path <- mapply(fill_path, ext_grid$grids, ext_grid$site, USE.NAMES = FALSE)
ext_grid$pred_sym  <- rlang::syms(ifelse(ext_grid$surface == "raw",
                                         paste0("pred_drone_", ext_grid$site),
                                         paste0("pred_smooth_drone_", ext_grid$site)))
extracts <- tar_map(
  values = ext_grid[, c("sensor", "surface", "site", "grid_path", "pred_sym")],
  names = c("sensor", "surface", "site"),
  tar_target(grid_file, grid_path, format = "file"),
  tar_target(ext, purity_extract(pred_sym, grid_file[1], site))
)
ext_combines <- lapply(seq_len(nrow(ext_base)), function(i) {
  s <- ext_base$sensor[i]; sf <- ext_base$surface[i]
  targets::tar_target_raw(
    paste0("ext_all_", s, "_", sf),
    rlang::call2("bind_rows", !!!rlang::syms(paste0("ext_", s, "_", sf, "_", SITES)), .ns = "dplyr"))
})
purity_layers <- lapply(seq_len(nrow(pur)), function(i) {
  s <- pur$sensor[i]; sf <- pur$surface[i]
  targets::tar_target_raw(
    paste0("purity_layer_", s, "_", sf),
    rlang::call2("build_purity_layer", rlang::sym(paste0("ext_all_", s, "_", sf)),
                 pur$purity[i],
                 rlang::call2("[[", rlang::call2("[[", quote(sensors_cfg), s), "classes"),
                 file.path("data-out", s, paste0("train_purity_", sf, ".fgb"))),
    format = "file")
})

# ---------------------------------------------------------------------------
# TASKS: one training table and task per (sensor, unit, tag, source).
tasks <- tar_map(
  values = TG[, c("sensor", "unit", "tag", "source", "source_type", "cube_sym",
                  "layer_sym", "balance", "class_size", "epsg",
                  "domain_sym", "domain_kind")],
  names = c("sensor", "unit", "tag", "source"),
  tar_target(train_split,
             build_source_training(source_type, cube_sym, layer_sym[1], unit, tag,
                                   classes = classes, balance = balance,
                                   class_size = class_size, seed = resampling$seed)),
  tar_target(train, train_split$training),
  tar_target(train_drops, cbind(sensor = sensor, source = source, train_split$drops)),
  tar_target(train_check, cbind(sensor = sensor, source = source,
                                validate_training_table(train, unit, sites = sites))),
  tar_target(task, make_task(train, unit, tag, sites = data.frame(site = unit, epsg = epsg))),
  # The evaluation design, built ONCE per task: kNNDM folds against the task's
  # prediction domain (the unit's AOI, or the study area), W recorded per
  # repeat. Every learner on the task gets these same folds.
  tar_target(cv, build_cv_design(task, domain_sym[1], domain_kind, eval_shared, tune_settings)),
  tar_target(cv_row, tidy_cv_design(cv, sensor, unit, tag, source))
)

# ---------------------------------------------------------------------------
# FITS: every task x every learner. Tuning once per task; the chosen
# configuration evaluated under the outer repeated spatial CV.
per_spec <- tar_map(
  values = list(learner_id = LEARNER_IDS), names = learner_id,
  tar_target(spec, learner_spec(resampling, learner_id))
)
fits <- tar_map(
  values = FG[, c("sensor", "unit", "tag", "source", "learner_id", "task_sym", "spec_sym",
                  "cv_sym", "site_label")],
  names = c("sensor", "unit", "tag", "source", "learner_id"),
  tar_target(tuned, tune_config(task_sym, spec_sym, tune_settings, cv_sym[["inner"]]),
             resources = ml_resources),
  tar_target(fit, run_resample(task_sym, spec_sym, eval_shared, tuned, cv_sym[["outer"]]),
             resources = ml_resources),
  tar_target(fit_tidy, tidy_resample(fit, site_label, tag, learner_id, sensor, unit, source)),
  tar_target(fit_class, tidy_class_accuracy(fit, site_label, tag, learner_id, neltuma_code,
                                            sensor, unit, source)),
  # Out-of-fold probabilities (the kNNDM test predictions) for the soft-vote
  # and conformal calibration; small (n_obs x n_classes).
  tar_target(fit_oof, tidy_oof(fit))
)

# Per-task soft-vote (the mapped surface's own accuracy, D16/7.39) and
# conformal calibration, over the TUNED learners' out-of-fold probabilities.
CONF_ALPHAS <- c(0.05, 0.10, 0.20)
CONF_ALPHA_MAP <- 0.10   # headline alpha for the landscape uncertainty surfaces
sv_grid <- unique(TG[, c("sensor", "unit", "tag", "source", "id", "site_label")])
for (lid in TUNED_IDS) sv_grid[[paste0("oof_", lid)]] <- rlang::syms(paste0("fit_oof_", sv_grid$id, "_", lid))
softvote <- tar_map(
  values = sv_grid[, c("sensor", "unit", "tag", "source", "site_label",
                       paste0("oof_", TUNED_IDS))],
  names = c("sensor", "unit", "tag", "source"),
  tar_target(sv, softvote_oof(list(svm = oof_svm, xgboost = oof_xgboost, ranger = oof_ranger,
                                   lightgbm = oof_lightgbm, glmnet = oof_glmnet))),
  tar_target(sv_tidy, tidy_softvote(sv, site_label, tag, sensor, unit, source, neltuma_code)),
  tar_target(sv_cal, conformal_calibrate(sv, CONF_ALPHAS, site_label, tag, sensor, unit, source)),
  tar_target(sv_honest, honest_coverage(sv, CONF_ALPHAS, neltuma_code, site_label, tag))
)

# Per-sensor score and class-accuracy tables, and the per-sensor winners.
per_sensor_scores <- unlist(lapply(names(SENSORS), function(s) {
  ids <- FG$fit_id[FG$sensor == s]
  list(
    targets::tar_target_raw(paste0("score_index_", s),
      rlang::call2("rbind", !!!rlang::syms(paste0("fit_tidy_", ids)))),
    targets::tar_target_raw(paste0("class_index_", s),
      rlang::call2("rbind", !!!rlang::syms(paste0("fit_class_", ids)))),
    targets::tar_target_raw(paste0("best_", s),
      rlang::call2("select_best", rlang::sym(paste0("score_index_", s))))
  )
}), recursive = FALSE)

# ---------------------------------------------------------------------------
# PREDICTIONS: one surface per unit on its primary source and prediction
# stack; the winner's spec and configuration are the only tuning inputs.
preds <- tar_map(
  values = PG[, c("sensor", "unit", "tag", "pred_id", "site_label", "cube_sym", "train_sym",
                  "aoi_path", "window", paste0("cfg_", TUNED_IDS))],
  names = c("sensor", "unit"),
  # D16: no winner. Every tuned learner is refitted on all of the unit's data
  # and their class probabilities are averaged with equal weights, in one pass
  # over the cube (R/predict.R). The per-learner classes ride along for the
  # sensitivity table.
  tar_target(pred,
             predict_unit_average(
               cube_sym, aoi_path, train_sym,
               specs = list(svm = spec_svm, xgboost = spec_xgboost, ranger = spec_ranger,
                            lightgbm = spec_lightgbm, glmnet = spec_glmnet),
               shared = eval_shared,
               configs = list(svm = cfg_svm, xgboost = cfg_xgboost, ranger = cfg_ranger,
                              lightgbm = cfg_lightgbm, glmnet = cfg_glmnet),
               site = pred_id, tag = tag, aggregate = PRED_AGG),
             format = "file", resources = predict_resources),
  tar_target(pred_summary, summarise_prediction(pred, pred_id, tag)),
  tar_target(pred_learner_areas, learner_area_table(pred, pred_id, tag)),
  # The modal filter is retired as a product (D5); kept as a sensitivity surface.
  tar_target(pred_smooth, smooth_prediction(pred, window, pred_id, tag),
             format = "file", resources = predict_resources),
  tar_target(smooth_areas, class_area_table(pred_smooth, pred_id, tag, "smoothed")),
  # Conformal uncertainty surfaces + Neltuma area bounds (Phase C, R1 L253/L272).
  # The unit's calibration is its MAPPED task (site_label at PRED/native tag).
  # [[ ]] not $: tar_map substitutes value symbols even inside `$` accessors,
  # so conformal_cal$tag would become conformal_cal$"<tag>" -> NULL (the
  # documented stacks$tag trap).
  tar_target(pred_conf_cal, conformal_cal[conformal_cal[["site"]] == site_label &
                                          conformal_cal[["tag"]] == tag, , drop = FALSE]),
  tar_target(pred_conformal,
             conformal_surface(pred, pred_conf_cal, CONF_ALPHA_MAP, neltuma_code, pred_id, tag),
             format = "file", resources = predict_resources),
  tar_target(pred_conf_bounds,
             conformal_area_bounds(pred, pred_conf_cal, CONF_ALPHAS, neltuma_code, pred_id, tag))
)

# ---------------------------------------------------------------------------
# COMPARISONS against the drone surfaces (Table S10 producers), per satellite
# sensor x drone site, in all four raw/smoothed combinations.
cmp_grid <- expand.grid(sensor = SAT_SENSORS, site = SITES, stringsAsFactors = FALSE)
cmp_grid$pred_sym   <- rlang::syms(paste0("pred_drone_", cmp_grid$site))
cmp_grid$smooth_sym <- rlang::syms(paste0("pred_smooth_drone_", cmp_grid$site))
cmp_grid$aoi_sym    <- rlang::syms(paste0("aoi_paths_", cmp_grid$site))
cmp_grid$sat_pred_sym   <- rlang::syms(paste0("pred_", cmp_grid$sensor, "_scene"))
cmp_grid$sat_smooth_sym <- rlang::syms(paste0("pred_smooth_", cmp_grid$sensor, "_scene"))
compares <- tar_map(
  values = cmp_grid, names = c("sensor", "site"),
  tar_target(site_areas,
             compare_site_surfaces(site, aoi_sym[1],
                                   drone = list(raw = pred_sym, smoothed = smooth_sym),
                                   wv2   = list(raw = sat_pred_sym, smoothed = sat_smooth_sym),
                                   sensor = sensor))
)
conf_grid <- data.frame(sensor = SAT_SENSORS, stringsAsFactors = FALSE)
conf_grid$ext_raw    <- rlang::syms(paste0("ext_all_", SAT_SENSORS, "_raw"))
conf_grid$ext_smooth <- rlang::syms(paste0("ext_all_", SAT_SENSORS, "_smoothed"))
conf_grid$pred_sym   <- rlang::syms(paste0("pred_", SAT_SENSORS, "_scene"))
conf_grid$smooth_sym <- rlang::syms(paste0("pred_smooth_", SAT_SENSORS, "_scene"))
confusions <- tar_map(
  values = conf_grid, names = sensor,
  tar_target(confusion_raw_raw, wv2_drone_confusion(ext_raw, pred_sym, sensors_cfg[[sensor]]$classes)),
  tar_target(confusion_smooth_smooth, wv2_drone_confusion(ext_smooth, smooth_sym, sensors_cfg[[sensor]]$classes)),
  tar_target(confusion_raw_smooth, wv2_drone_confusion(ext_raw, smooth_sym, sensors_cfg[[sensor]]$classes))
)

# Plant-scale validation (Table S9) per drone site.
s9_grid <- data.frame(site = SITES, stringsAsFactors = FALSE)
s9_grid$pred_sym   <- rlang::syms(paste0("pred_drone_", SITES))
s9_grid$smooth_sym <- rlang::syms(paste0("pred_smooth_drone_", SITES))
s9_grid$aoi_sym    <- rlang::syms(paste0("aoi_paths_", SITES))
plant_scale <- tar_map(
  values = s9_grid, names = site,
  tar_target(plant_rows, plant_scale_site(site, s9_points_path, aoi_sym[1], pred_sym, smooth_sym))
)

# ---------------------------------------------------------------------------
# SUB-PIXEL NELTUMA COVER (C2/C3): the satellites' primary Neltuma product.
# Calibrated drone P(Neltuma) warp-averaged onto each satellite grid = cover
# target; regr twins (svm dropped: worst + slowest, 2026-09-20) on leave-site-out
# folds; DI-stratified conformal (cover_di, FNN); full-scene cover raster + DI +
# within-AOA PPI area. S2 first; wv2/planet added once S2 validates in-pipeline.
COVER_IDS <- c("glmnet", "ranger", "lightgbm")
COVER_SENSORS <- c("s2", "planet")   # wv2 (175M px) added for a quiet-machine run
cover_targets <- c(
  list(targets::tar_target_raw("cover_calibrator",
    rlang::call2("drone_calibrator",
      rlang::call2("list", !!!rlang::syms(paste0("sv_drone_", SITES, "_5_CHM_ALLVI_field"))),
      quote(neltuma_code)))),
  unlist(lapply(COVER_SENSORS, function(s) {
    cube_sym  <- rlang::sym(paste0("cube_", s, "_scene_4_ALLVI"))
    bands_sym <- rlang::sym(paste0("cover_bands_", s))
    folds_sym <- rlang::sym(paste0("cover_folds_", s))
    di_sym    <- rlang::sym(paste0("cover_di_", s))
    train_sym <- rlang::sym(paste0("cover_train_", s))
    px_ha <- (SENSORS[[s]]$pixel_m^2) / 1e4
    cube_path <- rlang::call2("[", cube_sym, 1L)
    cells <- lapply(SITES, function(site) targets::tar_target_raw(
      paste0("cover_cells_", s, "_", site),
      rlang::call2("$", rlang::call2("cover_training_table",
        cube_path, rlang::call2("[", rlang::sym(paste0("pred_drone_", site)), 2L),
        file.path("data-in", s, "grids", paste0(site, ".fgb")),
        site, "4_ALLVI", quote(cover_calibrator), 1L), quote(training))))
    c(cells, list(
      targets::tar_target_raw(paste0("cover_train_", s),
        rlang::call2("rbind", !!!rlang::syms(paste0("cover_cells_", s, "_", SITES)))),
      targets::tar_target_raw(paste0("cover_bands_", s), rlang::call2("cover_bands", train_sym)),
      targets::tar_target_raw(paste0("cover_folds_", s), rlang::call2("leave_site_out_folds", train_sym)),
      targets::tar_target_raw(paste0("cover_oof_", s),
        rlang::call2("cover_run_oof", train_sym, COVER_IDS, folds_sym)),
      targets::tar_target_raw(paste0("cover_di_", s),
        rlang::call2("cover_di", train_sym, bands_sym, folds_sym)),
      targets::tar_target_raw(paste0("cover_coverage_", s),
        rlang::call2("cover_coverage_table", rlang::sym(paste0("cover_oof_", s)), di_sym, quote(CONF_ALPHAS), s)),
      targets::tar_target_raw(paste0("cover_scene_", s),
        rlang::call2("predict_cover_scene", train_sym, cube_path, bands_sym, COVER_IDS,
                     file.path("data-out", "predict", paste0(s, "_scene__4_ALLVI_cover.tif")),
                     aoi = quote(wv2_aoi)),
        format = "file"),
      targets::tar_target_raw(paste0("cover_di_raster_", s),
        rlang::call2("predict_di_raster", cube_path, bands_sym, di_sym,
                     file.path("data-out", "predict", paste0(s, "_scene__4_ALLVI_di.tif")), quote(wv2_aoi)),
        format = "file"),
      targets::tar_target_raw(paste0("cover_area_", s),
        rlang::call2("cover_scene_area",
          rlang::call2("[", rlang::sym(paste0("cover_scene_", s)), 1L),
          rlang::call2("[", rlang::sym(paste0("cover_di_raster_", s)), 1L),
          rlang::call2("$", di_sym, quote(threshold)),
          rlang::sym(paste0("cover_oof_", s)), train_sym, quote(wv2_aoi), px_ha, s))))
  }), recursive = FALSE))

# ---------------------------------------------------------------------------
list(
  # ---- configuration ------------------------------------------------------
  tar_file(sites_file,      file.path(CONFIG_DIR, "sites.csv")),
  tar_file(stacks_file,     file.path(CONFIG_DIR, "stacks.csv")),
  tar_file(sensors_file,    file.path(CONFIG_DIR, "sensors.yml")),
  tar_file(resampling_file, file.path(CONFIG_DIR, "resampling.yml")),
  tar_file(prediction_file, file.path(CONFIG_DIR, "prediction.yml")),
  tar_file(classes_file,    CLASSES_JSON),
  tar_file(manifest_file,   MANIFEST_CSV),
  tar_target(sites,       read_sites(sites_file)),
  tar_target(stacks,      read_stacks(stacks_file)),
  tar_target(sensors_cfg, read_sensors_yml(sensors_file)),
  tar_target(sensor_table, sensor_summary_table(sensors_cfg)),
  tar_target(classes,     class_lookup("field", classes_file)),
  tar_target(manifest,    read_manifest(manifest_file)),
  tar_target(resampling,  resampling_config(PROFILE, resampling_file)),
  tar_target(tune_settings, tuning_settings(resampling)),
  tar_target(eval_shared,   eval_settings(resampling)),
  tar_target(neltuma_code, { code <- classes$Type[grepl("Neltuma", classes$Class)]
                             stopifnot(length(code) == 1L); as.integer(code) }),

  # ---- inputs -------------------------------------------------------------
  tar_target(manifest_status, manifest_summary(manifest)),
  tar_target(drone_inputs_present,
             lapply(c("drone_refl_stack", "drone_chm", "drone_field_points", "drone_aoi_clip"),
                    function(id) assert_manifest_files(id, sites = SITES, manifest = manifest))),
  per_site,
  tar_combine(refl_checks,  per_site[["refl_check"]],  command = rbind(!!!.x)),
  tar_combine(chm_checks,   per_site[["chm_check"]],   command = rbind(!!!.x)),
  tar_combine(field_checks, per_site[["field_check"]], command = rbind(!!!.x)),
  tar_combine(aoi_checks,   per_site[["aoi_check"]],   command = rbind(!!!.x)),
  tar_target(inputs_validated,
             list(profile = PROFILE, sites = SITES,
                  rasters = nrow(refl_checks) + nrow(chm_checks),
                  vectors = nrow(field_checks) + nrow(aoi_checks),
                  n_field = sum(field_checks$n_features), validated = TRUE)),
  tar_target(wv2_aoi, SENSORS$wv2$aoi, format = "file"),
  tar_target(s9_points_path, SENSORS$wv2$s9_points, format = "file"),

  # ---- cubes, layers, tasks, fits -----------------------------------------
  drone_cubes,
  tar_combine(cube_index, drone_cubes[["cube_info"]], command = rbind(!!!.x)),
  sat_cubes,
  archived_layers,
  field_layers,
  extracts,
  ext_combines,
  purity_layers,
  tasks,
  tar_combine(training_attrition, tasks[["train_drops"]], command = rbind(!!!.x)),
  # W and fold structure per task: the table (and figure) R1 asked for.
  tar_combine(cv_index, tasks[["cv_row"]], command = rbind(!!!.x)),
  tar_combine(training_index_all, tasks[["train_check"]], command = rbind(!!!.x)),
  # v2.0 view: the drone field tables only (the paper's field-point counts)
  tar_target(training_index, training_index_all[training_index_all$sensor == "drone" &
                                                training_index_all$source == "field", ]),
  per_spec,
  fits,
  softvote,
  tar_combine(softvote_scores, softvote[["sv_tidy"]], command = do.call(rbind, lapply(list(!!!.x), `[[`, "score"))),
  tar_combine(softvote_classes, softvote[["sv_tidy"]], command = do.call(rbind, lapply(list(!!!.x), `[[`, "class"))),
  tar_combine(conformal_cal, softvote[["sv_cal"]], command = do.call(rbind, lapply(list(!!!.x), `[[`, "thresholds"))),
  tar_combine(conformal_apparent, softvote[["sv_cal"]], command = do.call(rbind, lapply(list(!!!.x), `[[`, "apparent"))),
  tar_combine(conformal_coverage_honest, softvote[["sv_honest"]], command = rbind(!!!.x)),
  per_sensor_scores,
  # Reporting views (nothing upstream of a prediction may read these).
  targets::tar_target_raw("score_index_all",
    rlang::call2("rbind", rlang::call2("rbind", !!!rlang::syms(paste0("score_index_", names(SENSORS)))), quote(softvote_scores))),
  targets::tar_target_raw("class_index_all",
    rlang::call2("rbind", rlang::call2("rbind", !!!rlang::syms(paste0("class_index_", names(SENSORS)))), quote(softvote_classes))),
  targets::tar_target_raw("best_all", rlang::call2("rbind", !!!rlang::syms(paste0("best_", names(SENSORS))))),

  # v2.0-compatible views, consumed by the figures and the paper until Phase E
  # score_index (figure view) includes the soft-vote 'average' rows so Fig 4
  # shows the mapped surface beside the learners.
  tar_target(score_index, rbind(score_index_drone,
                                softvote_scores[softvote_scores$sensor == "drone", ])),
  tar_target(class_index, class_index_drone),
  tar_target(best_models, best_drone),
  tar_target(wv2_scores,  score_index_wv2),
  tar_target(sat_scores,  rbind(score_index_planet, score_index_s2)),
  tar_target(wv2_class_index, class_index_wv2),
  tar_target(sat_class_index, rbind(class_index_planet, class_index_s2)),
  tar_target(wv2_best, best_wv2),
  tar_target(sat_best, rbind(best_planet, best_s2)),

  # ---- predictions and their accounting -----------------------------------
  preds,
  tar_combine(pred_index, preds[["pred_summary"]], command = rbind(!!!.x)),
  # Sensitivity of every class area to the learner, beside the average (7.39).
  tar_combine(learner_area_index, preds[["pred_learner_areas"]], command = rbind(!!!.x)),
  tar_combine(smooth_index, preds[["smooth_areas"]], command = rbind(!!!.x)),
  tar_combine(conformal_bounds, preds[["pred_conf_bounds"]], command = rbind(!!!.x)),
  tar_target(class_areas, pred_index[grepl("^drone_", pred_index$site), ]),
  tar_target(class_areas_smooth, smooth_index[grepl("^drone_", smooth_index$site), ]),
  tar_target(area_comparison, compare_areas(class_areas, class_areas_smooth)),
  tar_target(wv2_pred_summary, pred_index[pred_index$site == "wv2_scene", ]),
  tar_target(wv2_smooth_areas, smooth_index[smooth_index$site == "wv2_scene", ]),
  tar_target(sat_pred_index, pred_index[pred_index$site %in% c("planet_scene", "s2_scene"), ]),

  compares,
  tar_combine(drone_areas_all, compares[["site_areas"]], command = rbind(!!!.x)),
  tar_target(wv2_drone_areas, drone_areas_all[drone_areas_all$sensor %in% c("drone", "wv2") &
                                              drone_areas_all$site %in% SITES, ]),
  tar_target(sat_drone_areas, drone_areas_all),
  confusions,
  tar_target(wv2_confusion_raw_raw, confusion_raw_raw_wv2),
  tar_target(wv2_confusion_smooth_smooth, confusion_smooth_smooth_wv2),
  # PPI (D6): correct each sensor's scene Neltuma area by the bias measured on
  # the drone overlap, with a CI (R1 range / R2 propagate-the-discrepancy).
  tar_target(ppi_wv2, ppi_neltuma_area(pred_wv2_scene, confusion_raw_raw_wv2, neltuma_code, "wv2")),
  tar_target(ppi_planet, ppi_neltuma_area(pred_planet_scene, confusion_raw_raw_planet, neltuma_code, "planet")),
  tar_target(ppi_s2, ppi_neltuma_area(pred_s2_scene, confusion_raw_raw_s2, neltuma_code, "s2")),
  tar_target(ppi_area, rbind(ppi_wv2, ppi_planet, ppi_s2)),
  cover_targets,
  plant_scale,
  tar_combine(plant_validation, plant_scale[["plant_rows"]], command = rbind(!!!.x)),
  tar_target(plant_validation_summary, plant_scale_summary(plant_validation)),

  # ---- invasion extent and phase (WV2; section 2.7) -----------------------
  tar_target(wv2_grid_phase,
             make_analysis_grid(wv2_aoi, sensors_cfg$wv2$phases$cell_m,
                                "data-out/wv2/hex_phase.fgb", square = FALSE), format = "file"),
  tar_target(wv2_grid_prevalence,
             make_analysis_grid(wv2_aoi, sensors_cfg$wv2$phases$prevalence_cell_m,
                                "data-out/wv2/grid_prevalence.fgb", square = TRUE), format = "file"),
  tar_target(wv2_phase_layer,
             build_phase_layer(pred_wv2_scene, pred_smooth_wv2_scene, wv2_grid_phase, neltuma_code,
                               sensors_cfg$wv2$phases, "data-out/wv2/phases.fgb"), format = "file"),
  tar_target(wv2_prevalence_layer,
             build_phase_layer(pred_wv2_scene, pred_smooth_wv2_scene, wv2_grid_prevalence, neltuma_code,
                               sensors_cfg$wv2$phases, "data-out/wv2/prevalence.fgb"), format = "file"),
  tar_target(wv2_phase_table, phase_summary(wv2_phase_layer)),
  # Probabilistic phases (D7): the hard map, plus the conformal lower/upper
  # cover envelope, per 250 m hexagon -> Table 1 with ranges (R1 L272).
  tar_target(wv2_phase_conformal_layer,
             phase_conformal_layer(pred_wv2_scene, pred_conformal_wv2_scene, wv2_grid_phase,
                                   neltuma_code, sensors_cfg$wv2$phases, "data-out/wv2/phases_conformal.fgb"),
             format = "file"),
  tar_target(wv2_phase_table_conformal, phase_summary_conformal(wv2_phase_conformal_layer)),

  # ---- figures ------------------------------------------------------------
  targets::tar_target_raw("fig_maps",
    rlang::call2("fig_landscape_maps",
                 rlang::call2("setNames", rlang::call2("list", !!!rlang::syms(paste0("pred_drone_", SITES))), SITES),
                 quote(best_models), quote(PRED_TAG)), format = "file"),
  tar_target(fig_acc, fig_accuracy(best_models, score_index), format = "file"),
  tar_target(fig_cover, fig_subpixel_cover(list(wv2 = ext_all_wv2_raw, planet = ext_all_planet_raw,
                                                s2 = ext_all_s2_raw), sensor_table), format = "file"),
  tar_target(fig_wv2_map, fig_satellite_map(list(raw = pred_wv2_scene, "smoothed (w = 9)" = pred_smooth_wv2_scene),
                                            "WorldView-2 (1.6 m)", "data-out/figures/fig6c_wv2_landscape.png"),
             format = "file"),
  tar_target(fig7_scores, sensor_accuracy_summary(SITES[1], PRED_TAG, best_models, class_index,
                                                  wv2_scores, sat_scores, wv2_class_index, sat_class_index)),
  targets::tar_target_raw("fig_sensors",
    rlang::call2("fig_sensor_comparison", SITES[1],
                 rlang::call2("[", rlang::sym(paste0("aoi_paths_", SITES[1])), 1L),
                 rlang::call2("list", drone = rlang::sym(paste0("pred_drone_", SITES[1])),
                              wv2 = quote(pred_wv2_scene), planet = quote(pred_planet_scene), s2 = quote(pred_s2_scene)),
                 quote(fig7_scores)), format = "file"),
  targets::tar_target_raw("fig_study",
    rlang::call2("fig_study_area", quote(wv2_aoi),
                 rlang::call2("setNames", rlang::call2("list", !!!rlang::syms(paste0("aoi_paths_", SITES))), SITES)),
    format = "file"),
  tar_target(fig_wv2_bench, fig_wv2_benchmark(wv2_scores), format = "file"),
  tar_target(fig_phases, fig_phase_maps(wv2_prevalence_layer, wv2_phase_layer), format = "file"),
  # ---- Phase C figures ----------------------------------------------------
  tar_target(fig_conformal,
             fig_conformal_map(pred_conformal_wv2_scene, "WorldView-2 (1.6 m), 90% coverage",
                               "data-out/figures/figC1_conformal_wv2.png"), format = "file"),
  tar_target(fig_coverage, fig_coverage_curve(conformal_coverage_honest), format = "file"),
  tar_target(fig_area, fig_area_bounds(conformal_bounds, ppi_area, CONF_ALPHA_MAP), format = "file"),

  # ---- the paper ----------------------------------------------------------
  # ---- invariants (refactor-3.0 4.4) --------------------------------------
  tar_target(checks, run_checks(score_index_all, training_attrition, cube_index, resampling, sensors_cfg, cv_index)),

  tar_target(paper_values,
             build_paper_values(score_index, best_models, class_areas, training_index,
                                class_index = class_index, wv2_scores = wv2_scores, sat_scores = sat_scores,
                                sat_class_index = rbind(wv2_class_index, sat_class_index),
                                wv2_drone_areas = wv2_drone_areas,
                                wv2_confusion = wv2_confusion_smooth_smooth,
                                wv2_confusion_raw = wv2_confusion_raw_raw,
                                wv2_phase_table = wv2_phase_table,
                                plant_validation_summary = plant_validation_summary)),
  # The render runs in a Quarto subprocess that cannot see tar_make(store =);
  # the qmd reads NELTUMA_STORE, which the run command sets (see header).
  tarchetypes::tar_quarto(paper, "paper/manuscript.qmd")
)
