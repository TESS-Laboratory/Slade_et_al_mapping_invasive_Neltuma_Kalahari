# Reproduction pipeline for Slade et al., mapping invasive Neltuma in the Kalahari.
#
# Run:
#   source tools/uvr-env.sh          # required: forces source builds, wires pandoc
#   NELTUMA_PROFILE=fast  R -e 'targets::tar_make(store = "_targets_fast")'
#   NELTUMA_PROFILE=full  R -e 'targets::tar_make()'
#
# GIVE THE FAST PROFILE ITS OWN STORE (store = "_targets_fast"). The two
# profiles share target names but not settings, so a fast smoke test run into
# the default store overwrites the overlapping site's full-profile records with
# 3-fold versions, which the next full run must redo. tar_read() takes the same
# store argument.
#
# Design rules, from refactor-findings.md:
#   - No number that appears in the methods section is hardcoded here. Everything
#     comes from inst/config/ via R/config.R.
#   - Missing or malformed inputs stop the pipeline with an acquisition note.
#     Nothing is substituted, skipped or half-run.
#   - Predictor stacks are assembled on demand, never stored pre-combined.
#   - The fast profile is the default, so an accidental run costs minutes.

library(targets)
library(tarchetypes)

# ---------------------------------------------------------------------------
# COMPUTE PROFILES
#
# Two crew controllers, so a target declares what kind of compute it needs:
#
#   "general"  cheap, numerous, IO-bound: config, validation, cubes, training
#              tables. Many workers, no internal parallelism.
#   "ml"       benchmarking. Fewer crew workers, each free to spawn futures via
#              NELTUMA_FUTURE, so mlr3 can parallelise inside a single task.
#
# Sizing. Total work is fixed, so wall time is bounded by cores used, not by the
# split - but the two axes do not convert cores to throughput equally well. Crew
# across independent targets is embarrassingly parallel; future inside one task
# pays coordination overhead that grows with worker count. So prefer crew where
# there are many targets, and future where there are few.
#
# With 28 benchmark targets, ML_WORKERS x FUTURE ~ 56 keeps the box busy without
# oversubscribing. With only a handful of targets (the fast profile, or later the
# landscape predictions) drop ML_WORKERS and raise FUTURE instead - one worker
# with 50 futures is the right shape when there is nothing to run alongside.
# MEASURED, 2026-08-17, not assumed. Future parallelism inside a benchmark buys
# NOTHING on this workload: a nested resample of tuned ranger on bokspits_3 took
# 248.8s with future disabled and 250.5s with 4 workers - 0.99x, i.e. 25%
# efficiency. The mlr3 book predicts exactly this when individual iterations are
# short, and ours are milliseconds. So futures stay at 1 and every core goes to
# crew, which parallelises independent targets at ~100% efficiency. Finding 7.24.
#
# Consequence: with 28 benchmark targets, 28 ML workers is the ceiling this graph
# shape can use. Splitting bench per learner would give 140 targets and let the
# whole machine work - see 7.24.
GENERAL_WORKERS <- as.integer(Sys.getenv("NELTUMA_WORKERS", "16"))
ML_WORKERS      <- as.integer(Sys.getenv("NELTUMA_ML_WORKERS", "28"))
FUTURE_WORKERS  <- as.integer(Sys.getenv("NELTUMA_FUTURE", "1"))

if (ML_WORKERS * FUTURE_WORKERS > 64L) {
  warning("ML_WORKERS x NELTUMA_FUTURE = ", ML_WORKERS * FUTURE_WORKERS,
          " exceeds the 64 cores on this machine.", call. = FALSE)
}

controller_general <- crew::crew_controller_local(
  name = "general", workers = GENERAL_WORKERS, seconds_idle = 60
)
controller_ml <- crew::crew_controller_local(
  name = "ml", workers = ML_WORKERS, seconds_idle = 300
)

# Targets ask for the ml controller explicitly; everything else gets general.
ml_resources <- tar_resources(crew = tar_resources_crew(controller = "ml"))

tar_option_set(
  packages = c(
    "terra", "sf", "exactextractr",
    "mlr3", "mlr3learners", "mlr3spatiotempcv", "mlr3pipelines",
    "mlr3tuning", "mlr3tuningspaces", "mlr3filters", "paradox",
    "mlr3extralearners", "mlr3mbo",
    "dplyr", "tidyr", "purrr", "jsonlite", "yaml"
  ),
  format = "qs",
  controller = crew::crew_controller_group(controller_general, controller_ml),
  resources = tar_resources(crew = tar_resources_crew(controller = "general")),
  # A terra SpatRaster is a pointer to an open GDAL dataset and does not survive
  # being serialised into the store and read back in another process. Rasters
  # therefore move between targets as file paths, never as objects.
  memory = "transient",
  garbage_collection = TRUE,
  # Decide whether an input file changed from its timestamp rather than by
  # hashing it. The mirrored rasters run to 2.6 GB each and total ~64 GB;
  # content-hashing them on every tar_make would dominate runtime. Safe here
  # because nothing edits these files in place - they arrive by mirror or by
  # git, both of which move the mtime.
  trust_timestamps = TRUE
)

tar_source()

# Static branching needs the site list when the graph is constructed, so this is
# read here rather than as a target. The profile comes from NELTUMA_PROFILE, so
# switching profile rebuilds the graph rather than mutating it mid-run.
PROFILE <- active_profile()
SITES   <- site_ids(PROFILE)
TAGS    <- read_stacks()$tag
LEARNER_IDS <- vapply(read_resampling()$learners, function(x) x$id, character(1))
TUNED_IDS   <- vapply(Filter(function(x) isTRUE(x$tuned), read_resampling()$learners),
                      function(x) x$id, character(1))
PRED_TAG <- read_resampling()$prediction$stack
PRED_AGG <- if (PROFILE == "fast") as.integer(read_resampling()$prediction$fast_aggregate) else 1L
`%||%` <- function(a, b) if (is.null(a)) b else a
PRED_SMOOTH <- as.integer(read_resampling()$prediction$smooth_window %||% 0L)
# The WV2 benchmark task: four corrected reflectance bands + all five VIs,
# named by analogy with the drone tags (there is no CHM at satellite scale).
WV2_TAG <- "4_ALLVI"

# Per-site input paths, checks, and eventually cubes and models. tar_map is used
# in preference to dynamic branching because the sites are known up front: it
# gives one named target per site (refl_check_bokspits_1 and so on), which is
# addressable in tar_read, visible in tar_visnetwork, and names the offending
# site directly when something fails.
per_site <- tar_map(
  values = list(site = SITES),
  names = site,

  # Inputs tracked as files so a changed raster invalidates only that site.
  # Timestamp-based change detection comes from trust_timestamps above.
  tar_target(refl_path, file.path("data-in/drone", site, "refl_stack.tif"),
             format = "file"),
  tar_target(chm_path,  file.path("data-in/drone", site, "chm.tif"),
             format = "file"),

  # Shapefiles are file sets: track the sidecars too, or a changed .dbf or a
  # vanished .prj goes unnoticed (finding 7.13).
  tar_target(field_paths,
             shapefile_files(file.path("data-in/drone", site, "field_points.shp")),
             format = "file"),
  tar_target(aoi_paths,
             shapefile_files(file.path("data-in/drone", site, "aoi.shp")),
             format = "file"),

  # Validation. Each fails loudly, naming this site.
  tar_target(refl_check,
             validate_raster(refl_path, site, expect_bands = 5L, sites = sites)),
  tar_target(chm_check,
             validate_raster(chm_path, site, expect_bands = 1L, sites = sites)),
  tar_target(field_check,
             validate_vector(field_paths[1], site,
                             expect_geometry = "POLYGON", sites = sites)),
  tar_target(aoi_check,
             validate_vector(aoi_paths[1], site,
                             expect_geometry = c("POLYGON", "MULTIPOLYGON"),
                             sites = sites))
)

# Predictor cubes: every site x every stack tag. Cheap - a VRT is a few kB - so
# there is no reason to restrict which combinations exist.
cube_grid <- expand.grid(site = SITES, tag = TAGS, stringsAsFactors = FALSE)
# Carry n_bands through the values grid rather than looking it up in the command.
# tar_map substitutes its value symbols throughout the expression, INCLUDING
# inside `stacks$tag`, which silently becomes stacks$"5_CHM_NDVI" -> NULL. Any
# `$<symbol>` accessor inside a tar_map command is a trap for the same reason.
cube_grid$n_bands <- read_stacks()$n_bands[match(cube_grid$tag, TAGS)]
cube_grid$field_path <- file.path("data-in/drone", cube_grid$site, "field_points.shp")

per_cube <- tar_map(
  values = cube_grid,
  names = c("site", "tag"),
  tar_target(
    cube,
    {
      inputs_validated          # gate: no cube is assembled on unvalidated inputs
      build_cube(site, tag, stacks = stacks, sites = sites)
    },
    format = "file"
  ),
  tar_target(cube_info, assert_cube_grid(cube, site, tag,
                                         expect_bands = n_bands, sites = sites)),

  # Training table: areal mean over the buffered field polygons, response = Type.
  # Depends on inputs_validated, which carries the shapefile file-tracking, so a
  # changed .dbf or .prj propagates here.
  tar_target(
    training_raw,
    {
      inputs_validated
      build_training_table(cube, field_path, site, tag, classes = classes)
    }
  ),
  tar_target(training_split, drop_incomplete(training_raw)),
  tar_target(training,       training_split$data),
  tar_target(training_drops, training_split$summary),
  tar_target(training_check, validate_training_table(training, site, sites = sites)),

  # The task is built here; fitting happens in per_fit below, one target per
  # learner, so crew schedules 140 units instead of 28.
  tar_target(task, make_task(training, site, tag, sites = sites))
)

# One fit per (site, stack, learner): 7 x 4 x 5 = 140 independent targets, which
# is what lets crew use the whole machine at ~100% efficiency instead of leaning
# on future's ~55% (finding 7.24). Each fit references its task target by symbol
# - the standard pattern for chaining tar_map blocks.
# Each fit depends on its OWN learner's spec plus the shared budget, not on the
# whole config: targets invalidates on upstream VALUE, so a per-learner spec
# target that comes back unchanged leaves that learner's fits alone. The first
# full run lost 112 banked fits to editing one learner's entry; this decoupling
# is why that cannot recur.
per_spec <- tar_map(
  values = list(learner_id = LEARNER_IDS),
  names = learner_id,
  tar_target(spec, learner_spec(resampling, learner_id))
)

fit_grid <- expand.grid(site = SITES, tag = TAGS, learner_id = LEARNER_IDS,
                        stringsAsFactors = FALSE)
fit_grid$task_sym <- rlang::syms(paste0("task_", fit_grid$site, "_", fit_grid$tag))
fit_grid$spec_sym <- rlang::syms(paste0("spec_", fit_grid$learner_id))

per_fit <- tar_map(
  values = fit_grid[, c("site", "tag", "learner_id", "task_sym", "spec_sym")],
  names = c("site", "tag", "learner_id"),
  # Tuning runs ONCE per (site, stack, learner); the fixed configuration is then
  # evaluated with the repeated outer CV. ~350 fits per tuned target instead of
  # the nested design's 25,100. See tune_config() for the trade-off.
  tar_target(tuned, tune_config(task_sym, spec_sym, tune_settings),
             resources = ml_resources),
  tar_target(fit, run_resample(task_sym, spec_sym, eval_shared, tuned),
             resources = ml_resources),
  tar_target(fit_tidy, tidy_resample(fit, site, tag, learner_id)),
  tar_target(fit_class, tidy_class_accuracy(fit, site, tag, learner_id, neltuma_code))
)

# Landscape prediction: one surface per site, winning learner for PRED_TAG,
# retrained on all training data with its tuned configuration. The tuned configs
# for every tuned learner are passed as a named list because the winner is only
# known at runtime (best_models); untuned winners look up NULL, which is correct.
pred_grid <- data.frame(site = SITES, stringsAsFactors = FALSE)
pred_grid$cube_sym  <- rlang::syms(paste0("cube_", SITES, "_", PRED_TAG))
pred_grid$train_sym <- rlang::syms(paste0("training_", SITES, "_", PRED_TAG))
pred_grid$aoi_sym   <- rlang::syms(paste0("aoi_paths_", SITES))
for (id in TUNED_IDS) {
  pred_grid[[paste0("cfg_", id)]] <- rlang::syms(paste0("tuned_", SITES, "_", PRED_TAG, "_", id))
}

per_pred <- tar_map(
  values = pred_grid,
  names = site,
  # A per-site best row, so a change to best_models (e.g. a new learner joining
  # the pool) invalidates only the predictions whose winner actually changed.
  # [["site"]] not $site: tar_map substitutes its value symbols even inside `$`
  # accessors (the stacks$tag trap), and `site` is one of them here.
  tar_target(best_row,
             best_models[best_models[["site"]] == site &
                         best_models[["tag"]] == PRED_TAG, , drop = FALSE]),
  tar_target(
    pred,
    predict_site(
      cube_sym, aoi_sym[1], train_sym,
      best = best_row,
      resampling = resampling,
      tuned_configs = list(svm = cfg_svm, xgboost = cfg_xgboost,
                           ranger = cfg_ranger, lightgbm = cfg_lightgbm,
                           glmnet = cfg_glmnet),
      site = site, tag = PRED_TAG, aggregate = PRED_AGG
    ),
    format = "file", resources = ml_resources
  ),
  tar_target(pred_summary, summarise_prediction(pred, site, PRED_TAG)),

  # The explicit modal smoothing step (finding 1.6 done honestly), plus its
  # area accounting so the filter's effect is measured rather than assumed.
  tar_target(pred_smooth,
             smooth_prediction(pred, PRED_SMOOTH, site, PRED_TAG),
             format = "file", resources = ml_resources),
  tar_target(smooth_areas, class_area_table(pred_smooth, site, PRED_TAG, "smoothed"))
)

# Satellite arm, WV2 first. Three training arms share the same cube, budget
# and learner roster (spec_/tune_settings/eval_shared, so a budget change
# invalidates the drone and satellite arms together):
#
#   archived    Glen's surviving extraction - the reproduction (finding 7.32)
#   dr_raw      re-derived from OUR raw drone surfaces
#   dr_smooth   re-derived from OUR smoothed drone surfaces
#
# dr_raw vs dr_smooth is finding 7.31 carried into training: the smoothing
# deletes exactly the sparse-Neltuma pixels a purity filter would admit, so
# the reference-side choice must be measured, not inherited.
WV2_ARMS <- c("archived", "dr_raw", "dr_smooth")

wv2_arm_grid <- expand.grid(arm = WV2_ARMS, learner_id = LEARNER_IDS,
                            stringsAsFactors = FALSE)
wv2_arm_grid$task_sym <- rlang::syms(paste0("wv2_task_", wv2_arm_grid$arm))
wv2_arm_grid$spec_sym <- rlang::syms(paste0("spec_", wv2_arm_grid$learner_id))

wv2_fits <- tar_map(
  values = wv2_arm_grid,
  names = c("arm", "learner_id"),
  tar_target(wv2_tuned, tune_config(task_sym, spec_sym, tune_settings),
             resources = ml_resources),
  tar_target(wv2_fit, run_resample(task_sym, spec_sym, eval_shared, wv2_tuned),
             resources = ml_resources),
  tar_target(wv2_fit_tidy,
             tidy_resample(wv2_fit, paste0("wv2_", arm), WV2_TAG, learner_id)),
  tar_target(wv2_fit_class,
             tidy_class_accuracy(wv2_fit, paste0("wv2_", arm), WV2_TAG, learner_id, neltuma_code))
)

# Per-site purity extraction against both drone surfaces, feeding the two
# re-derived arms.
wv2_ext_grid <- data.frame(site = SITES, stringsAsFactors = FALSE)
wv2_ext_grid$pred_sym   <- rlang::syms(paste0("pred_", SITES))
wv2_ext_grid$smooth_sym <- rlang::syms(paste0("pred_smooth_", SITES))
wv2_ext_grid$grid_path  <- file.path("data-in/wv2/grids", paste0(SITES, ".shp"))

wv2_extracts <- tar_map(
  values = wv2_ext_grid,
  names = site,
  tar_target(wv2_grid_files, shapefile_files(grid_path), format = "file"),
  tar_target(wv2_ext_raw,    purity_extract(pred_sym, wv2_grid_files[1], site)),
  tar_target(wv2_ext_smooth, purity_extract(smooth_sym, wv2_grid_files[1], site))
)

# The field-observations-only WV2 arm (section 3.2's 69.7%; Fig 6B), a
# fourth WV2 arm kept out of WV2_ARMS because Planet/S2 share that list.
wv2_field_fits <- tar_map(
  values = list(learner_id = LEARNER_IDS,
                spec_sym = rlang::syms(paste0("spec_", LEARNER_IDS))),
  names = learner_id,
  tar_target(wv2_field_tuned, tune_config(wv2_task_field, spec_sym, tune_settings),
             resources = ml_resources),
  tar_target(wv2_field_fit,
             run_resample(wv2_task_field, spec_sym, eval_shared, wv2_field_tuned),
             resources = ml_resources),
  tar_target(wv2_field_fit_tidy,
             tidy_resample(wv2_field_fit, "wv2_field", WV2_TAG, learner_id)),
  tar_target(wv2_field_fit_class,
             tidy_class_accuracy(wv2_field_fit, "wv2_field", WV2_TAG, learner_id, neltuma_code))
)

# Drone vs WV2 class areas per drone site - the Table S10 producer. All four
# raw/smoothed combinations, so the reference-side choice (finding 7.31) is
# explicit rather than inherited from what was on disk.
wv2_cmp_grid <- data.frame(site = SITES, stringsAsFactors = FALSE)
wv2_cmp_grid$pred_sym   <- rlang::syms(paste0("pred_", SITES))
wv2_cmp_grid$smooth_sym <- rlang::syms(paste0("pred_smooth_", SITES))
wv2_cmp_grid$aoi_sym    <- rlang::syms(paste0("aoi_paths_", SITES))

wv2_compare <- tar_map(
  values = wv2_cmp_grid,
  names = site,
  tar_target(
    wv2_site_areas,
    compare_site_surfaces(site, aoi_sym[1],
                          drone = list(raw = pred_sym, smoothed = smooth_sym),
                          wv2   = list(raw = wv2_pred, smoothed = wv2_pred_smooth))
  ),
  # Plant-scale validation rows for this site (Table S9 producer): every
  # buffered field point falling on this site's surface, majority class from
  # both the raw and smoothed classification.
  tar_target(
    plant_scale,
    plant_scale_site(site, s9_points_paths[1], aoi_sym[1], pred_sym, smooth_sym)
  )
)


# ---------------------------------------------------------------------------
# Planet and S2: the generic satellite arm, driven from satellite.yml.
# Same three training arms, prediction and S10 comparison as WV2; no phases
# (the manuscript derives those from WV2 only). Flat tar_maps over
# sensor x site and sensor x arm x learner, chained by symbol.
# ---------------------------------------------------------------------------
SAT_SENSORS <- c("planet", "s2")
SATCFG_BUILD <- read_satellite()

sat_grid <- data.frame(sensor = SAT_SENSORS, stringsAsFactors = FALSE)
sat_grid$tag       <- vapply(SAT_SENSORS, function(s) SATCFG_BUILD[[s]]$tag, "")
sat_grid$train_shp <- vapply(SAT_SENSORS, function(s)
  file.path(SATCFG_BUILD[[s]]$dir, SATCFG_BUILD[[s]]$training), "")
sat_grid$ext_raw_sym    <- rlang::syms(paste0("sat_ext_raw_all_", SAT_SENSORS))
sat_grid$ext_smooth_sym <- rlang::syms(paste0("sat_ext_smooth_all_", SAT_SENSORS))
for (id in TUNED_IDS) {
  sat_grid[[paste0("cfg_", id)]] <-
    rlang::syms(paste0("sat_tuned_", SAT_SENSORS, "_archived_", id))
}

per_sensor <- tar_map(
  values = sat_grid,
  names = sensor,
  tar_target(sat_cfg, satcfg[[sensor]]),
  tar_target(sat_base_path, file.path(sat_cfg$dir, sat_cfg$base), format = "file"),
  tar_target(sat_vi_paths,
             { sat_base_path; sat_vi_files(sat_cfg, sensor) },
             format = "file"),
  tar_target(sat_cube,
             {
               spec <- sat_cube_spec(sat_cfg, sat_vi_paths)
               build_satellite_cube(spec$srcs, spec$bands, sensor)
             },
             format = "file"),
  tar_target(sat_train_paths, shapefile_files(train_shp), format = "file"),
  tar_target(sat_sensor_row, sensors[sensors[["sensor"]] == sensor, , drop = FALSE]),

  # archived arm
  tar_target(sat_training_ext,
             build_training_table(sat_cube, sat_train_paths[1], sensor, tag,
                                  classes = classes)),
  tar_target(sat_training_archived,
             balance_classes(drop_incomplete(sat_training_ext)$data,
                             seed = resampling$seed)),
  tar_target(sat_task_archived,
             make_task(sat_training_archived, sensor, tag,
                       sites = data.frame(site = sensor, epsg = sat_cfg$epsg))),

  # re-derived arms
  tar_target(sat_layer_dr_raw,
             build_purity_layer(ext_raw_sym, sat_sensor_row$purity_threshold,
                                sat_cfg$classes,
                                file.path("data-out", sensor, "train_dr_raw.fgb")),
             format = "file"),
  tar_target(sat_layer_dr_smooth,
             build_purity_layer(ext_smooth_sym, sat_sensor_row$purity_threshold,
                                sat_cfg$classes,
                                file.path("data-out", sensor, "train_dr_smooth.fgb")),
             format = "file"),
  tar_target(sat_training_dr_raw,
             balance_classes(drop_incomplete(build_training_table(
               sat_cube, sat_layer_dr_raw, sensor, tag, classes = classes))$data,
               cap = sat_sensor_row$class_size, seed = resampling$seed)),
  tar_target(sat_training_dr_smooth,
             balance_classes(drop_incomplete(build_training_table(
               sat_cube, sat_layer_dr_smooth, sensor, tag, classes = classes))$data,
               cap = sat_sensor_row$class_size, seed = resampling$seed)),
  tar_target(sat_task_dr_raw,
             make_task(sat_training_dr_raw, sensor, tag,
                       sites = data.frame(site = sensor, epsg = sat_cfg$epsg))),
  tar_target(sat_task_dr_smooth,
             make_task(sat_training_dr_smooth, sensor, tag,
                       sites = data.frame(site = sensor, epsg = sat_cfg$epsg))),

  # prediction on the archived arm, masked to the shared study-area boundary
  tar_target(sat_best_archived,
             sat_best[sat_best[["site"]] == paste0(sensor, "_archived"), , drop = FALSE]),
  tar_target(
    sat_pred,
    predict_site(
      sat_cube, wv2_aoi, sat_training_archived,
      best = sat_best_archived, resampling = resampling,
      tuned_configs = list(svm = cfg_svm, xgboost = cfg_xgboost,
                           ranger = cfg_ranger, lightgbm = cfg_lightgbm,
                           glmnet = cfg_glmnet),
      site = sensor, tag = tag, aggregate = PRED_AGG
    ),
    format = "file", resources = ml_resources
  ),
  tar_target(sat_pred_summary, summarise_prediction(sat_pred, sensor, tag)),
  tar_target(sat_pred_smooth,
             smooth_prediction(sat_pred, sat_cfg$smooth_window, sensor, tag),
             format = "file", resources = ml_resources),
  tar_target(sat_confusion_raw_raw,
             wv2_drone_confusion(ext_raw_sym, sat_pred, sat_cfg$classes)),
  tar_target(sat_confusion_raw_smoothdrone,
             wv2_drone_confusion(ext_smooth_sym, sat_pred, sat_cfg$classes))
)

# sensor x site: purity extraction over each sensor's pixel grids, and the
# per-site area comparison against the drone surfaces.
sat_site_grid <- expand.grid(sensor = SAT_SENSORS, site = SITES,
                             stringsAsFactors = FALSE)
sat_site_grid$grid_path <- mapply(function(s, site)
  file.path(SATCFG_BUILD[[s]]$grids, paste0(site, ".shp")),
  sat_site_grid$sensor, sat_site_grid$site, USE.NAMES = FALSE)
sat_site_grid$pred_sym   <- rlang::syms(paste0("pred_", sat_site_grid$site))
sat_site_grid$smooth_sym <- rlang::syms(paste0("pred_smooth_", sat_site_grid$site))
sat_site_grid$aoi_sym    <- rlang::syms(paste0("aoi_paths_", sat_site_grid$site))
sat_site_grid$sat_pred_sym   <- rlang::syms(paste0("sat_pred_", sat_site_grid$sensor))
sat_site_grid$sat_smooth_sym <- rlang::syms(paste0("sat_pred_smooth_", sat_site_grid$sensor))

sat_sites <- tar_map(
  values = sat_site_grid,
  names = c("sensor", "site"),
  tar_target(sat_grid_files, shapefile_files(grid_path), format = "file"),
  tar_target(sat_ext_raw,    purity_extract(pred_sym, sat_grid_files[1], site)),
  tar_target(sat_ext_smooth, purity_extract(smooth_sym, sat_grid_files[1], site)),
  tar_target(sat_site_areas,
             compare_site_surfaces(site, aoi_sym[1],
                                   drone = list(raw = pred_sym, smoothed = smooth_sym),
                                   wv2   = list(raw = sat_pred_sym, smoothed = sat_smooth_sym),
                                   sensor = sensor))
)

# Per-sensor combines of the site extractions, by explicit symbol list (the
# same pattern as fig_maps) since tar_combine cannot subset a map by sensor.
sat_ext_combines <- unlist(lapply(SAT_SENSORS, function(s) lapply(c("raw", "smooth"), function(k)
  targets::tar_target_raw(
    paste0("sat_ext_", k, "_all_", s),
    rlang::call2("bind_rows", !!!rlang::syms(paste0("sat_ext_", k, "_", s, "_", SITES)),
                 .ns = "dplyr")
  ))), recursive = FALSE)

# sensor x arm x learner fits
sat_fit_grid <- expand.grid(sensor = SAT_SENSORS, arm = WV2_ARMS,
                            learner_id = LEARNER_IDS, stringsAsFactors = FALSE)
sat_fit_grid$task_sym <- rlang::syms(paste0("sat_task_", sat_fit_grid$arm, "_", sat_fit_grid$sensor))
sat_fit_grid$spec_sym <- rlang::syms(paste0("spec_", sat_fit_grid$learner_id))
sat_fit_grid$tag <- vapply(sat_fit_grid$sensor, function(s) SATCFG_BUILD[[s]]$tag, "")

sat_fits <- tar_map(
  values = sat_fit_grid,
  names = c("sensor", "arm", "learner_id"),
  tar_target(sat_tuned, tune_config(task_sym, spec_sym, tune_settings),
             resources = ml_resources),
  tar_target(sat_fit, run_resample(task_sym, spec_sym, eval_shared, sat_tuned),
             resources = ml_resources),
  tar_target(sat_fit_tidy,
             tidy_resample(sat_fit, paste0(sensor, "_", arm), tag, learner_id)),
  tar_target(sat_fit_class,
             tidy_class_accuracy(sat_fit, paste0(sensor, "_", arm), tag, learner_id, neltuma_code))
)

list(

  # ---- configuration -------------------------------------------------------
  # Config files are tracked as files so that editing one invalidates exactly
  # the targets that depend on it.

  tar_file(sites_file,      file.path(CONFIG_DIR, "sites.csv")),
  tar_file(stacks_file,     file.path(CONFIG_DIR, "stacks.csv")),
  tar_file(sensors_file,    file.path(CONFIG_DIR, "sensors.csv")),
  tar_file(resampling_file, file.path(CONFIG_DIR, "resampling.yml")),
  tar_file(classes_file,    CLASSES_JSON),
  tar_file(manifest_file,   MANIFEST_CSV),

  tar_target(sites,      read_sites(sites_file)),
  tar_target(stacks,     read_stacks(stacks_file)),
  tar_target(sensors,    read_sensors(sensors_file)),
  tar_target(classes,    class_lookup("field", classes_file)),
  tar_target(manifest,   read_manifest(manifest_file)),
  tar_target(resampling, resampling_config(PROFILE, resampling_file)),

  # ---- data validation -----------------------------------------------------

  tar_target(manifest_status, manifest_summary(manifest)),

  # Existence and availability of every drone input this run needs, checked
  # against the manifest rather than the filesystem, so that a lost input
  # produces its acquisition note.
  tar_target(
    drone_inputs_present,
    lapply(
      c("drone_refl_stack", "drone_chm", "drone_field_points", "drone_aoi_clip"),
      function(id) assert_manifest_files(id, sites = SITES, manifest = manifest)
    )
  ),

  per_site,

  # Collect the per-site checks into single tables.
  tar_combine(refl_checks,   per_site[["refl_check"]],   command = rbind(!!!.x)),
  tar_combine(chm_checks,    per_site[["chm_check"]],    command = rbind(!!!.x)),
  tar_combine(field_checks,  per_site[["field_check"]],  command = rbind(!!!.x)),
  tar_combine(aoi_checks,    per_site[["aoi_check"]],    command = rbind(!!!.x)),

  # The gate. Nothing expensive may run without depending on this.
  tar_target(
    inputs_validated,
    list(
      profile   = PROFILE,
      sites     = SITES,
      rasters   = nrow(refl_checks) + nrow(chm_checks),
      vectors   = nrow(field_checks) + nrow(aoi_checks),
      n_field   = sum(field_checks$n_features),
      validated = TRUE
    )
  ),

  # ---- predictor cubes -----------------------------------------------------

  per_cube,
  tar_combine(cube_index,     per_cube[["cube_info"]],     command = rbind(!!!.x)),
  tar_combine(training_index, per_cube[["training_check"]], command = rbind(!!!.x)),
  tar_combine(training_attrition, per_cube[["training_drops"]], command = rbind(!!!.x)),
  per_spec,
  tar_target(tune_settings, tuning_settings(resampling)),
  tar_target(eval_shared,   eval_settings(resampling)),

  per_fit,
  tar_combine(score_index, per_fit[["fit_tidy"]], command = rbind(!!!.x)),
  # Neltuma-specific accuracy per fit, kept separate from score_index so the
  # landscape predictions' best_row dependency is untouched.
  tar_combine(class_index, per_fit[["fit_class"]], command = rbind(!!!.x)),

  # Best learner per site x stack, with the margin over the runner-up and a
  # clear_win flag so wins inside the noise are not silently promoted.
  tar_target(best_models, select_best(score_index)),

  # ---- landscape prediction ----------------------------------------------
  per_pred,
  tar_combine(class_areas, per_pred[["pred_summary"]], command = rbind(!!!.x)),
  tar_combine(class_areas_smooth, per_pred[["smooth_areas"]], command = rbind(!!!.x)),
  tar_target(area_comparison, compare_areas(class_areas, class_areas_smooth)),

  # ---- satellite arm: WV2 -------------------------------------------------
  # Inputs tracked as files: the six mirrored rasters and the archived training
  # extraction with its sidecars (the .prj is present here, unlike WV2_clip).
  tar_target(wv2_raster_paths, wv2_raster_files(), format = "file"),
  tar_target(wv2_train_paths,
             shapefile_files(file.path(WV2_DIR,
                             "WV2_equal_class_size_500_train_95.shp")),
             format = "file"),

  tar_target(wv2_cube,
             build_satellite_cube(wv2_raster_paths, WV2_BANDS, "wv2"),
             format = "file"),

  # ARCHIVED ARM: feature extraction over Glen's pixel polygons, then
  # attrition accounting and the balance-to-rarest the reported run applied
  # (500 requested, 400 effective - findings 7.19/7.32).
  tar_target(wv2_training_ext,
             build_training_table(wv2_cube, wv2_train_paths[1], "wv2", WV2_TAG,
                                  classes = classes)),
  tar_target(wv2_training_split, drop_incomplete(wv2_training_ext)),
  tar_target(wv2_training_drops, wv2_training_split$summary),
  tar_target(wv2_training_archived,
             balance_classes(wv2_training_split$data, seed = resampling$seed)),
  tar_target(wv2_task_archived,
             make_task(wv2_training_archived, "wv2", WV2_TAG,
                       sites = data.frame(site = "wv2", epsg = 32734L))),

  # RE-DERIVED ARMS: our drone surfaces -> purity layers -> training tables.
  # Purity threshold and class size come from sensors.csv, the class roster
  # from satellite.yml; balancing caps at the original's class size so the
  # arms train at comparable scale.
  wv2_extracts,
  tar_combine(wv2_ext_raw_all, wv2_extracts[["wv2_ext_raw"]],
              command = dplyr::bind_rows(!!!.x)),
  tar_combine(wv2_ext_smooth_all, wv2_extracts[["wv2_ext_smooth"]],
              command = dplyr::bind_rows(!!!.x)),
  tar_target(wv2_sensor_row, sensors[sensors[["sensor"]] == "wv2", , drop = FALSE]),
  tar_target(wv2_layer_dr_raw,
             build_purity_layer(wv2_ext_raw_all,
                                purity = wv2_sensor_row$purity_threshold,
                                keep_classes = satcfg$wv2$classes,
                                out = "data-out/wv2/wv2_train_dr_raw.fgb"),
             format = "file"),
  tar_target(wv2_layer_dr_smooth,
             build_purity_layer(wv2_ext_smooth_all,
                                purity = wv2_sensor_row$purity_threshold,
                                keep_classes = satcfg$wv2$classes,
                                out = "data-out/wv2/wv2_train_dr_smooth.fgb"),
             format = "file"),
  tar_target(wv2_training_dr_raw,
             balance_classes(
               drop_incomplete(
                 build_training_table(wv2_cube, wv2_layer_dr_raw, "wv2",
                                      WV2_TAG, classes = classes))$data,
               cap = wv2_sensor_row$class_size, seed = resampling$seed)),
  tar_target(wv2_training_dr_smooth,
             balance_classes(
               drop_incomplete(
                 build_training_table(wv2_cube, wv2_layer_dr_smooth, "wv2",
                                      WV2_TAG, classes = classes))$data,
               cap = wv2_sensor_row$class_size, seed = resampling$seed)),
  tar_target(wv2_task_dr_raw,
             make_task(wv2_training_dr_raw, "wv2", WV2_TAG,
                       sites = data.frame(site = "wv2", epsg = 32734L))),
  tar_target(wv2_task_dr_smooth,
             make_task(wv2_training_dr_smooth, "wv2", WV2_TAG,
                       sites = data.frame(site = "wv2", epsg = 32734L))),

  # FIELD-ONLY ARM: the seven sites' field polygons re-buffered to the
  # manuscript's 0.8 m, features from the WV2 cube, unbalanced as the
  # original's was. The dependency list is built from SITES (fig_maps pattern).
  targets::tar_target_raw(
    "wv2_layer_field",
    rlang::call2("build_field_layer",
                 rlang::call2("setNames",
                              rlang::call2("list", !!!rlang::syms(paste0("field_paths_", SITES))),
                              SITES),
                 quote(satcfg$wv2$field_buffer_m), quote(satcfg$wv2$classes),
                 "data-out/wv2/wv2_train_field.fgb"),
    format = "file"
  ),
  tar_target(wv2_training_field,
             drop_incomplete(build_training_table(wv2_cube, wv2_layer_field, "wv2",
                                                  WV2_TAG, classes = classes))$data),
  tar_target(wv2_task_field,
             make_task(wv2_training_field, "wv2", WV2_TAG,
                       sites = data.frame(site = "wv2", epsg = 32734L))),

  wv2_fits,
  wv2_field_fits,
  tar_combine(wv2_scores, wv2_fits[["wv2_fit_tidy"]], wv2_field_fits[["wv2_field_fit_tidy"]],
              command = rbind(!!!.x)),
  tar_combine(wv2_class_index, wv2_fits[["wv2_fit_class"]], wv2_field_fits[["wv2_field_fit_class"]],
              command = rbind(!!!.x)),
  tar_target(wv2_best, select_best(wv2_scores)),
  # Prediction reproduces the reported product, so it runs on the archived arm.
  tar_target(wv2_best_archived,
             wv2_best[wv2_best[["site"]] == "wv2_archived", , drop = FALSE]),

  # Landscape prediction over the full WV2 scene, masked to the study area,
  # then the explicit majority filter at the original's ACTUAL window (9, not
  # the 25 its filenames claim - see satellite.yml).
  tar_file(satellite_file, file.path(CONFIG_DIR, "satellite.yml")),
  tar_target(satcfg, read_satellite(satellite_file)),
  tar_target(wv2_aoi_paths,
             shapefile_files(file.path(WV2_DIR, "WV2_clip.shp")),
             format = "file"),
  tar_target(wv2_aoi, fix_wv2_aoi(wv2_aoi_paths[1], satcfg$wv2$epsg),
             format = "file"),
  tar_target(
    wv2_pred,
    predict_site(
      wv2_cube, wv2_aoi, wv2_training_archived,
      best = wv2_best_archived,
      resampling = resampling,
      tuned_configs = list(svm = wv2_tuned_archived_svm,
                           xgboost = wv2_tuned_archived_xgboost,
                           ranger = wv2_tuned_archived_ranger,
                           lightgbm = wv2_tuned_archived_lightgbm,
                           glmnet = wv2_tuned_archived_glmnet),
      site = "wv2", tag = WV2_TAG, aggregate = PRED_AGG
    ),
    format = "file", resources = ml_resources
  ),
  tar_target(wv2_pred_summary, summarise_prediction(wv2_pred, "wv2", WV2_TAG)),
  tar_target(wv2_pred_smooth,
             smooth_prediction(wv2_pred, satcfg$wv2$smooth_window, "wv2", WV2_TAG),
             format = "file", resources = ml_resources),
  tar_target(wv2_smooth_areas,
             class_area_table(wv2_pred_smooth, "wv2", WV2_TAG, "smoothed")),

  wv2_compare,
  tar_combine(wv2_drone_areas, wv2_compare[["wv2_site_areas"]],
              command = rbind(!!!.x)),

  # The Table S10 matrix proper: WV2 class vs drone majority per WV2 pixel
  # inside the sites. Drone reference raw or smoothed x WV2 raw or smoothed,
  # so 7.35's "the filters roughly cancel" reading can be checked cell by cell.
  tar_target(wv2_confusion_raw_raw,
             wv2_drone_confusion(wv2_ext_raw_all, wv2_pred, satcfg$wv2$classes)),
  tar_target(wv2_confusion_smooth_smooth,
             wv2_drone_confusion(wv2_ext_smooth_all, wv2_pred_smooth,
                                 satcfg$wv2$classes)),
  tar_target(wv2_confusion_raw_smooth,
             wv2_drone_confusion(wv2_ext_raw_all, wv2_pred_smooth,
                                 satcfg$wv2$classes)),

  # Invasion extent and phase (section 2.7, Table S8/Table 1, Figure 8).
  # Cover from BOTH surfaces: the phase floor is 0.1% cover, squarely in the
  # range the modal filter erases (7.31), so the input-surface choice is
  # measured here, not assumed.
  tar_target(neltuma_code,
             {
               code <- classes$Type[grepl("Neltuma", classes$Class)]
               stopifnot(length(code) == 1L)
               as.integer(code)
             }),
  tar_target(wv2_grid_phase,
             make_analysis_grid(wv2_aoi, satcfg$wv2$phases$cell_m,
                                "data-out/wv2/hex_phase.fgb", square = FALSE),
             format = "file"),
  tar_target(wv2_grid_prevalence,
             make_analysis_grid(wv2_aoi, satcfg$wv2$phases$prevalence_cell_m,
                                "data-out/wv2/grid_prevalence.fgb",
                                square = TRUE),
             format = "file"),
  tar_target(wv2_phase_layer,
             build_phase_layer(wv2_pred, wv2_pred_smooth, wv2_grid_phase,
                               neltuma_code, satcfg$wv2$phases,
                               "data-out/wv2/phases.fgb"),
             format = "file"),
  tar_target(wv2_prevalence_layer,
             build_phase_layer(wv2_pred, wv2_pred_smooth, wv2_grid_prevalence,
                               neltuma_code, satcfg$wv2$phases,
                               "data-out/wv2/prevalence.fgb"),
             format = "file"),
  tar_target(wv2_phase_table, phase_summary(wv2_phase_layer)),

  # Plant-scale validation (Table S9). The input layer ships with the WV2
  # mirror; n(Neltuma) = 214 here vs the manuscript's 184 - recorded as
  # finding 7.33, not resolved.
  tar_target(s9_points_paths,
             shapefile_files(file.path(WV2_DIR,
                             "All_points_buffered_additional.shp")),
             format = "file"),
  tar_combine(plant_validation, wv2_compare[["plant_scale"]],
              command = rbind(!!!.x)),
  tar_target(plant_validation_summary, plant_scale_summary(plant_validation)),

  # ---- satellite arms: Planet and S2 -------------------------------------
  sat_sites,
  sat_ext_combines,
  per_sensor,
  sat_fits,
  tar_combine(sat_scores, sat_fits[["sat_fit_tidy"]], command = rbind(!!!.x)),
  tar_combine(sat_class_index, sat_fits[["sat_fit_class"]], command = rbind(!!!.x)),
  tar_target(sat_best, select_best(sat_scores)),
  tar_combine(sat_drone_areas, sat_sites[["sat_site_areas"]], command = rbind(!!!.x)),
  tar_combine(sat_pred_index, per_sensor[["sat_pred_summary"]], command = rbind(!!!.x)),

  # ---- figures -------------------------------------------------------------
  # The pred_* dependency list is built from SITES so the same code works under
  # both profiles (fast has one site; a hardcoded seven would not resolve).
  targets::tar_target_raw(
    "fig_maps",
    rlang::call2("fig_landscape_maps",
                 rlang::call2("setNames",
                              rlang::call2("list", !!!rlang::syms(paste0("pred_smooth_", SITES))),
                              SITES),
                 quote(best_models), quote(PRED_TAG)),
    format = "file"
  ),

  tar_target(fig_acc, fig_accuracy(best_models, score_index), format = "file"),
  # Figure 6C analogue: the WV2 classification of the study area, raw and at
  # the original's w=9 filter, Neltuma area in each subtitle.
  tar_target(fig_wv2_map,
             fig_satellite_map(list(raw = wv2_pred, "smoothed (w = 9)" = wv2_pred_smooth),
                               "WorldView-2 (1.6 m)",
                               "data-out/figures/fig6c_wv2_landscape.png"),
             format = "file"),
  # Figure 7 analogue: one site through four sensors, plus accuracy per
  # sensor. The site is the first in SITES so the fast profile resolves too.
  tar_target(fig7_scores,
             sensor_accuracy_summary(SITES[1], PRED_TAG, best_models, class_index,
                                     wv2_scores, sat_scores, wv2_class_index,
                                     sat_class_index)),
  targets::tar_target_raw(
    "fig_sensors",
    rlang::call2("fig_sensor_comparison", SITES[1],
                 rlang::call2("[", rlang::sym(paste0("aoi_paths_", SITES[1])), 1L),
                 rlang::call2("list", drone = rlang::sym(paste0("pred_", SITES[1])),
                              wv2 = quote(wv2_pred), planet = quote(sat_pred_planet),
                              s2 = quote(sat_pred_s2)),
                 quote(fig7_scores)),
    format = "file"
  ),
  # Figure 8 analogue: prevalence and phase maps from the phase layers.
  tar_target(fig_phases, fig_phase_maps(wv2_prevalence_layer, wv2_phase_layer),
             format = "file"),
  # Figure 5 analogue: sub-pixel Neltuma cover per sensor, from the raw-surface
  # purity extractions of all three sensors.
  tar_target(fig_cover,
             fig_subpixel_cover(list(wv2 = wv2_ext_raw_all,
                                     planet = sat_ext_raw_all_planet,
                                     s2 = sat_ext_raw_all_s2),
                                sensors),
             format = "file"),

  # ---- the paper ----------------------------------------------------------
  # The manuscript as a pipeline product: verbatim text, pipeline numbers as
  # inline expressions, contradictions flagged for authorial decisions.
  # tar_quarto scans the qmd for tar_read() calls and wires the dependencies.
  tar_target(paper_values,
             build_paper_values(score_index, best_models, class_areas, training_index,
                                class_index = class_index,
                                wv2_scores = wv2_scores, sat_scores = sat_scores,
                                sat_class_index = rbind(wv2_class_index, sat_class_index),
                                wv2_drone_areas = wv2_drone_areas,
                                wv2_confusion = wv2_confusion_smooth_smooth,
                                wv2_confusion_raw = wv2_confusion_raw_raw,
                                wv2_phase_table = wv2_phase_table,
                                plant_validation_summary = plant_validation_summary)),
  tarchetypes::tar_quarto(paper, "paper/manuscript.qmd")
)