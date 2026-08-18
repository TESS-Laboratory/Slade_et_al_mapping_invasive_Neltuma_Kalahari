# Reproduction pipeline for Slade et al., mapping invasive Neltuma in the Kalahari.
#
# Run:
#   source tools/uvr-env.sh          # required: forces source builds, wires pandoc
#   NELTUMA_PROFILE=fast  R -e 'targets::tar_make()'    # smoke run, minutes
#   NELTUMA_PROFILE=full  R -e 'targets::tar_make()'    # real run, hours
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
    "mlr3extralearners",
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
  tar_target(tuned, tune_config(task_sym, spec_sym, tune_shared),
             resources = ml_resources),
  tar_target(fit, run_resample(task_sym, spec_sym, tune_shared, tuned),
             resources = ml_resources),
  tar_target(fit_tidy, tidy_resample(fit, site, tag, learner_id))
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
  tar_target(tune_shared, shared_budget(resampling)),

  per_fit,
  tar_combine(score_index, per_fit[["fit_tidy"]], command = rbind(!!!.x)),

  # Best learner per site x stack, with the margin over the runner-up and a
  # clear_win flag so wins inside the noise are not silently promoted.
  tar_target(best_models, select_best(score_index))
)
