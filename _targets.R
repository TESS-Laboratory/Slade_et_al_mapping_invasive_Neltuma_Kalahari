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

tar_option_set(
  packages = c(
    "terra", "sf", "exactextractr",
    "mlr3", "mlr3learners", "mlr3spatiotempcv", "mlr3pipelines",
    "mlr3tuning", "mlr3tuningspaces", "mlr3filters",
    "dplyr", "tidyr", "purrr", "jsonlite", "yaml"
  ),
  format = "qs",
  # crew keeps the long model fits off the main process. Workers are modest by
  # default because this is a shared machine; raise with NELTUMA_WORKERS.
  controller = crew::crew_controller_local(
    workers = as.integer(Sys.getenv("NELTUMA_WORKERS", "4")),
    seconds_idle = 60
  ),
  # Rasters are passed between targets as file paths, not serialised objects: a
  # terra SpatRaster is a pointer to an open GDAL dataset and does not survive
  # being written to the targets store and read back in a different process.
  memory = "transient",
  garbage_collection = TRUE
)

tar_source()

list(

  # ---- configuration -------------------------------------------------------
  # Config files are tracked as files so that editing one invalidates exactly
  # the targets that depend on it.

  tar_target(profile, active_profile(), cue = tar_cue(mode = "always")),

  # format = "file" rather than tarchetypes::tar_file: one fewer dependency, and
  # every `uvr add` re-resolves the pinned mlr3extralearners sha against the
  # GitHub API, which rate-limits (finding 9.7).
  tar_target(sites_file,      file.path(CONFIG_DIR, "sites.csv"),      format = "file"),
  tar_target(stacks_file,     file.path(CONFIG_DIR, "stacks.csv"),     format = "file"),
  tar_target(sensors_file,    file.path(CONFIG_DIR, "sensors.csv"),    format = "file"),
  tar_target(resampling_file, file.path(CONFIG_DIR, "resampling.yml"), format = "file"),
  tar_target(classes_file,    CLASSES_JSON,                            format = "file"),
  tar_target(manifest_file,   MANIFEST_CSV,                            format = "file"),

  tar_target(sites,      read_sites(sites_file)),
  tar_target(stacks,     read_stacks(stacks_file)),
  tar_target(sensors,    read_sensors(sensors_file)),
  tar_target(classes,    class_lookup("field", classes_file)),
  tar_target(manifest,   read_manifest(manifest_file)),
  tar_target(resampling, resampling_config(profile, resampling_file)),

  # The sites this run covers. Under the fast profile this is one site.
  tar_target(active_sites, site_ids(profile, sites_file)),

  # ---- data validation -----------------------------------------------------
  # These run before anything expensive. Each fails loudly rather than letting a
  # wrong input reach a model.

  tar_target(
    manifest_status,
    manifest_summary(manifest)
  ),

  # Existence of every drone input this run needs.
  tar_target(
    drone_files_ok,
    lapply(
      c("drone_refl_stack", "drone_chm", "drone_field_points", "drone_aoi_clip"),
      function(id) tryCatch(
        assert_manifest_files(id, sites = active_sites, manifest = manifest),
        error = function(e) structure(conditionMessage(e), class = "check_failure")
      )
    )
  ),

  # Per-site raster validation: CRS, band count, pixel size.
  tar_target(
    refl_stack_valid,
    validate_raster(
      file.path("data-in/drone", active_sites, "refl_stack.tif"),
      site = active_sites, expect_bands = 5L, sites = sites
    ),
    pattern = map(active_sites)
  ),

  tar_target(
    chm_valid,
    validate_raster(
      file.path("data-in/drone", active_sites, "chm.tif"),
      site = active_sites, expect_bands = 1L, sites = sites
    ),
    pattern = map(active_sites)
  ),

  # Per-site vector validation. Geometry is checked explicitly because these are
  # POLYGON despite the `points` in their name (finding 4.15).
  tar_target(
    field_points_valid,
    validate_vector(
      file.path("data-in/drone", active_sites, "field_points.shp"),
      site = active_sites, expect_geometry = "POLYGON", sites = sites
    ),
    pattern = map(active_sites)
  ),

  # A single gate the rest of the pipeline can depend on, so that no modelling
  # target can run unless validation passed.
  tar_target(
    inputs_validated,
    {
      failed <- vapply(drone_files_ok, inherits, logical(1), "check_failure")
      if (any(failed)) {
        stop("Input validation failed:\n\n",
             paste(unlist(drone_files_ok[failed]), collapse = "\n\n"),
             call. = FALSE)
      }
      list(
        profile   = profile,
        sites     = active_sites,
        rasters   = nrow(refl_stack_valid) + nrow(chm_valid),
        vectors   = nrow(field_points_valid),
        validated = TRUE
      )
    }
  )
)
