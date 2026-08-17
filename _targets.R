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
                                         expect_bands = n_bands, sites = sites))
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
  tar_combine(cube_index, per_cube[["cube_info"]], command = rbind(!!!.x))
)
