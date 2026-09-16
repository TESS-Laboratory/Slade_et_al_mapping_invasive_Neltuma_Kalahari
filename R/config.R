#' Pipeline configuration accessors
#'
#' Single entry point for everything in inst/config/. Nothing else in the
#' pipeline may hardcode a site name, band list, fold count, purity threshold or
#' class size - if a number appears in the methods section, it comes from here.
#'
#' Sources of truth:
#'   inst/config/sites.csv       seven drone AOIs, read off the mirrored rasters
#'   inst/config/stacks.csv      the four predictor stacks
#'   inst/config/sensors.yml     every sensor: units, cubes, training sources (R/graph.R)
#'   inst/config/prediction.yml  landscape prediction settings
#'   inst/config/resampling.yml  folds, repeats, tuner, learners, budget
#'   inst/config/classes.json    class scheme (see R/classes.R)

CONFIG_DIR <- "inst/config"


#' Fail with a consistent message when a config file is missing
#'
#' @param path the file that should exist
#' @param what human-readable description for the error
assert_config_exists <- function(path, what) {
  if (!file.exists(path)) {
    stop(what, " not found at '", path, "'.\n",
         "  Pipeline configuration is required, not optional.\n",
         "  Run from the project root, or restore the file from git.",
         call. = FALSE)
  }
  invisible(TRUE)
}


#' The seven drone survey areas
#'
#' Every column was measured from the mirrored rasters and vectors, not asserted
#' from the manuscript. Treat as observed fact about the data on disk.
#'
#' @param path location of sites.csv
#' @return data.frame, one row per site
read_sites <- function(path = file.path(CONFIG_DIR, "sites.csv")) {
  assert_config_exists(path, "Site configuration")
  s <- utils::read.csv(path, stringsAsFactors = FALSE)

  required <- c("site", "cluster", "epsg", "pixel_m", "ncol", "nrow",
                "n_bands_refl", "n_bands_chm", "n_field_features")
  missing <- setdiff(required, names(s))
  if (length(missing)) {
    stop("sites.csv is missing column(s): ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  if (anyDuplicated(s$site)) {
    stop("sites.csv has duplicate site ids.", call. = FALSE)
  }
  if (length(unique(s$epsg)) != 1L) {
    stop("sites.csv mixes CRS: ", paste(unique(s$epsg), collapse = ", "),
         ".\n  Every site is expected to be EPSG:32734. Investigate before ",
         "proceeding - a reprojection would change every extracted value.",
         call. = FALSE)
  }
  s
}


#' Site identifiers
#'
#' @param profile "full" for all seven, "fast" for the single-site smoke run
#' @param path location of sites.csv
#' @return character vector of site ids
site_ids <- function(profile = c("full", "fast"),
                     path = file.path(CONFIG_DIR, "sites.csv")) {
  profile <- match.arg(profile)
  all_sites <- read_sites(path)$site
  if (profile == "full") return(all_sites)

  wanted <- read_resampling()$profile$fast$sites
  if (identical(wanted, "all")) return(all_sites)
  unknown <- setdiff(wanted, all_sites)
  if (length(unknown)) {
    stop("resampling.yml profile$fast$sites names unknown site(s): ",
         paste(unknown, collapse = ", "), call. = FALSE)
  }
  wanted
}


#' The four predictor stacks
#'
#' Band lists come from DRONE_STACK_BANDS in the original
#' build_cube_variants.R, which is authoritative. The `source` column describes
#' the post-split layout: stacks are assembled from independent rasters by VRT
#' as a pipeline target, never stored pre-combined (finding 5.3).
#'
#' @param path location of stacks.csv
#' @return data.frame with `bands` split into a list column
read_stacks <- function(path = file.path(CONFIG_DIR, "stacks.csv")) {
  assert_config_exists(path, "Stack configuration")
  s <- utils::read.csv(path, stringsAsFactors = FALSE)

  s$band_list <- strsplit(s$bands, "|", fixed = TRUE)
  n <- lengths(s$band_list)
  if (!all(n == s$n_bands)) {
    bad <- s$tag[n != s$n_bands]
    stop("stacks.csv declares n_bands inconsistent with the bands column for: ",
         paste(bad, collapse = ", "), call. = FALSE)
  }
  if (anyDuplicated(s$tag)) {
    stop("stacks.csv has duplicate stack tags.", call. = FALSE)
  }
  s
}



#' Resampling, tuning and compute configuration
#'
#' The single source of truth for folds, repeats, tuner and budget. Records what
#' the original code did; where the manuscript says something different, the
#' divergence is noted in the file rather than reconciled silently.
#'
#' @param path location of resampling.yml
#' @return the parsed list
read_resampling <- function(path = file.path(CONFIG_DIR, "resampling.yml")) {
  assert_config_exists(path, "Resampling configuration")
  y <- yaml::read_yaml(path)

  required <- c("seed", "tuning", "final", "learners", "predict_type", "profile")
  missing <- setdiff(required, names(y))
  if (length(missing)) {
    stop("resampling.yml is missing key(s): ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  if (!identical(y$predict_type, "prob")) {
    stop("resampling.yml sets predict_type = '", y$predict_type, "'.\n",
         "  Action item 3 requires 'prob' everywhere: the original never set it,\n",
         "  which is why no uncertainty surface exists (finding 1.5).",
         call. = FALSE)
  }
  y
}



#' Resampling settings with a profile applied
#'
#' The `fast` profile exercises the whole graph end to end on reduced budgets.
#' Nothing expensive should ever be committed before a fast pass is green.
#'
#' @param profile "fast" or "full"
#' @param path location of resampling.yml
#' @return list with tuning and final settings resolved for that profile
resampling_config <- function(profile = c("fast", "full"),
                              path = file.path(CONFIG_DIR, "resampling.yml")) {
  profile <- match.arg(profile)
  y <- read_resampling(path)
  p <- y$profile[[profile]]
  if (is.null(p)) {
    stop("resampling.yml has no profile called '", profile, "'.", call. = FALSE)
  }

  list(
    profile      = profile,
    seed         = y$seed,
    predict_type = y$predict_type,
    tuning = list(
      resampling = y$tuning$resampling,
      folds      = p$tuning_folds,
      tuner      = y$tuning$tuner,
      term_evals = p$term_evals
    ),
    final = list(
      resampling = y$final$resampling,
      folds      = p$final_folds,
      repeats    = p$final_repeats,
      iterations = p$final_folds * p$final_repeats
    ),
    learners = y$learners,
    block_cv = y$block_cv
  )
}


#' The active profile
#'
#' Set with the NELTUMA_PROFILE environment variable. Defaults to "fast" on
#' purpose: an accidental full run costs hours of compute, an accidental fast run
#' costs minutes.
#'
#' @return "fast" or "full"
active_profile <- function() {
  p <- Sys.getenv("NELTUMA_PROFILE", "fast")
  if (!p %in% c("fast", "full")) {
    stop("NELTUMA_PROFILE must be 'fast' or 'full', got '", p, "'.",
         call. = FALSE)
  }
  p
}

`%||%` <- function(a, b) if (is.null(a)) b else a


#' Landscape prediction settings
#'
#' Split from resampling.yml (refactor-3.0): the evaluation config feeds the
#' fits, the prediction config feeds the surfaces, and neither edit should
#' invalidate the other's targets.
#'
#' @param path location of prediction.yml
#' @return the parsed list
read_prediction <- function(path = file.path(CONFIG_DIR, "prediction.yml")) {
  assert_config_exists(path, "Prediction configuration")
  y <- yaml::read_yaml(path)
  missing <- setdiff(c("stack", "write_prob", "fast_aggregate", "smooth_window"), names(y))
  if (length(missing)) {
    stop("prediction.yml is missing key(s): ", paste(missing, collapse = ", "), call. = FALSE)
  }
  y
}
