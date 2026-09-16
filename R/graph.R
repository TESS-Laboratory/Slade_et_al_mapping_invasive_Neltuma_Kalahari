#' The pipeline graph, generated from sensors.yml
#'
#' refactor-3.0 section 4.1: one code path for every sensor. This file turns
#' inst/config/sensors.yml into the value grids that _targets.R's tar_map()
#' calls consume, so adding a sensor, a training source or a stack is a config
#' change, not a new block of targets.
#'
#' Vocabulary
#'   sensor  drone | wv2 | planet | s2
#'   unit    what one classification covers: a drone site, or a satellite
#'           "scene"
#'   tag     a predictor stack (drone: stacks.csv tags; satellite: one)
#'   source  where the training labels come from: field | archived |
#'           purity_raw | purity_smooth
#'   task    one (sensor, unit, tag, source); every learner runs on every task

#' Read sensors.yml
#'
#' @param path location of sensors.yml
#' @return named list, one entry per sensor
read_sensors_yml <- function(path = file.path(CONFIG_DIR, "sensors.yml")) {
  assert_config_exists(path, "Sensor configuration")
  y <- yaml::read_yaml(path)
  for (s in names(y)) {
    missing <- setdiff(c("epsg", "units", "cube", "sources", "aoi", "domain",
                         "primary_source", "classes"), names(y[[s]]))
    if (length(missing)) {
      stop("sensors.yml entry '", s, "' is missing key(s): ",
           paste(missing, collapse = ", "), call. = FALSE)
    }
    if (!y[[s]]$primary_source %in% names(y[[s]]$sources)) {
      stop("sensors.yml '", s, "': primary_source '", y[[s]]$primary_source,
           "' is not one of its sources.", call. = FALSE)
    }
  }
  y
}


#' Expand `{unit}` / `{site}` placeholders in a config path
#'
#' @param template a path with `{unit}` or `{site}`
#' @param unit the unit or site id
#' @return the path
fill_path <- function(template, unit) {
  gsub("\\{(unit|site)\\}", unit, template)
}


#' Units of a sensor under the active profile
#'
#' @param sensor sensor id
#' @param cfg sensors.yml list
#' @param sites drone site ids under the profile
#' @return character vector of unit ids
sensor_units <- function(sensor, cfg, sites) {
  if (identical(cfg[[sensor]]$units, "sites")) sites else "scene"
}


#' Stack tags of a sensor
#'
#' @param sensor sensor id
#' @param cfg sensors.yml list
#' @param stack_tags the drone tags from stacks.csv
#' @return character vector
sensor_stacks <- function(sensor, cfg, stack_tags) {
  st <- cfg[[sensor]]$stacks
  if (identical(st, "stacks_csv")) stack_tags else unlist(st)
}


#' The unit x stack grid: one cube per row
#'
#' @param cfg sensors.yml list
#' @param sites drone sites under the profile
#' @param stack_tags drone stack tags
#' @return data.frame(sensor, unit, tag, cube_kind)
cube_grid <- function(cfg, sites, stack_tags) {
  rows <- lapply(names(cfg), function(s) {
    expand.grid(sensor = s, unit = sensor_units(s, cfg, sites),
                tag = sensor_stacks(s, cfg, stack_tags),
                stringsAsFactors = FALSE)
  })
  g <- do.call(rbind, rows)
  g$cube_kind <- vapply(g$sensor, function(s) cfg[[s]]$cube, "")
  g$id <- paste(g$sensor, g$unit, g$tag, sep = "_")
  g
}


#' The task grid: one training table and task per row
#'
#' Carries everything the training builder needs as plain columns - never
#' looked up inside a tar_map command (the `$<symbol>` substitution trap).
#'
#' @param cfg sensors.yml list
#' @param sites drone sites under the profile
#' @param stack_tags drone stack tags
#' @return data.frame with sensor, unit, tag, source, source_type, layer,
#'   purity, class_size, balance, epsg, ids and the cube target symbol
task_grid <- function(cfg, sites, stack_tags) {
  cg <- cube_grid(cfg, sites, stack_tags)
  rows <- lapply(seq_len(nrow(cg)), function(i) {
    s <- cg$sensor[i]; u <- cg$unit[i]; tg <- cg$tag[i]
    src <- cfg[[s]]$sources
    do.call(rbind, lapply(names(src), function(nm) {
      sp <- src[[nm]]
      type <- if (grepl("^purity", nm)) "purity" else nm
      data.frame(
        sensor = s, unit = u, tag = tg, source = nm, source_type = type,
        layer = if (!is.null(sp$layer)) fill_path(sp$layer, u) else NA_character_,
        purity = sp$purity %||% NA_real_,
        class_size = sp$class_size %||% NA_integer_,
        balance = sp$balance %||% "none",
        surface = sp$surface %||% NA_character_,
        buffer_m = sp$buffer_m %||% NA_real_,
        epsg = as.integer(cfg[[s]]$epsg),
        primary = identical(nm, cfg[[s]]$primary_source),
        stringsAsFactors = FALSE
      )
    }))
  })
  g <- do.call(rbind, rows)
  g$cube_id <- paste(g$sensor, g$unit, g$tag, sep = "_")
  g$id <- paste(g$cube_id, g$source, sep = "_")
  g$cube_sym <- rlang::syms(paste0("cube_", g$cube_id))
  # Layer symbol per source: drone field polygons are the per-site input
  # targets; a satellite field layer, an archived layer and a purity layer
  # are one target per sensor (and surface), shared by every task using them.
  g$layer_sym <- rlang::syms(ifelse(
    g$source_type == "purity", paste0("purity_layer_", g$sensor, "_", g$surface),
    ifelse(g$source_type == "field",
           ifelse(g$sensor == "drone", paste0("field_paths_", g$unit),
                  paste0("field_layer_", g$sensor)),
           paste0("layer_archived_", g$sensor))))
  g$site_label <- legacy_site_label(g$sensor, g$unit, g$source)
  g
}


#' The v2.0-compatible `site` label used by the score tables and the paper
#'
#' drone -> the site id; satellites -> sensor_source with the v2 source names,
#' so select_best(), the paper values and the v2 baseline comparison keep
#' working while the paper is rewritten (Phase E).
#'
#' @param sensor,unit,source vectors
#' @return character vector
legacy_site_label <- function(sensor, unit, source) {
  v2 <- c(archived = "archived", purity_raw = "dr_raw",
          purity_smooth = "dr_smooth", field = "field")
  ifelse(sensor == "drone", unit, paste0(sensor, "_", v2[source]))
}


#' The fit grid: every task x every learner
#'
#' @param tg the task grid
#' @param learner_ids configured learner ids
#' @return data.frame with task and spec symbols
fit_grid <- function(tg, learner_ids) {
  g <- merge(tg[, c("id", "sensor", "unit", "tag", "source", "site_label")],
             data.frame(learner_id = learner_ids, stringsAsFactors = FALSE),
             by = NULL)
  g$task_sym <- rlang::syms(paste0("task_", g$id))
  g$spec_sym <- rlang::syms(paste0("spec_", g$learner_id))
  g$fit_id <- paste(g$id, g$learner_id, sep = "_")
  g
}


#' The prediction grid: one landscape surface per unit, on the primary source
#'
#' @param tg the task grid
#' @param cfg sensors.yml list
#' @param pred_tag the drone prediction stack (prediction.yml)
#' @param tuned_ids tuned learner ids
#' @return data.frame with the symbols the prediction targets chain to
pred_grid <- function(tg, cfg, pred_tag, tuned_ids) {
  keep <- tg$primary & (tg$sensor != "drone" | tg$tag == pred_tag)
  g <- tg[keep, c("sensor", "unit", "tag", "source", "id", "site_label", "epsg"), drop = FALSE]
  rownames(g) <- NULL
  g$pred_id   <- paste(g$sensor, g$unit, sep = "_")
  g$cube_sym  <- rlang::syms(paste0("cube_", g$sensor, "_", g$unit, "_", g$tag))
  g$train_sym <- rlang::syms(paste0("train_", g$id))
  g$aoi_path  <- vapply(seq_len(nrow(g)), function(i) fill_path(cfg[[g$sensor[i]]]$aoi, g$unit[i]), "")
  g$smooth_window <- vapply(g$sensor, function(s) as.integer(cfg[[s]]$smooth_window %||% NA_integer_), 1L)
  for (id in tuned_ids) {
    g[[paste0("cfg_", id)]] <- rlang::syms(paste0("tuned_", g$id, "_", id))
  }
  g
}
