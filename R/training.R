#' Training table construction
#'
#' The port of `build_ml_df()`. Two copies of that function survive and they
#' disagree; the console-history copy is the correct one and the response is the
#' field `Type` code, settled from the archived confusion workbooks (finding
#' 3.3). This implements that version.
#'
#' What the original did, and this preserves:
#'   - extracts with `exact_extract(fun = "mean")` over the field POLYGONS, which
#'     are a 30 cm buffer despite being named `points` (finding 4.15)
#'   - takes centroids afterwards to obtain coordinates for spatial CV
#'   - orders by `Type` and makes it a factor with levels in that order
#'
#' What this deliberately changes, and why:
#'   - the class lookup comes from `inst/config/classes.json` via
#'     `class_lookup()`, never from the superseded xlsx, which silently breaks
#'     since a header row was added to it (findings 8.1, 8.2)
#'   - nothing is written to `<site>ML_in_Point_level.rds`. The targets store
#'     handles persistence, so the casing bug in finding 4.14 cannot recur
#'   - feature names are the clean band names from the cube (`blue`, `chm`)
#'     rather than `mean.Bokspits_1_MS_transparent_reflectance_blue`. Cosmetic:
#'     feature naming does not affect a fitted model, and the original names were
#'     site-specific, which prevented pooling sites without renaming.


#' Build the per-site training table
#'
#' @param cube_path path to the predictor cube VRT
#' @param field_path path to the field polygon layer
#' @param site site id, recorded in the output
#' @param tag stack tag, recorded in the output
#' @param classes class lookup, from `class_lookup()`
#' @return data.frame with Type (factor), site, tag, x, y and one column per band
build_training_table <- function(cube_path, field_path, site, tag,
                                 classes = class_lookup("field")) {
  cube <- terra::rast(cube_path)
  veg  <- sf::st_read(field_path, quiet = TRUE)

  if (!"Type" %in% names(veg)) {
    stop("Field layer for ", site, " has no `Type` column. Columns present: ",
         paste(setdiff(names(veg), "geometry"), collapse = ", "), "\n",
         "  `Type` is the integer class code and is the modelled response ",
         "(finding 3.3).", call. = FALSE)
  }

  geom <- as.character(unique(sf::st_geometry_type(veg)))
  if (!all(geom %in% c("POLYGON", "MULTIPOLYGON"))) {
    stop("Field layer for ", site, " is ", paste(geom, collapse = "/"),
         ", expected POLYGON.\n",
         "  The `_b30` layers are a 30 cm buffer; extracting from points ",
         "instead of the buffer changes every training value (finding 4.15).",
         call. = FALSE)
  }

  # Areal mean over the buffer, exactly as the original. Not a point sample.
  ex <- exactextractr::exact_extract(cube, veg, fun = "mean", progress = FALSE)
  ex <- as.data.frame(ex)
  names(ex) <- sub("^mean\\.", "", names(ex))
  if (!identical(names(ex), names(cube))) {
    stop("Extracted columns do not match the cube bands for ", site, "/", tag,
         ".\n  cube : ", paste(names(cube), collapse = ", "),
         "\n  got  : ", paste(names(ex), collapse = ", "), call. = FALSE)
  }

  xy <- sf::st_coordinates(sf::st_centroid(sf::st_geometry(veg)))

  out <- data.frame(
    Type = as.integer(veg$Type),
    site = site, tag = tag,
    x = xy[, 1], y = xy[, 2],
    ex, stringsAsFactors = FALSE
  )

  # Order by Type, then make it a factor with levels in that order. Level order
  # follows the original, which called arrange() before factor(levels = unique()).
  out <- out[order(out$Type), , drop = FALSE]
  rownames(out) <- NULL

  unknown <- setdiff(unique(out$Type), classes$Type)
  if (length(unknown)) {
    stop("Field layer for ", site, " uses class code(s) absent from ",
         "classes.json: ", paste(sort(unknown), collapse = ", "), "\n",
         "  Every code must be catalogued before it can be modelled ",
         "(see unresolved_codes in classes.json, finding 8.4).", call. = FALSE)
  }
  out$Type <- factor(out$Type, levels = unique(out$Type))
  out
}


#' Report and drop rows the model cannot use
#'
#' `exact_extract` returns NA where a polygon lies outside the raster's valid
#' data. Those rows cannot be fitted, but dropping them silently would hide how
#' much training data a given predictor stack actually costs — which is exactly
#' the kind of quiet attrition this refactor exists to make visible.
#'
#' @param df a training table from `build_training_table()`
#' @param bands band column names to check; defaults to all non-metadata columns
#' @return list with `data` (complete rows) and `dropped` (a summary)
drop_incomplete <- function(df, bands = NULL) {
  meta <- c("Type", "site", "tag", "x", "y")
  if (is.null(bands)) bands <- setdiff(names(df), meta)

  ok <- stats::complete.cases(df[, bands, drop = FALSE])
  dropped <- df[!ok, , drop = FALSE]

  summary <- data.frame(
    site = df$site[1], tag = df$tag[1],
    n_in = nrow(df), n_kept = sum(ok), n_dropped = sum(!ok),
    dropped_types = if (any(!ok)) {
      paste(sort(unique(as.integer(as.character(dropped$Type)))), collapse = ",")
    } else "",
    stringsAsFactors = FALSE
  )

  list(data = df[ok, , drop = FALSE], summary = summary)
}


#' Check a training table is fit to model
#'
#' @param df a training table
#' @param site site id
#' @param sites the sites table, for the expected feature count
#' @param min_per_class refuse to proceed below this many observations per class
#' @return one-row data.frame summarising the table, invisibly
validate_training_table <- function(df, site, sites = read_sites(),
                                    min_per_class = 2L) {
  s <- sites[sites$site == site, , drop = FALSE]
  problems <- character(0)

  if (nrow(df) == 0L) {
    stop("Training table for ", site, " is empty.", call. = FALSE)
  }
  tab <- table(droplevels(df$Type))
  thin <- names(tab)[tab < min_per_class]
  if (length(thin)) {
    problems <- c(problems, paste0(
      "class(es) with fewer than ", min_per_class, " observations: ",
      paste0(thin, " (n=", tab[thin], ")", collapse = ", ")))
  }
  if (any(!is.finite(df$x)) || any(!is.finite(df$y))) {
    problems <- c(problems, "non-finite centroid coordinates")
  }
  if (length(problems)) {
    stop("Training table for ", site, " is not fit to model:\n",
         paste0("    - ", problems, collapse = "\n"), "\n",
         "  Note the manuscript claims a minimum of 20 observations per class ",
         "per site (finding 1.7); Table S4 already contradicts that.",
         call. = FALSE)
  }

  invisible(data.frame(
    site = site, tag = df$tag[1],
    n = nrow(df), n_expected_polygons = as.integer(s$n_field_features),
    n_classes = length(tab),
    classes = paste(names(tab), collapse = ","),
    min_class_n = as.integer(min(tab)), max_class_n = as.integer(max(tab)),
    stringsAsFactors = FALSE
  ))
}


#' Build one task's training table from its source (refactor-3.0 4.1)
#'
#' One entry point for every (sensor, unit, tag, source) row of the task
#' grid, so the graph has a single training target family:
#'
#'   field     areal mean over the field polygons (drone: as shipped; satellite:
#'             the seven sites' polygons re-buffered, see build_field_layer),
#'             unbalanced - as the original's field arms were
#'   archived  Glen's purity extraction, balanced to the rarest class (the
#'             500-requested / 400-effective of 7.32)
#'   purity    a purity layer re-derived from our drone surfaces, balanced to
#'             the sensor's class size
#'
#' @param source_type field | archived | purity
#' @param cube_path predictor cube (VRT)
#' @param layer_path polygon layer with a Type column
#' @param unit,tag ids recorded in the table
#' @param classes class lookup
#' @param balance "min" or "none" (archived)
#' @param class_size cap for purity balancing
#' @param seed RNG seed for balancing
#' @return list(training = data.frame, drops = attrition summary)
build_source_training <- function(source_type, cube_path, layer_path, unit, tag,
                                  classes, balance = "none", class_size = NA,
                                  seed) {
  raw   <- build_training_table(cube_path, layer_path, unit, tag, classes = classes)
  split <- drop_incomplete(raw)
  tr <- split$data
  if (identical(source_type, "archived") && identical(balance, "min")) {
    tr <- balance_classes(tr, seed = seed)
  } else if (identical(source_type, "purity")) {
    tr <- balance_classes(tr, cap = if (is.na(class_size)) NULL else as.integer(class_size),
                          seed = seed)
  }
  list(training = tr, drops = split$summary)
}
