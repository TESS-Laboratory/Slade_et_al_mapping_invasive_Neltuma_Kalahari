#' Prediction-domain adaptive evaluation: kNNDM cross-validation
#'
#' Decision D2 (2026-09-16 [HUGH]). Folds are built so that the distribution of
#' nearest-neighbour distances between test and training points matches the
#' distribution between the PREDICTION DOMAIN and the training points
#' (Linnenbrink et al. 2024, GMD 17:5897; Linnenbrink, Nowosad & Meyer 2026).
#' The mismatch is the Wasserstein statistic W, which we record for every
#' repeat - it is the quantitative statement of how much extrapolation a map
#' demands, and it is what Reviewer 1 asked to see.
#'
#' Measured on our points (finding 7.38, and the 2026-09-17 probe):
#'   - drone, domain = the site AOI: W ~ 1-3 m, near-random balanced folds;
#'   - satellites, domain = the 445 km2 study area: every fold sits inside one
#'     site, W ~ 3.7-4.3 km; k = 5 matches better than k = 10;
#'   - kNNDM with hierarchical clustering is deterministic (1-2 distinct fold
#'     sets over 5 seeds), so "repeats" would be copies. k-means clustering
#'     gives a distinct fold set per seed at equal or better W for the drone
#'     sites, so a repeat here is a separately seeded k-means kNNDM.
#'
#' CAST is called directly rather than through mlr3spatiotempcv's wrapper: the
#' wrapper hides W, insists on a SpatRaster domain, and would rebuild the folds
#' inside every fit. The design is built ONCE per task (its own target) and
#' handed to every learner, so learners see identical folds and their scores
#' are paired.

#' One kNNDM fold assignment
#'
#' @param pts sf points, the task's coordinates
#' @param domain sf polygon(s), the prediction domain, same CRS object
#' @param k number of folds
#' @param seed RNG seed for the prediction-point sample and the clustering
#' @param samplesize prediction points drawn from the domain
#' @param clustering "kmeans" or "hierarchical"
#' @return list(fold = integer vector, W = numeric)
knndm_once <- function(pts, domain, k, seed, samplesize = 2000L, clustering = "kmeans") {
  set.seed(seed)
  pp <- sf::st_sample(sf::st_union(domain), samplesize, type = "random")
  set.seed(seed)
  kn <- suppressMessages(suppressWarnings(
    CAST::knndm(pts, predpoints = pp, k = k, maxp = 0.5, clustering = clustering)))
  list(fold = as.integer(kn$clusters), W = as.numeric(kn$W))
}


#' The evaluation design for one task: outer and inner kNNDM folds
#'
#' @param task a TaskClassifST
#' @param domain_path the prediction domain (.fgb): the unit's AOI for the
#'   drone, the study area for the satellites
#' @param domain_kind "unit" or "aoi" - selects the outer fold count
#' @param eval the `final` settings (folds_unit, folds_aoi, repeats, samplesize,
#'   clustering) plus seed
#' @param tune the `tuning` settings (folds) plus seed
#' @return list(outer, inner, W_outer, W_inner, k_outer, k_inner, repeats)
#'   where outer/inner are lists of train_sets and test_sets of row ids
build_cv_design <- function(task, domain_path, domain_kind, eval, tune) {
  crs <- sf::st_crs(task$crs)
  domain <- sf::st_transform(sf::st_read(domain_path, quiet = TRUE), crs)
  pts <- sf::st_as_sf(as.data.frame(task$coordinates()), coords = c("x", "y"), crs = crs)
  ids <- task$row_ids
  f <- eval$final
  k_outer <- as.integer(if (identical(domain_kind, "unit")) f$folds_unit else f$folds_aoi)
  k_outer <- min(k_outer, max(2L, length(ids) %/% 5L))   # never folds of < ~5 points
  reps <- as.integer(f$repeats)

  sets <- function(fold) {
    ks <- sort(unique(fold))
    list(train = lapply(ks, function(j) ids[fold != j]),
         test  = lapply(ks, function(j) ids[fold == j]))
  }
  outer <- lapply(seq_len(reps), function(r)
    knndm_once(pts, domain, k_outer, eval$seed + r - 1L, f$samplesize, f$clustering))
  outer_sets <- lapply(outer, function(o) sets(o$fold))
  k_inner <- min(as.integer(tune$tuning$folds), k_outer)
  inner <- knndm_once(pts, domain, k_inner, tune$seed + 1000L, f$samplesize, f$clustering)

  list(
    outer = list(train_sets = unlist(lapply(outer_sets, `[[`, "train"), recursive = FALSE),
                 test_sets  = unlist(lapply(outer_sets, `[[`, "test"),  recursive = FALSE)),
    inner = { s <- sets(inner$fold); list(train_sets = s$train, test_sets = s$test) },
    W_outer = vapply(outer, `[[`, 1, "W"), W_inner = inner$W,
    k_outer = k_outer, k_inner = k_inner, repeats = reps, n = length(ids),
    domain_kind = domain_kind
  )
}


#' An instantiated mlr3 resampling from a stored set of folds
#'
#' @param task the task the folds were built for
#' @param folds list(train_sets, test_sets)
#' @return an instantiated ResamplingCustom
as_custom_resampling <- function(task, folds) {
  r <- mlr3::rsmp("custom")
  r$instantiate(task, train_sets = folds$train_sets, test_sets = folds$test_sets)
  r
}


#' One row describing a task's evaluation design, for the W table and figure
#'
#' @param cv output of `build_cv_design()`
#' @param sensor,unit,tag,source ids
#' @return one-row data.frame
tidy_cv_design <- function(cv, sensor, unit, tag, source) {
  data.frame(sensor = sensor, unit = unit, tag = tag, source = source,
             domain = cv$domain_kind, n = cv$n, k_outer = cv$k_outer,
             repeats = cv$repeats, iterations = length(cv$outer$test_sets),
             W_mean = mean(cv$W_outer), W_min = min(cv$W_outer), W_max = max(cv$W_outer),
             k_inner = cv$k_inner, W_inner = cv$W_inner, stringsAsFactors = FALSE)
}
