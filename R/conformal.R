#' Split-conformal prediction for the multi-class classifier (Phase C)
#'
#' refactor-3.0 sections 3.1-3.2. Turns the averaged class probabilities into
#' PREDICTION SETS with a finite-sample coverage guarantee, calibrated on the
#' out-of-fold probabilities the kNNDM evaluation already produces - so the
#' calibration data is spatially held out by construction, and "coverage" means
#' "a new fold of this landscape" (the deployment claim).
#'
#' Decisions D1 [HUGH]: LAC score (least-ambiguous, smallest sets at nominal
#' coverage) with MONDRIAN (per-class) calibration, so Neltuma's own coverage is
#' guaranteed rather than met on average by covering the common classes. APS is
#' reported alongside for conditional coverage (aps_scores below).
#'
#' Nothing in mlr3 does Mondrian split conformal for classification (the R
#' Journal 2026 survey confirms the gap; learner_pi_cvplus is regression-only),
#' so the maths is ours and kept small and auditable.
#'
#' The recipe, for reference:
#'   score   s_i   = 1 - p_i[y_i]                       (LAC)
#'   per class c:   q_c = the ceil((n_c + 1)(1 - alpha)) / n_c empirical
#'                        quantile of { s_i : y_i = c }
#'   set(x)        = { c : 1 - p_x[c] <= q_c }  =  { c : p_x[c] >= 1 - q_c }

#' LAC nonconformity scores
#'
#' @param prob n x k probability matrix, columns named by class label
#' @param truth length-n factor/character of true labels
#' @return length-n numeric, s_i = 1 - p_i[y_i]
lac_scores <- function(prob, truth) {
  j <- match(as.character(truth), colnames(prob))
  if (anyNA(j)) stop("truth has labels absent from the probability columns.", call. = FALSE)
  1 - prob[cbind(seq_len(nrow(prob)), j)]
}


#' APS (adaptive prediction set) nonconformity scores
#'
#' The cumulative probability of classes at least as likely as the true one -
#' reported for conditional coverage, not used as the primary set.
#'
#' @param prob n x k probability matrix, columns named by class label
#' @param truth true labels
#' @return length-n numeric
aps_scores <- function(prob, truth) {
  j <- match(as.character(truth), colnames(prob))
  vapply(seq_len(nrow(prob)), function(i) {
    p <- prob[i, ]
    sum(p[p >= p[j[i]]])
  }, numeric(1))
}


#' Mondrian (per-class) conformal thresholds
#'
#' One threshold per class from that class's own calibration scores, at the
#' finite-sample-valid quantile rank. A class with too few calibration points
#' to reach the rank gets Inf (it can be ruled out nowhere) - honest, and the
#' returned `n` makes the scarcity visible.
#'
#' @param prob calibration probability matrix (out-of-fold)
#' @param truth calibration labels
#' @param alpha miscoverage level (0.1 = 90% coverage)
#' @param score_fn lac_scores (default) or aps_scores
#' @return data.frame: class, q (threshold), n (calibration points)
mondrian_thresholds <- function(prob, truth, alpha, score_fn = lac_scores) {
  s <- score_fn(prob, truth)
  classes <- colnames(prob)
  truth_c <- as.character(truth)
  do.call(rbind, lapply(classes, function(c) {
    sc <- sort(s[truth_c == c])
    n <- length(sc)
    k <- ceiling((n + 1) * (1 - alpha))
    q <- if (n == 0 || k > n) Inf else sc[k]
    data.frame(class = c, q = q, n = n, stringsAsFactors = FALSE)
  }))
}


#' Prediction sets from a probability matrix and Mondrian thresholds
#'
#' @param prob n x k probability matrix, columns named by class
#' @param thresholds output of `mondrian_thresholds()`
#' @param score_fn the same score used to calibrate (LAC by default)
#' @return logical n x k matrix, TRUE where the class is in the set
prediction_sets <- function(prob, thresholds, score_fn = lac_scores) {
  classes <- colnames(prob)
  q <- thresholds$q[match(classes, thresholds$class)]
  if (identical(score_fn, lac_scores)) {
    scores <- 1 - prob                                  # per-class LAC score
    sweep(scores, 2, q, FUN = function(a, b) a <= b)
  } else {
    stop("prediction_sets currently supports LAC scoring only.", call. = FALSE)
  }
}


#' Empirical coverage of prediction sets, overall and per class
#'
#' @param prob probability matrix (a HELD-OUT set, for honest coverage)
#' @param truth true labels
#' @param thresholds Mondrian thresholds to apply
#' @return list(overall, by_class = data.frame(class, coverage, n, mean_set_size))
conformal_coverage <- function(prob, truth, thresholds) {
  sets <- prediction_sets(prob, thresholds)
  classes <- colnames(prob)
  truth_c <- as.character(truth)
  j <- match(truth_c, classes)
  covered <- sets[cbind(seq_len(nrow(prob)), j)]
  set_size <- rowSums(sets)
  by_class <- do.call(rbind, lapply(classes, function(c) {
    idx <- truth_c == c
    data.frame(class = c, coverage = if (any(idx)) mean(covered[idx]) else NA_real_,
               n = sum(idx), mean_set_size = if (any(idx)) mean(set_size[idx]) else NA_real_,
               stringsAsFactors = FALSE)
  }))
  list(overall = mean(covered), mean_set_size = mean(set_size), by_class = by_class)
}


#' Soft-vote out-of-fold probabilities: the equal-weight average across learners
#'
#' Aligns each learner's OOF probability matrix by row id (the learners share
#' the kNNDM folds, so the ids match) and averages. This is the cross-validated
#' estimate of the surface the pipeline actually maps (D16), and it is both the
#' thing whose accuracy Fig 4 should report and the calibration data for
#' conformal.
#'
#' @param oof list of per-learner OOF objects, each list(row_ids, prob, truth)
#' @return list(row_ids, prob, truth) for the averaged model
softvote_oof <- function(oof) {
  ids <- oof[[1]]$row_ids
  classes <- colnames(oof[[1]]$prob)
  acc <- matrix(0, nrow = length(ids), ncol = length(classes), dimnames = list(NULL, classes))
  for (o in oof) {
    ord <- match(ids, o$row_ids)
    if (anyNA(ord)) stop("learners disagree on row ids; kNNDM folds should be shared.", call. = FALSE)
    acc <- acc + o$prob[ord, classes, drop = FALSE]
  }
  list(row_ids = ids, prob = acc / length(oof), truth = oof[[1]]$truth)
}


#' One-row score for the soft-vote model, matching tidy_resample's columns
#'
#' The averaged surface's OWN accuracy and Neltuma recall (finding note: Fig 4
#' should place this beside the per-learner rows - it is what is mapped).
#'
#' @param sv output of `softvote_oof()`
#' @param site,tag ids; sensor,unit,source metadata
#' @param neltuma_code Neltuma class code, for recall
#' @return list(score, class): a score row (tidy_resample columns) and a
#'   Neltuma class-accuracy row (tidy_class_accuracy columns), both with
#'   learner = "average", so they slot into score_index_all / class_index_all.
tidy_softvote <- function(sv, site, tag, sensor = NA_character_, unit = NA_character_,
                          source = NA_character_, neltuma_code = 1L) {
  classes <- colnames(sv$prob)
  pred <- classes[max.col(sv$prob, ties.method = "first")]
  tk <- as.character(sv$truth)
  acc <- mean(pred == tk)
  nk <- as.character(neltuma_code)
  tp <- sum(pred == nk & tk == nk); n_truth <- sum(tk == nk); n_pred <- sum(pred == nk)
  score <- data.frame(
    sensor = sensor, unit = unit, source = source,
    site = site, tag = tag, learner = "average",
    classif.acc = acc, classif.ce = 1 - acc,
    acc_sd = NA_real_, acc_min = NA_real_, acc_max = NA_real_,
    n_iters = NA_integer_, stringsAsFactors = FALSE)
  class <- data.frame(
    sensor = sensor, unit = unit, source = source,
    site = site, tag = tag, learner = "average", class = as.integer(neltuma_code),
    recall = if (n_truth) tp / n_truth else NA_real_,
    precision = if (n_pred) tp / n_pred else NA_real_,
    n_truth = as.integer(n_truth), stringsAsFactors = FALSE)
  list(score = score, class = class)
}


#' Extract out-of-fold probabilities from a ResampleResult
#'
#' The kNNDM outer folds predict every training observation exactly once (when
#' it is in a test fold). With repeats, an observation is predicted several
#' times; the probabilities are averaged per row id so the result is one OOF
#' probability vector per observation. This is the calibration data for
#' conformal and the input to the soft-vote.
#'
#' @param rr a mlr3 ResampleResult (predict_type = "prob")
#' @return list(row_ids, prob = n x k matrix, truth = factor)
tidy_oof <- function(rr) {
  p <- rr$prediction()
  prob <- p$prob
  ids <- p$row_ids
  # average duplicate predictions (repeats) per observation
  uid <- sort(unique(ids))
  if (length(uid) < length(ids)) {
    agg <- rowsum(prob, group = ids) / as.vector(table(ids))
    prob <- agg[as.character(uid), , drop = FALSE]
    truth <- p$truth[match(uid, ids)]
    ids <- uid
  } else {
    ord <- order(ids); prob <- prob[ord, , drop = FALSE]; truth <- p$truth[ord]; ids <- ids[ord]
  }
  list(row_ids = ids, prob = prob, truth = truth)
}


#' Conformal calibration for one task at several alpha levels
#'
#' @param sv soft-vote OOF (list row_ids, prob, truth)
#' @param alphas miscoverage levels
#' @param site,tag,sensor,unit,source ids carried into the output
#' @return list(thresholds = long data.frame over alpha, apparent = coverage
#'   on the calibration OOF - a lower bound on honest coverage, flagged as such)
conformal_calibrate <- function(sv, alphas, site, tag,
                                sensor = NA, unit = NA, source = NA) {
  th <- do.call(rbind, lapply(alphas, function(a) {
    m <- mondrian_thresholds(sv$prob, sv$truth, a)
    cbind(alpha = a, m)
  }))
  app <- do.call(rbind, lapply(alphas, function(a) {
    m <- mondrian_thresholds(sv$prob, sv$truth, a)
    cov <- conformal_coverage(sv$prob, sv$truth, m)
    data.frame(alpha = a, overall = cov$overall, mean_set_size = cov$mean_set_size)
  }))
  meta <- data.frame(site = site, tag = tag, sensor = sensor, unit = unit, source = source)
  list(thresholds = cbind(meta[rep(1, nrow(th)), ], th, row.names = NULL),
       apparent = cbind(meta[rep(1, nrow(app)), ], app, row.names = NULL))
}


#' Per-class LAC inclusion thresholds on the probability scale
#'
#' A class is in a pixel's set when p[c] >= 1 - q_c. Returns 1 - q_c per class
#' band, in band order; q_c = Inf (unresolvable class) maps to -Inf, i.e.
#' "always in the set".
#'
#' @param cal conformal thresholds for one task at one alpha (class, q)
#' @param classes integer class codes, in prob-raster band order
#' @return numeric inclusion threshold per band
inclusion_thresholds <- function(cal, classes) {
  q <- cal$q[match(classes, cal$class)]
  1 - q
}


#' Landscape conformal surfaces for one unit at one alpha
#'
#' Applies the Mondrian thresholds to the averaged probability raster to make
#' the uncertainty products Reviewer 1 asked for:
#'   set_size          number of classes in the prediction set (1 = decisive)
#'   neltuma_possible  Neltuma is in the set (the management map: where to look)
#'   neltuma_only      the set is exactly {Neltuma} (confident Neltuma)
#'
#' @param prob_path the unit's averaged prob raster (Int16 x scale, tag-scaled)
#' @param cal conformal_cal rows for this unit's mapped task
#' @param alpha miscoverage level for these surfaces
#' @param neltuma_code Neltuma class code
#' @param site,tag ids for the filename
#' @param out_dir output directory
#' @return path to the 3-band conformal raster
conformal_surface <- function(prob_path, cal, alpha, neltuma_code, site, tag,
                              out_dir = out_path("conformal")) {
  p <- read_prob(prob_path[grepl("_prob\\.tif$", prob_path)][1])
  classes <- as.integer(sub("^prob_", "", names(p)))
  th <- inclusion_thresholds(cal[cal$alpha == alpha, , drop = FALSE], classes)
  insets <- terra::rast(lapply(seq_along(classes), function(i) p[[i]] >= th[i]))
  set_size <- sum(insets)
  nb <- which(classes == neltuma_code)
  nel_poss <- insets[[nb]]
  nel_only <- (set_size == 1) & nel_poss
  out <- c(set_size, nel_poss, nel_only)
  names(out) <- c("set_size", "neltuma_possible", "neltuma_only")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(out_dir, sprintf("%s__%s_conformal_a%02d.tif", site, tag, round(alpha * 100)))
  terra::writeRaster(out, path, overwrite = TRUE, datatype = "INT1U",
                     gdal = c("COMPRESS=LZW", "TILED=YES"), NAflag = 255)
  path
}


#' Neltuma area BOUNDS across alpha levels - the "range of km2" (R1 L272)
#'
#' Lower bound  = pixels whose set is exactly {Neltuma} (confident Neltuma).
#' Point        = the averaged hard class (argmax) - the single number the
#'                map shows, which is NOT reportable without the interval (7.39).
#' Upper bound  = pixels where Neltuma is in the set (cannot be ruled out).
#'
#' [lower, upper] is the conformal interval and lower <= upper always. The
#' hard-map point is a SEPARATE object and is NOT guaranteed to fall inside it:
#' at high alpha (tight sets) a pixel can be argmax-Neltuma yet have Neltuma
#' excluded from its set (p_neltuma below the inclusion threshold), so the
#' point can exceed the upper bound. That is correct - the interval reflects
#' calibrated confidence, the point reflects the winner - and the paper reports
#' them as distinct quantities, not point +/- interval.
#'
#' @param prob_path the averaged prob raster
#' @param cal conformal_cal rows for this unit's mapped task
#' @param alphas miscoverage levels
#' @param neltuma_code Neltuma class code
#' @param site,tag ids
#' @return data.frame: site, tag, alpha, neltuma_lower_ha, point_ha, neltuma_upper_ha
conformal_area_bounds <- function(prob_path, cal, alphas, neltuma_code, site, tag) {
  p <- read_prob(prob_path[grepl("_prob\\.tif$", prob_path)][1])
  classes <- as.integer(sub("^prob_", "", names(p)))
  nb <- which(classes == neltuma_code)
  px_ha <- prod(terra::res(p)) / 1e4
  hard <- terra::which.max(p)
  point_ha <- as.numeric(terra::global(hard == nb, "sum", na.rm = TRUE)[1, 1]) * px_ha
  do.call(rbind, lapply(alphas, function(a) {
    th <- inclusion_thresholds(cal[cal$alpha == a, , drop = FALSE], classes)
    insets <- terra::rast(lapply(seq_along(classes), function(i) p[[i]] >= th[i]))
    set_size <- sum(insets)
    nel_poss <- insets[[nb]]
    nel_only <- (set_size == 1) & nel_poss
    data.frame(site = site, tag = tag, alpha = a,
               neltuma_lower_ha = as.numeric(terra::global(nel_only, "sum", na.rm = TRUE)[1, 1]) * px_ha,
               point_ha = point_ha,
               neltuma_upper_ha = as.numeric(terra::global(nel_poss, "sum", na.rm = TRUE)[1, 1]) * px_ha,
               stringsAsFactors = FALSE)
  }))
}


#' Honest held-out coverage via K-fold cross-conformal
#'
#' The `apparent` coverage in `conformal_calibrate()` is measured on the same
#' OOF the thresholds were fit on, so it is mildly optimistic. Here the OOF is
#' split into K groups; each group's coverage is measured with thresholds
#' calibrated on the OTHER groups, so every test point is genuinely held out
#' from its own threshold. This is the honest number, and its gap from the
#' apparent figure is the optimism.
#'
#' Folds adapt to the calibration size (a class with few points cannot support
#' many folds); the accompanying `neltuma_n` keeps that visible.
#'
#' @param sv soft-vote OOF (list row_ids, prob, truth)
#' @param alphas miscoverage levels
#' @param neltuma_code Neltuma class code
#' @param site,tag ids
#' @param seed fold-assignment seed
#' @return data.frame: site, tag, alpha, overall, mean_set_size,
#'   neltuma_coverage, neltuma_n, folds
honest_coverage <- function(sv, alphas, neltuma_code, site, tag, seed = 5446) {
  n <- nrow(sv$prob)
  folds <- max(2L, min(5L, n %/% 10L))
  set.seed(seed)
  fk <- sample(rep_len(seq_len(folds), n))
  classes <- colnames(sv$prob)
  nk <- as.character(neltuma_code)
  tk <- as.character(sv$truth)
  do.call(rbind, lapply(alphas, function(a) {
    covered <- logical(n); ssize <- numeric(n)
    for (k in seq_len(folds)) {
      ci <- fk != k; ti <- fk == k
      th <- mondrian_thresholds(sv$prob[ci, , drop = FALSE], sv$truth[ci], a)
      sets <- prediction_sets(sv$prob[ti, , drop = FALSE], th)
      j <- match(tk[ti], classes)
      covered[ti] <- sets[cbind(seq_len(sum(ti)), j)]
      ssize[ti] <- rowSums(sets)
    }
    data.frame(site = site, tag = tag, alpha = a,
               overall = mean(covered), mean_set_size = mean(ssize),
               neltuma_coverage = if (any(tk == nk)) mean(covered[tk == nk]) else NA_real_,
               neltuma_n = sum(tk == nk), folds = folds, stringsAsFactors = FALSE)
  }))
}
