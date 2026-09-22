#' Single-pass compiled prediction for ranger forests
#'
#' Performance finding 2026-09-22 (prediction-arm review, measured on real tiles):
#' ranger's predict is 58-72% of all landscape-prediction CPU. Its C++ predict is
#' two-phase - it fills a `num.trees x n_rows x 8 B` terminal-node buffer (13 GB per
#' WV2 block, 8 GB per 2M-cell cover tile) and then gathers across trees per row -
#' and every call first re-marshals the whole R forest into C++ (~10 s per call for
#' the 690k-row cover forest; 352 tiles = ~1 h of the 3.4 h WV2 cover run).
#'
#' This flattens the fitted forest ONCE into contiguous vectors and predicts each
#' row in one pass over the trees, accumulating class fractions (probability
#' forests) or terminal means (regression) directly. Identical to ranger up to
#' floating-point summation order (tested to 1e-12 in tests/testthat/test-forest.R).
#' ranger's rule is `x <= split.value -> left child`; terminal nodes have child id
#' 0; `split.varIDs` are 0-based indexes into `independent.variable.names`. Only
#' numeric (ordered) predictors are supported - the cubes carry nothing else - and
#' the flattener refuses a forest with unordered factor splits.
#'
#' Compiled with Rcpp on first use and cached on disk (`cacheDir`), so crew workers
#' and mirai daemons pay the compile once per code change, not per process.

FOREST_CACHE_DIR <- out_path(".rcpp-cache")

.forest_cpp <- '
#include <Rcpp.h>
#include <vector>
#ifdef _OPENMP
#include <omp.h>
#endif
using namespace Rcpp;

// TREE-MAJOR traversal (measured 2026-09-22: a row-major loop over a 500-tree
// forest with ~140k nodes per tree was 3x SLOWER than ranger because every row
// touched 500 cold trees; sweeping all rows through one tree at a time keeps that
// tree\'s nodes in cache, which is also ranger\'s order - but here the terminal
// payload is accumulated straight into the output instead of a num.trees x n
// buffer, and the forest is never re-marshalled). X is copied once to row-major so
// a row\'s predictors sit in one cache line. Threads split the trees and reduce
// their private accumulators at the end.
// [[Rcpp::plugins(openmp)]]
// [[Rcpp::export]]
NumericMatrix forest_predict_cpp(NumericMatrix X, IntegerVector left, IntegerVector right,
                                 IntegerVector var, NumericVector split, NumericVector payload,
                                 IntegerVector off, IntegerVector nn, int k, int nthreads) {
  const int n = X.nrow(), p = X.ncol(), T = off.size();
  std::vector<double> xr((size_t)n * p);
  for (int j = 0; j < p; ++j) for (int i = 0; i < n; ++i) xr[(size_t)i * p + j] = X(i, j);
  const int* L = left.begin(); const int* R = right.begin(); const int* V = var.begin();
  const double* S = split.begin(); const double* P = payload.begin(); const int* O = off.begin();
  NumericMatrix out(n, k);
  double* o = out.begin();
  int nth = 1;
#ifdef _OPENMP
  if (nthreads > 0) omp_set_num_threads(nthreads);
  nth = omp_get_max_threads();
#endif
  std::vector<std::vector<double> > acc(nth, std::vector<double>((size_t)n * k, 0.0));
#ifdef _OPENMP
  #pragma omp parallel
#endif
  {
    int tid = 0;
#ifdef _OPENMP
    tid = omp_get_thread_num();
#endif
    double* a = acc[tid].data();
#ifdef _OPENMP
    #pragma omp for schedule(dynamic, 1)
#endif
    for (int t = 0; t < T; ++t) {
      const int base = O[t];
      const int* Lt = L + base; const int* Rt = R + base; const int* Vt = V + base;
      const double* St = S + base; const double* Pt = P + (size_t)base * k;
      for (int i = 0; i < n; ++i) {
        const double* xi = xr.data() + (size_t)i * p;
        int node = 0;
        while (true) {
          const int l = Lt[node];
          if (l == 0) break;
          node = (xi[Vt[node]] <= St[node]) ? l : Rt[node];
        }
        const double* pl = Pt + (size_t)node * k;
        double* ai = a + (size_t)i * k;
        for (int c = 0; c < k; ++c) ai[c] += pl[c];
      }
    }
  }
  for (int i = 0; i < n; ++i) for (int c = 0; c < k; ++c) {
    double s = 0.0;
    for (int th = 0; th < nth; ++th) s += acc[th][(size_t)i * k + c];
    o[i + (size_t)c * n] = s / T;
  }
  return out;
}
'

#' Compile (or load from cache) the traversal
#' @return the compiled function
forest_predict_fn <- function() {
  fn <- get0(".forest_predict_compiled", envir = globalenv(), inherits = FALSE)
  if (!is.null(fn)) return(fn)
  dir.create(FOREST_CACHE_DIR, recursive = TRUE, showWarnings = FALSE)
  env <- new.env()
  Rcpp::sourceCpp(code = .forest_cpp, env = env, cacheDir = FOREST_CACHE_DIR, verbose = FALSE)
  fn <- get("forest_predict_cpp", envir = env)
  assign(".forest_predict_compiled", fn, envir = globalenv())
  fn
}


#' Flatten a fitted ranger model into contiguous prediction arrays
#'
#' @param model a `ranger` object (probability forest or regression forest)
#' @return list(left, right, var, split, payload, off, nn, k, classes, vars, type)
flatten_ranger <- function(model) {
  f <- model$forest
  if (!is.null(f$is.ordered) && !all(f$is.ordered)) {
    stop("flatten_ranger: unordered factor splits are not supported.", call. = FALSE)
  }
  type <- f$treetype
  if (!type %in% c("Probability estimation", "Regression")) {
    stop("flatten_ranger: unsupported treetype '", type, "'.", call. = FALSE)
  }
  k <- if (type == "Regression") 1L else length(f$class.values)
  nn <- vapply(f$child.nodeIDs, function(ch) length(ch[[1]]), integer(1))
  off <- c(0L, cumsum(nn)[-length(nn)])
  left  <- unlist(lapply(f$child.nodeIDs, `[[`, 1L), use.names = FALSE)
  right <- unlist(lapply(f$child.nodeIDs, `[[`, 2L), use.names = FALSE)
  var   <- unlist(f$split.varIDs, use.names = FALSE)
  split <- unlist(f$split.values, use.names = FALSE)
  payload <- if (type == "Regression") {
    as.numeric(split)                           # terminal split.values hold the mean
  } else {
    unlist(lapply(seq_along(f$terminal.class.counts), function(t) {
      tc <- f$terminal.class.counts[[t]]
      unlist(lapply(tc, function(v) if (length(v)) as.numeric(v) else numeric(k)), use.names = FALSE)
    }), use.names = FALSE)
  }
  stopifnot(length(payload) == sum(nn) * k, length(left) == sum(nn))
  list(left = as.integer(left), right = as.integer(right), var = as.integer(var),
       split = as.numeric(split), payload = payload, off = as.integer(off), nn = as.integer(nn),
       k = k, classes = if (type == "Regression") NULL else as.character(f$levels[f$class.values]),
       vars = f$independent.variable.names, type = type)
}


#' Predict with a flattened forest
#'
#' @param ff output of `flatten_ranger()`
#' @param newdata data.frame or matrix holding at least the forest's predictors
#' @param nthreads OpenMP threads (0 = library default)
#' @return probability forest: n x k matrix with class names; regression: numeric n
predict_flat_forest <- function(ff, newdata, nthreads = 1L) {
  X <- if (is.matrix(newdata)) newdata[, ff$vars, drop = FALSE] else
    as.matrix(newdata[, ff$vars, drop = FALSE])
  storage.mode(X) <- "double"
  out <- forest_predict_fn()(X, ff$left, ff$right, ff$var, ff$split, ff$payload,
                             ff$off, ff$nn, ff$k, as.integer(nthreads))
  if (ff$type == "Regression") return(as.numeric(out[, 1]))
  colnames(out) <- ff$classes
  out
}


#' A fast predictor for an mlr3 learner, falling back to the learner itself
#'
#' For a trained `classif.ranger` (predict_type prob) or `regr.ranger` learner the
#' flattened forest is used; every other learner predicts through mlr3 as before.
#' Returns a function(newdata) -> probability matrix (columns = `lvls`, for
#' classification) or numeric vector (regression), so callers do not branch.
#'
#' @param learner a trained mlr3 Learner
#' @param lvls class levels in the order the caller wants columns (classif only)
#' @param nthreads threads for the compiled traversal
#' @return function(newdata)
fast_predictor <- function(learner, lvls = NULL, nthreads = 1L) {
  model <- learner$model
  # mlr3learners wraps regr.ranger's model as list(model = <ranger>) (quantile support)
  if (!inherits(model, "ranger") && is.list(model) && inherits(model$model, "ranger")) model <- model$model
  is_ranger <- inherits(model, "ranger") &&
    model$forest$treetype %in% c("Probability estimation", "Regression") &&
    isTRUE(Sys.getenv("NELTUMA_FAST_FOREST", "1") == "1")
  if (is_ranger) {
    ff <- flatten_ranger(model)
    fn <- if (ff$type == "Regression") {
      function(newdata) predict_flat_forest(ff, newdata, nthreads)
    } else {
      function(newdata) predict_flat_forest(ff, newdata, nthreads)[, lvls, drop = FALSE]
    }
    attr(fn, "engine") <- "flat"
    return(fn)
  }
  fn <- if (is.null(lvls)) {
    function(newdata) as.numeric(learner$predict_newdata(newdata)$response)
  } else {
    function(newdata) learner$predict_newdata(newdata)$prob[, lvls, drop = FALSE]
  }
  attr(fn, "engine") <- "mlr3"
  fn
}
