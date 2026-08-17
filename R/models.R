#' Tasks, learners and benchmarking
#'
#' Driven entirely from inst/config/resampling.yml. Nothing here hardcodes a
#' fold count, repeat count, tuner or budget.
#'
#' THIS IS NO LONGER A REPRODUCTION OF THE ORIGINAL TUNING DESIGN.
#'
#' Deliberate decision, 2026-08-17 [HUGH]. The archived learner ids describe a
#' graph - a scale/no-scale branch, a pca/passthrough branch and an importance
#' filter - which an element-by-element audit found to be inert: it tuned
#' preprocessing that cannot affect these learners, and tuned no learner
#' hyperparameter at all (finding 7.23). The graph is therefore gone, and the
#' learners tune real hyperparameters instead.
#'
#' The analysis SHAPE is still reproduced: per site, per predictor stack, the
#' same five learner slots, spatial cross-validation, the same benchmark
#' structure. Consequence to be aware of: our learner ids no longer match the
#' archived ones, so the structural evidence in finding 7.22 is spent.
#'
#' The ensemble is kept, because stacking is a real modelling choice rather than
#' an inert one. `ens_rf` now wraps ranger and `ens_svm` wraps svm; the original
#' had them swapped (finding 4.9).
#'
#' predict_type is "prob" everywhere (action item 3), which is the foundation the
#' conformal work deferred to the next refactor will need.


#' Build a spatial classification task
#'
#' Coordinates are used for spatial resampling only, never as predictors:
#' `coords_as_features = FALSE`. Including them would let a model memorise
#' location and inflate every accuracy estimate.
#'
#' @param df a training table from `build_training_table()`
#' @param site site id
#' @param tag stack tag
#' @param sites the sites table, for the CRS
#' @return a TaskClassifST
make_task <- function(df, site, tag, sites = read_sites()) {
  epsg <- sites$epsg[sites$site == site][1]
  drop <- intersect(c("site", "tag"), names(df))
  d <- df[, setdiff(names(df), drop), drop = FALSE]
  d$Type <- droplevels(d$Type)

  mlr3spatiotempcv::as_task_classif_st(
    d,
    target = "Type",
    id = paste0(site, "__", tag),
    coordinate_names = c("x", "y"),
    crs = paste0("EPSG:", epsg),
    coords_as_features = FALSE
  )
}


#' Trimmed SVM search space
#'
#' `lts("classif.svm.rbv2")` minus `tolerance`, which is the optimiser's stopping
#' criterion rather than a capacity parameter - tuning it buys numerical noise.
#' Dependencies are kept: gamma applies only to polynomial and radial kernels,
#' degree only to polynomial. Written out rather than merged programmatically so
#' the space a reviewer reads is the space that runs.
#'
#' @return a paradox::ParamSet
svm_search_space <- function() {
  paradox::ps(
    cost      = paradox::p_dbl(1e-4, 1e3, logscale = TRUE),
    kernel    = paradox::p_fct(c("linear", "polynomial", "radial")),
    gamma     = paradox::p_dbl(1e-4, 1e3, logscale = TRUE,
                               depends = kernel %in% c("polynomial", "radial")),
    degree    = paradox::p_int(2, 5, depends = kernel == "polynomial")
  )
}


#' Build one learner from its resampling.yml entry
#'
#' @param spec one entry from `resampling.yml$learners`
#' @param cfg the resolved resampling config
#' @return a Learner, wrapped in an AutoTuner when the spec is tuned
make_learner <- function(spec, cfg) {
  pt <- cfg$predict_type

  if (identical(spec$id, "ensemble")) return(ensemble_learner(cfg))

  base <- switch(
    spec$id,
    xgboost        = mlr3::lrn("classif.xgboost", predict_type = pt),
    ranger         = mlr3::lrn("classif.ranger", predict_type = pt,
                               importance = "impurity"),
    ranger_untuned = mlr3::lrn("classif.ranger", predict_type = pt,
                               id = "ranger.untuned"),
    svm            = mlr3::lrn("classif.svm", predict_type = pt,
                               type = "C-classification"),
    stop("Unknown learner id '", spec$id, "' in resampling.yml.", call. = FALSE)
  )

  if (!isTRUE(spec$tuned)) {
    # Baseline: no tuning. Now genuinely informative - with a search space that
    # has real leverage, a tuned learner that cannot beat this is saying
    # something about the data rather than about the search.
    return(base)
  }

  ss <- if (identical(spec$tuning_space, "svm_trimmed")) {
    svm_search_space()
  } else {
    # lts() sets to_tune() tokens on the learner; auto_tuner infers the space.
    base <- mlr3tuningspaces::lts(spec$tuning_space)$get_learner()
    base$predict_type <- pt
    if (identical(spec$id, "ranger")) {
      base$param_set$set_values(importance = "impurity")
    }
    if (identical(spec$id, "xgboost")) {
      # colsample_bylevel and friends declare a dependency on booster == "gbtree".
      # xgboost's own default IS gbtree, but mlr3 leaves the value unset, so the
      # dependency cannot be verified and tuning aborts. Set it explicitly.
      base$param_set$set_values(booster = "gbtree")
    }
    NULL
  }

  # batch_size per the mlr3 book: batch_size x inner resampling iterations should
  # be at least the number of future workers, or the last batch leaves workers
  # idle.
  fw <- future_workers()
  batch <- max(1L, ceiling(fw / max(1L, cfg$tuning$folds)))

  args <- list(
    tuner = mlr3tuning::tnr(cfg$tuning$tuner, batch_size = batch),
    learner = base,
    resampling = mlr3::rsmp(cfg$tuning$resampling, folds = cfg$tuning$folds),
    measure = mlr3::msr("classif.ce"),
    terminator = mlr3tuning::trm("evals", n_evals = cfg$tuning$term_evals),
    store_models = FALSE
  )
  if (!is.null(ss)) args$search_space <- ss
  do.call(mlr3tuning::auto_tuner, args)
}


#' The stacking ensemble
#'
#' Base learners are wrapped in `learner_cv` so the master trains on
#' out-of-fold predictions rather than in-sample ones. `ens_nop` passes the
#' original features through alongside.
#'
#' @param cfg the resolved resampling config
#' @return a GraphLearner
ensemble_learner <- function(cfg) {
  pt <- cfg$predict_type
  cv <- function(lrn, id) mlr3pipelines::po("learner_cv", lrn, id = id)

  stack <- mlr3pipelines::gunion(list(
    cv(mlr3::lrn("classif.xgboost", predict_type = pt), "ens_xgb"),
    # Correctly paired, unlike the original where these two were swapped (4.9).
    cv(mlr3::lrn("classif.ranger", predict_type = pt), "ens_rf"),
    cv(mlr3::lrn("classif.svm", predict_type = pt, type = "C-classification"), "ens_svm"),
    mlr3pipelines::po("nop", id = "ens_nop")
  )) %>>%
    mlr3pipelines::po("featureunion", id = "ens_union") %>>%
    mlr3::lrn("classif.ranger", predict_type = pt, id = "master_rf")

  gl <- mlr3::as_learner(stack)
  gl$predict_type <- pt
  gl$id <- "ensemble"
  gl
}


#' All configured learners
#'
#' @param cfg the resolved resampling config
#' @return named list of Learners
make_learners <- function(cfg) {
  ls <- lapply(cfg$learners, make_learner, cfg = cfg)
  names(ls) <- vapply(cfg$learners, function(x) x$id, character(1))
  ls
}


#' Number of future workers for in-task parallelism
#'
#' mlr3 parallelises `benchmark()` over the flattened set of (learner,
#' resampling iteration, tuning evaluation) jobs. That is a second level of
#' parallelism on top of crew, which parallelises across targets, so the product
#' of the two must stay within the machine.
#'
#' Whether it helps at all is an empirical question, not an obvious win: the mlr3
#' book advises against parallelising when individual iterations are short, and
#' these tasks are 82-222 rows, so a single fit runs in milliseconds. See
#' `mlr3.exec_chunk_size` below.
#'
#' @return integer worker count; 1 disables future entirely
future_workers <- function() {
  as.integer(Sys.getenv("NELTUMA_FUTURE", "1"))
}


#' Jobs grouped into one future task
#'
#' The mlr3 book: "Aim for chunks with a runtime of at least several seconds, so
#' that the parallelization overhead remains reasonable." A single fit here takes
#' milliseconds, so a chunk size of 1 - the mlr3 default - would spend far more
#' time dispatching than computing.
#'
#' @return integer chunk size
exec_chunk_size <- function() {
  as.integer(Sys.getenv("NELTUMA_CHUNK", "50"))
}


#' Benchmark the configured learners on one task
#'
#' Uses the final resampling from resampling.yml. The seed is set here so a
#' repeated run reproduces; note that the original's balanced sampling called
#' `sample()` unseeded, so exact training sets are unrecoverable and the target
#' is statistical equivalence, not bit-identity.
#'
#' @param task a TaskClassifST
#' @param cfg the resolved resampling config
#' @return a BenchmarkResult
run_benchmark <- function(task, cfg) {
  # Keep one worker to roughly one core. data.table defaults to half the
  # machine's cores (32 of 64 here) and spawns a pool that size, but the pool
  # SLEEPS when idle - it does not hold 32 runnable threads - so this is a tidy
  # -up, not a fix for contention. Measured: one benchmark process sat at a
  # single running thread with 64 sleeping. Pinning to 1 removes a variable from
  # the timings and avoids 30 workers each carrying a pool they never use, on
  # tables of 82-222 rows. ranger and xgboost are already single-threaded in
  # mlr3learners.
  data.table::setDTthreads(1L)

  fw <- future_workers()
  if (fw > 1L) {
    options(mlr3.exec_chunk_size = exec_chunk_size())
    old_plan <- future::plan(future::multisession, workers = fw)
    on.exit(future::plan(old_plan), add = TRUE)
  }

  set.seed(cfg$seed)
  learners <- make_learners(cfg)
  resampling <- if (identical(cfg$final$resampling, "repeated_spcv_coords")) {
    mlr3::rsmp("repeated_spcv_coords", folds = cfg$final$folds,
               repeats = cfg$final$repeats)
  } else {
    mlr3::rsmp(cfg$final$resampling, folds = cfg$final$folds)
  }
  design <- mlr3::benchmark_grid(task, unname(learners), resampling)
  mlr3::benchmark(design, store_models = FALSE)
}


#' Tidy the benchmark result
#'
#' Both accuracy and classification error, plus the spread across resampling
#' iterations - the manuscript reports a single mean, and the spread is what
#' Reviewer 1's uncertainty question actually needs.
#'
#' @param bmr a BenchmarkResult
#' @param site site id
#' @param tag stack tag
#' @return data.frame, one row per learner
tidy_benchmark <- function(bmr, site, tag) {
  agg <- bmr$aggregate(list(mlr3::msr("classif.acc"), mlr3::msr("classif.ce")))
  sc  <- bmr$score(mlr3::msr("classif.acc"))
  spread <- stats::aggregate(classif.acc ~ learner_id, data = sc,
                             FUN = function(z) c(sd = stats::sd(z),
                                                 min = min(z), max = max(z)))
  sp <- data.frame(learner_id = spread$learner_id,
                   acc_sd = spread$classif.acc[, "sd"],
                   acc_min = spread$classif.acc[, "min"],
                   acc_max = spread$classif.acc[, "max"],
                   stringsAsFactors = FALSE)

  out <- data.frame(
    site = site, tag = tag,
    learner_id = agg$learner_id,
    classif.acc = agg$classif.acc,
    classif.ce = agg$classif.ce,
    n_iters = agg$iters,
    stringsAsFactors = FALSE
  )
  merge(out, sp, by = "learner_id", all.x = TRUE)
}
