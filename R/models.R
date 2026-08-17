#' Tasks, learners and benchmarking
#'
#' The learner graph is reconstructed from the learner ids recorded in the
#' archived benchmark workbooks, which spell the pipeline out in full:
#'
#'   scale_branch.scale.no.scale.scale_unbranch.
#'   pre_branch.pca.nop.pre_unbranch.
#'   importance.classif.xgboost
#'
#' That is: a tuned branch between scaling and not scaling, a tuned branch
#' between PCA and passthrough, an importance filter, then the learner. The SVM
#' id carries no `importance` token, so SVM is unfiltered. The ensemble id
#'
#'   ens_xgb.ens_rf.ens_svm.ens_nop.ens_union.master_rf
#'
#' is a stack of cross-validated base learners plus a passthrough, unioned and
#' fed to a random-forest master.
#'
#' Everything numeric comes from inst/config/resampling.yml. Nothing here
#' hardcodes a fold count, repeat count, tuner or budget.
#'
#' TWO DELIBERATE DEPARTURES
#'
#' predict_type is "prob" everywhere (action item 3). The original never set it,
#' which is why no uncertainty surface exists (finding 1.5) and why the conformal
#' work has nothing to build on. Hard class labels remain available from the
#' probabilities, so this loses nothing.
#'
#' In the original ensemble, `ens_rf` wrapped the SVM and `ens_svm` wrapped
#' ranger (finding 4.9). The labels are corrected here. Cosmetic for the fitted
#' model, but the original made learner_id output actively misleading.


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


#' The shared preprocessing graph
#'
#' scale-or-not and PCA-or-not, both as tuned branches, exactly as the archived
#' learner ids describe.
#'
#' @param with_filter add the importance filter stage
#' @return a Graph
preproc_graph <- function(with_filter = TRUE) {
  g <-
    mlr3pipelines::po("branch", options = c("scale", "no.scale"), id = "scale_branch") %>>%
    mlr3pipelines::gunion(list(
      mlr3pipelines::po("scale"),
      mlr3pipelines::po("nop", id = "no.scale")
    )) %>>%
    mlr3pipelines::po("unbranch", options = c("scale", "no.scale"), id = "scale_unbranch") %>>%
    mlr3pipelines::po("branch", options = c("pca", "nop"), id = "pre_branch") %>>%
    mlr3pipelines::gunion(list(
      mlr3pipelines::po("pca"),
      mlr3pipelines::po("nop")
    )) %>>%
    mlr3pipelines::po("unbranch", options = c("pca", "nop"), id = "pre_unbranch")

  if (with_filter) {
    g <- g %>>% mlr3pipelines::po(
      "filter",
      filter = mlr3filters::flt("importance",
                                learner = mlr3::lrn("classif.ranger",
                                                    importance = "impurity")),
      id = "importance"
    )
  }
  g
}


#' Search space for a learner specification
#'
#' The branch choices are always tuned. `filter.frac` is tuned over 0.1-1 where
#' a filter is present, per resampling.yml. Learner hyperparameters come from
#' mlr3tuningspaces where a space is named.
#'
#' @param spec one learner entry from resampling.yml
#' @return a paradox::ParamSet, or NULL if nothing to tune
search_space_for <- function(spec) {
  if (!is.null(spec$tuning_space) && identical(spec$tuning_space, "classif.svm.rbv2")) {
    # Transcribed from lts("classif.svm.rbv2")$values, with the dependencies the
    # flat space would otherwise lose: gamma applies only to polynomial and
    # radial kernels, degree only to polynomial. Written out rather than merged
    # programmatically so the space a reviewer sees is the space that runs.
    return(paradox::ps(
      scale_branch.selection = paradox::p_fct(c("scale", "no.scale")),
      pre_branch.selection   = paradox::p_fct(c("pca", "nop")),
      classif.svm.kernel     = paradox::p_fct(c("linear", "polynomial", "radial")),
      classif.svm.cost       = paradox::p_dbl(1e-4, 1e3, logscale = TRUE),
      classif.svm.tolerance  = paradox::p_dbl(1e-4, 2, logscale = TRUE),
      classif.svm.gamma      = paradox::p_dbl(1e-4, 1e3, logscale = TRUE,
                                              depends = classif.svm.kernel %in%
                                                c("polynomial", "radial")),
      classif.svm.degree     = paradox::p_int(2, 5,
                                              depends = classif.svm.kernel ==
                                                "polynomial")
    ))
  }

  if (!is.null(spec$filter)) {
    return(paradox::ps(
      scale_branch.selection = paradox::p_fct(c("scale", "no.scale")),
      pre_branch.selection   = paradox::p_fct(c("pca", "nop")),
      importance.filter.frac = paradox::p_dbl(0.1, 1)
    ))
  }

  paradox::ps(
    scale_branch.selection = paradox::p_fct(c("scale", "no.scale")),
    pre_branch.selection   = paradox::p_fct(c("pca", "nop"))
  )
}


#' Build one learner from its resampling.yml entry
#'
#' @param spec one entry from `resampling.yml$learners`
#' @param cfg the resolved resampling config
#' @param task_id used only for messages
#' @return a Learner, wrapped in an AutoTuner when the spec is tuned
make_learner <- function(spec, cfg) {
  pt <- cfg$predict_type

  base <- switch(
    spec$id,
    xgboost        = mlr3::lrn("classif.xgboost", predict_type = pt),
    ranger         = mlr3::lrn("classif.ranger", predict_type = pt,
                               importance = "impurity"),
    ranger_untuned = mlr3::lrn("classif.ranger", predict_type = pt,
                               id = "ranger.untuned"),
    svm            = mlr3::lrn("classif.svm", predict_type = pt,
                               type = "C-classification"),
    ensemble       = NULL,
    stop("Unknown learner id '", spec$id, "' in resampling.yml.", call. = FALSE)
  )

  if (identical(spec$id, "ensemble")) return(ensemble_learner(cfg))

  if (!isTRUE(spec$tuned)) {
    # Baseline: no graph, no tuning. Its whole purpose is to show what the
    # elaborate pipeline is worth relative to a bare learner.
    return(mlr3::as_learner(base))
  }

  graph <- preproc_graph(with_filter = !is.null(spec$filter)) %>>% base
  gl <- mlr3::as_learner(graph)
  gl$predict_type <- pt

  ss <- search_space_for(spec)

  mlr3tuning::auto_tuner(
    tuner = mlr3tuning::tnr(cfg$tuning$tuner),
    learner = gl,
    resampling = mlr3::rsmp(cfg$tuning$resampling, folds = cfg$tuning$folds),
    measure = mlr3::msr("classif.ce"),
    search_space = ss,
    terminator = mlr3tuning::trm("evals", n_evals = cfg$tuning$term_evals),
    store_models = FALSE
  )
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
