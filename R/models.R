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
  # No polynomial kernel (decision 2026-09-15 [HUGH]). Measured on the
  # 2,000-row Planet task: every linear/radial configuration in this space
  # fits in under 3 s; polynomial degree 5 takes ~107 s and hits libsvm's
  # iteration cap, which is what turned one tuning target into a 45-minute
  # smoke-test stall and a 1.5 h full-budget one. Of 31 banked svm winners,
  # 3 were polynomial, all degree 2, none by a clear margin. Finding 7.36.
  paradox::ps(
    cost      = paradox::p_dbl(1e-4, 1e3, logscale = TRUE),
    kernel    = paradox::p_fct(c("linear", "radial")),
    gamma     = paradox::p_dbl(1e-4, 1e3, logscale = TRUE,
                               depends = kernel == "radial")
  )
}


#' Compact xgboost search space
#'
#' Replaces lts("classif.xgboost.default"), whose nrounds ranged to ~2000 and
#' made xgboost the only long tail of the first full run: 28 fits still open at
#' 7 hours while every other learner had finished. On 82-222 observations there
#' is no realistic loss from capping rounds at 500 and tuning the four
#' parameters with real capacity leverage.
#'
#' @return a paradox::ParamSet
xgboost_search_space <- function() {
  paradox::ps(
    eta              = paradox::p_dbl(0.01, 0.3, logscale = TRUE),
    max_depth        = paradox::p_int(2L, 8L),
    nrounds          = paradox::p_int(50L, 500L),
    subsample        = paradox::p_dbl(0.5, 1),
    colsample_bytree = paradox::p_dbl(0.5, 1)
  )
}


#' lightgbm search space
#'
#' No lts space exists for lightgbm, so this is written out. Same philosophy as
#' the xgboost trim: the capacity parameters, bounded sanely for tables of
#' 82-222 rows. min_data_in_leaf is allowed down to 2 because the rarest classes
#' have single-digit counts (finding 7.21).
#'
#' @return a paradox::ParamSet
lightgbm_search_space <- function() {
  paradox::ps(
    learning_rate    = paradox::p_dbl(0.01, 0.3, logscale = TRUE),
    num_leaves       = paradox::p_int(4L, 64L),
    num_iterations   = paradox::p_int(50L, 500L),
    bagging_fraction = paradox::p_dbl(0.5, 1),
    feature_fraction = paradox::p_dbl(0.5, 1),
    min_data_in_leaf = paradox::p_int(2L, 20L)
  )
}


#' Build one learner from its resampling.yml entry
#'
#' @param spec one entry from `resampling.yml$learners`
#' @param cfg the resolved resampling config
#' @return a Learner, wrapped in an AutoTuner when the spec is tuned
#' A bare learner with no tuning apparatus
#'
#' @param spec one entry from `resampling.yml$learners`
#' @param shared the shared budget
#' @return a Learner
bare_learner <- function(spec, shared) {
  pt <- shared$predict_type
  if (identical(spec$id, "ensemble")) return(ensemble_learner(shared))
  switch(
    spec$id,
    xgboost        = mlr3::lrn("classif.xgboost", predict_type = pt,
                               booster = "gbtree"),
    lightgbm       = mlr3::lrn("classif.lightgbm", predict_type = pt,
                               verbose = -1L, num_threads = 1L),
    glmnet         = mlr3::lrn("classif.glmnet", predict_type = pt),
    ranger         = mlr3::lrn("classif.ranger", predict_type = pt,
                               importance = "impurity"),
    ranger_untuned = mlr3::lrn("classif.ranger", predict_type = pt,
                               id = "ranger.untuned"),
    svm            = mlr3::lrn("classif.svm", predict_type = pt,
                               type = "C-classification"),
    stop("Unknown learner id '", spec$id, "' in resampling.yml.", call. = FALSE)
  )
}


#' Wrap a learner so a failing fold scores as a failure, not a crash
#'
#' Finding 1.7 made concrete: V. erioloba has n = 2 at struizendam_4, so under
#' 5-fold CV most training folds hold 0-1 observations of it and glmnet's
#' multinomial refuses ("one multinomial or binomial class has 1 or 0
#' observations"), where trees and svm quietly cope. Encapsulation scores the
#' failing fold with a featureless fallback instead of killing the run, for
#' every learner - a fold-level failure is information, not a reason to lose
#' 300 targets.
#'
#' @param learner a Learner, modified in place and returned
#' @param pt predict_type for the fallback
with_fallback <- function(learner, pt) {
  learner$encapsulate("evaluate",
                      fallback = mlr3::lrn("classif.featureless", predict_type = pt))
  learner
}



#' Run an expression under the configured future plan
#'
#' NELTUMA_FUTURE > 1 parallelises mlr3's inner loops (tuning evaluations,
#' outer resampling iterations). Measured 4.4x at 8 workers once
#' exec_chunk_size is 1 (7.24). Always multisession: forking a process after
#' lightgbm has initialised its OpenMP pool deadlocks the next resample
#' (7.35), and multisession measured within 1% of multicore.
#'
#' @param expr expression to evaluate
#' @return the expression's value
with_future_plan <- function(expr) {
  fw <- future_workers()
  if (fw > 1L) {
    old_opt <- options(mlr3.exec_chunk_size = exec_chunk_size())
    old_plan <- future::plan(future::multisession, workers = fw)
    on.exit({ future::plan(old_plan); options(old_opt) }, add = TRUE)
  }
  force(expr)
}

#' Tune once per task, returning the chosen configuration
#'
#' TUNING IS DELIBERATELY NOT NESTED IN THE OUTER REPEATS (decision 2026-08-18
#' [HUGH]). The previous design re-ran the full 250-fit search inside each of
#' the 100 outer iterations - 25,100 fits per tuned learner per task - although
#' the tuning result barely changes across repeats: repeats measure the
#' fold-assignment sensitivity of the ACCURACY, not of the search. Tuning once
#' and evaluating the fixed configuration costs ~350 fits instead, ~70x less.
#'
#' The trade, stated honestly: the reported estimate is now "accuracy of the
#' CHOSEN configuration under spatial CV", not "of the tuning procedure" - the
#' configuration was selected using all of the task's data. This is a far
#' weaker leak than reporting the winning inner score (finding 7.23's 0.946
#' artefact), and the winning inner score is never reported.
#'
#' @param task a TaskClassifST
#' @param spec one learner entry
#' @param shared the shared budget
#' @return named list of chosen parameter values, or NULL for untuned specs
tune_config <- function(task, spec, shared) {
  if (!isTRUE(spec$tuned)) return(NULL)
  data.table::setDTthreads(1L)

  ss <- if (identical(spec$tuning_space, "svm_trimmed")) {
    svm_search_space()
  } else if (identical(spec$tuning_space, "xgboost_trimmed")) {
    xgboost_search_space()
  } else if (identical(spec$tuning_space, "lightgbm_custom")) {
    lightgbm_search_space()
  } else {
    NULL
  }

  learner <- if (is.null(ss)) {
    # lts() sets to_tune() tokens on the learner; tune() infers the space.
    l <- mlr3tuningspaces::lts(spec$tuning_space)$get_learner()
    l$predict_type <- shared$predict_type
    if (identical(spec$id, "ranger")) l$param_set$set_values(importance = "impurity")
    l
  } else {
    bare_learner(spec, shared)
  }

  learner <- with_fallback(learner, shared$predict_type)

  set.seed(shared$seed)
  args <- list(
    tuner = mlr3tuning::tnr(shared$tuning$tuner),
    task = task,
    learner = learner,
    resampling = mlr3::rsmp(shared$tuning$resampling, folds = shared$tuning$folds),
    measures = mlr3::msr("classif.ce"),
    terminator = mlr3tuning::trm("evals", n_evals = shared$tuning$term_evals),
    store_models = FALSE
  )
  if (!is.null(ss)) args$search_space <- ss
  ti <- with_future_plan(do.call(mlr3tuning::tune, args))
  ti$result_learner_param_vals
}


#' The stacking ensemble
#'
#' Base learners are wrapped in `learner_cv` so the master trains on
#' out-of-fold predictions rather than in-sample ones. `ens_nop` passes the
#' original features through alongside.
#'
#' @param cfg the resolved resampling config
#' @return a GraphLearner
ensemble_learner <- function(shared) {
  pt <- shared$predict_type
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


#' Number of future workers for in-task parallelism
#'
#' Measured (finding 7.24): with chunk size 1 this reaches 4.37x at 8 workers.
#' Kept at 1 by default because crew across the per-learner fit targets already
#' parallelises at ~100% efficiency; raise only when targets are scarce.
#'
#' @return integer worker count; 1 disables future entirely
future_workers <- function() {
  as.integer(Sys.getenv("NELTUMA_FUTURE", "1"))
}


#' Jobs grouped into one future task
#'
#' Deliberately 1: each job at this level is a whole tuning call or outer fit of
#' seconds, and chunking them serialises the loop - chunk 10 against a
#' 5-iteration outer loop made one chunk and was briefly mistaken for future not
#' working at all (7.24).
#'
#' @return integer chunk size
exec_chunk_size <- function() {
  as.integer(Sys.getenv("NELTUMA_CHUNK", "1"))
}


#' One learner spec from the resolved config
#'
#' Exists so each fit target depends on ITS OWN learner's spec plus the shared
#' budget, not on the whole config. targets invalidates on upstream VALUE, so
#' editing xgboost's entry reruns only xgboost fits - the first full run lost
#' 112 banked fits to exactly this coupling.
#'
#' @param cfg the resolved resampling config
#' @param learner_id one id
#' @return the spec list
learner_spec <- function(cfg, learner_id) {
  spec <- Filter(function(x) identical(x$id, learner_id), cfg$learners)
  if (length(spec) != 1L) {
    stop("No learner '", learner_id, "' in resampling.yml.", call. = FALSE)
  }
  spec[[1]]
}


#' Settings the tuning stage depends on
#'
#' Split from the evaluation settings so that changing the tuner or its budget
#' invalidates only the tune_config targets and their downstream fits - the
#' untuned learners (ensemble, baseline) never touch these and their fits
#' survive a tuner change untouched. Same dependency-granularity lesson as the
#' per-learner specs.
#'
#' @param cfg the resolved resampling config
#' @return list of seed, predict_type and tuning settings
tuning_settings <- function(cfg) {
  cfg[c("seed", "predict_type", "tuning")]
}


#' Settings the final evaluation depends on
#'
#' @param cfg the resolved resampling config
#' @return list of seed, predict_type and final-resampling settings
eval_settings <- function(cfg) {
  cfg[c("seed", "predict_type", "final")]
}


#' Resample one learner on one task
#'
#' One (site, stack, learner) combination per target, replacing the
#' mlr3 benchmark() wrapper. benchmark() was only grouping independent resamples
#' inside one process; splitting them gives crew 140 schedulable units instead
#' of 28, which parallelises at ~100% efficiency instead of relying on futures
#' at ~55% (finding 7.24).
#'
#' @param task a TaskClassifST
#' @param learner_id one id from resampling.yml
#' @param cfg the resolved resampling config
#' @return a ResampleResult
run_resample <- function(task, spec, shared, config = NULL) {
  # Keep one worker to roughly one core; see finding 7.24.
  data.table::setDTthreads(1L)

  learner <- with_fallback(bare_learner(spec, shared), shared$predict_type)
  if (!is.null(config)) {
    keep <- config[names(config) %in% learner$param_set$ids()]
    learner$param_set$set_values(.values = keep)
  }

  resampling <- if (identical(shared$final$resampling, "repeated_spcv_coords")) {
    mlr3::rsmp("repeated_spcv_coords", folds = shared$final$folds,
               repeats = shared$final$repeats)
  } else {
    mlr3::rsmp(shared$final$resampling, folds = shared$final$folds)
  }

  # Same seed for every learner, so all learners on a task see identical outer
  # splits and their scores are paired, not merely comparable.
  set.seed(shared$seed)
  with_future_plan(mlr3::resample(task, learner, resampling, store_models = FALSE))
}


#' Tidy one resample result
#'
#' Mean, error and the spread across iterations - the manuscript reports single
#' figures, and the spread is what Reviewer 1's uncertainty question needs.
#'
#' @param rr a ResampleResult
#' @param site site id
#' @param tag stack tag
#' @param learner_id the configured learner id
#' @return one-row data.frame
tidy_resample <- function(rr, site, tag, learner_id, sensor = NA_character_,
                          unit = NA_character_, source = NA_character_) {
  sc <- rr$score(mlr3::msr("classif.acc"))$classif.acc
  data.frame(
    sensor = sensor, unit = unit, source = source,
    site = site, tag = tag, learner = learner_id,
    classif.acc = mean(sc), classif.ce = 1 - mean(sc),
    acc_sd = stats::sd(sc), acc_min = min(sc), acc_max = max(sc),
    n_iters = length(sc),
    stringsAsFactors = FALSE
  )
}


#' Best learner per site and stack
#'
#' Selection by mean accuracy, with the runner-up gap reported so a "best" that
#' won by less than the noise is visible as such rather than silently promoted.
#'
#' @param scores combined rows from `tidy_resample()`
#' @return one row per site x tag
select_best <- function(scores) {
  do.call(rbind, lapply(split(scores, paste(scores$site, scores$tag)), function(g) {
    g <- g[order(-g$classif.acc), , drop = FALSE]
    out <- g[1, , drop = FALSE]
    out$runner_up <- if (nrow(g) > 1) g$learner[2] else NA_character_
    out$margin <- if (nrow(g) > 1) g$classif.acc[1] - g$classif.acc[2] else NA_real_
    # A win inside one standard deviation of the winner's own iterations is a
    # coin flip, and should be read as "no clear winner".
    out$clear_win <- !is.na(out$margin) & out$margin > out$acc_sd
    out
  }))
}


#' Per-class accuracy for one resample result - the "Neltuma-specific" figure
#'
#' The manuscript reports a "Neltuma-specific accuracy" (Fig 4C, section 3.3)
#' without defining it; recall of the Neltuma class over the pooled
#' cross-validation predictions is the reading that matches its numbers.
#' Kept out of tidy_resample() on purpose: adding columns there would change
#' best_models' rows and re-run every landscape prediction that depends on
#' them.
#'
#' @param rr a ResampleResult
#' @param site,tag,learner_id ids
#' @param code the class code of interest (Neltuma = 1)
#' @return one-row data.frame: recall, precision, n_truth for that class
tidy_class_accuracy <- function(rr, site, tag, learner_id, code = 1L,
                                sensor = NA_character_, unit = NA_character_,
                                source = NA_character_) {
  p  <- rr$prediction()
  cm <- table(truth = p$truth, response = p$response)
  k  <- as.character(code)
  tp <- if (k %in% rownames(cm) && k %in% colnames(cm)) cm[k, k] else 0
  n_truth <- if (k %in% rownames(cm)) sum(cm[k, ]) else 0
  n_pred  <- if (k %in% colnames(cm)) sum(cm[, k]) else 0
  data.frame(sensor = sensor, unit = unit, source = source,
             site = site, tag = tag, learner = learner_id, class = code,
             recall = if (n_truth) tp / n_truth else NA_real_,
             precision = if (n_pred) tp / n_pred else NA_real_,
             n_truth = as.integer(n_truth), stringsAsFactors = FALSE)
}
