#' Pipeline invariants, asserted after every run (refactor-3.0 4.4)
#'
#' The original plan's item 10: a small validation step that checks the
#' outputs against what the paper will claim, so a silent regression fails the
#' run instead of surfacing in a figure. Cheap, table-level, no rasters
#' re-read. Add a check here whenever a finding records a way things went
#' wrong before.

#' Assert the run's invariants
#'
#' @param score_index_all every fit's scores
#' @param training_index per-task attrition summaries
#' @param cube_index drone cube verification rows
#' @param resampling the resolved resampling config
#' @param sensors_cfg sensors.yml
#' @param cv_index per-task evaluation designs (W, folds, iterations)
#' @return one-row data.frame of the counts checked, invisibly
run_checks <- function(score_index_all, training_index, cube_index, resampling, sensors_cfg, cv_index) {
  fail <- character(0)
  note <- function(ok, msg) if (!isTRUE(ok)) fail <<- c(fail, msg)
  # The soft-vote 'average' is a derived reporting row, not a fitted learner:
  # exclude it from the per-learner completeness and iteration checks.
  score_index_all <- score_index_all[score_index_all$learner != "average", ]

  # Every fit ran its task's kNNDM design in full (folds x repeats), and scored.
  key <- function(d) paste(d$sensor, d$unit, d$tag, d$source)
  want <- cv_index$iterations[match(key(score_index_all), key(cv_index))]
  note(all(score_index_all$n_iters == want),
       sprintf("%d fit(s) did not run their task's full kNNDM design", sum(score_index_all$n_iters != want)))
  # The drone designs match their prediction situation closely (W in metres).
  dr <- cv_index[cv_index$domain == "unit", ]
  if (nrow(dr)) note(all(dr$W_mean < 25), "a within-unit kNNDM design has W > 25 m: check its domain")
  note(all(is.finite(score_index_all$classif.acc)) && all(score_index_all$classif.acc > 0 & score_index_all$classif.acc <= 1),
       "non-finite or out-of-range accuracies")
  # Every (sensor, unit, tag, source) task has every configured learner.
  tab <- table(paste(score_index_all$sensor, score_index_all$unit, score_index_all$tag, score_index_all$source))
  n_learners <- length(resampling$learners)
  note(all(tab == n_learners), sprintf("%d task(s) missing learners", sum(tab != n_learners)))
  # The untuned baseline is present and not systematically ahead of tuned learners (7.23 regression).
  base <- score_index_all[score_index_all$learner == "ranger_untuned", ]
  tuned <- score_index_all[score_index_all$learner == "ranger", ]
  if (nrow(base) && nrow(tuned)) {
    m <- merge(base, tuned, by = c("sensor", "unit", "tag", "source"), suffixes = c("_b", "_t"))
    note(mean(m$classif.acc_t - m$classif.acc_b) > -0.05,
         "tuned ranger trails the untuned baseline by > 5 points on average: tuning is broken (finding 7.23)")
  }
  # Drone cubes verified against their reflectance grids.
  note(all(cube_index$n_bands > 0), "a drone cube reports zero bands")
  # No task lost more than half its rows to incomplete predictors (7.20 attrition made visible).
  if (all(c("n_in", "n_kept") %in% names(training_index))) {
    note(all(training_index$n_kept >= 0.5 * training_index$n_in),
         "a task lost more than half its training rows to incomplete predictors")
  }

  if (length(fail)) {
    stop("Pipeline checks failed:\n", paste0("  - ", fail, collapse = "\n"), call. = FALSE)
  }
  invisible(data.frame(fits = nrow(score_index_all), tasks = length(tab),
                       learners = n_learners, W_unit_max = if (nrow(dr)) max(dr$W_mean) else NA_real_))
}
