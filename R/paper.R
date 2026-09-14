#' Values the manuscript reads from the pipeline
#'
#' The paper is a pipeline product: paper/manuscript.qmd carries the original
#' text VERBATIM, and every number the pipeline can compute is an inline
#' expression reading this list. Substitution policy (decision 2026-09-14
#' [HUGH]): numbers are substituted only where the surrounding sentence stays
#' TRUE; where our results contradict a claim (learner rankings, class-minimum
#' assertions), the original text stands and a PIPELINE-CONTRADICTS comment in
#' the qmd carries the replacement for an authorial decision. Text is never
#' changed silently.
#'
#' @param score_index all learner x task scores
#' @param best_models per site x stack winners
#' @param class_areas raw-surface class areas
#' @param training_index per-task training summaries
#' @return named list consumed by paper/manuscript.qmd
build_paper_values <- function(score_index, best_models, class_areas, training_index) {
  pct0 <- function(x) sprintf("%.0f", 100 * x)
  pct1 <- function(x) sprintf("%.1f", 100 * x)

  site_best <- stats::aggregate(classif.acc ~ site, best_models, max)
  lrn_mean  <- stats::aggregate(classif.acc ~ learner, score_index, mean)
  lrn_mean  <- lrn_mean[order(-lrn_mean$classif.acc), ]
  stack_mean <- stats::aggregate(classif.acc ~ tag, score_index, mean)

  list(
    n_sites            = length(unique(score_index$site)),
    n_learners         = length(unique(score_index$learner)),
    n_outer_iterations = max(score_index$n_iters),
    site_best_min      = pct0(min(site_best$classif.acc)),
    site_best_max      = pct0(max(site_best$classif.acc)),
    grand_mean_pct     = pct1(mean(score_index$classif.acc)),
    best_stack         = stack_mean$tag[which.max(stack_mean$classif.acc)],
    best_stack_mean    = pct1(max(stack_mean$classif.acc)),
    learner_top1       = lrn_mean$learner[1],
    learner_top1_pct   = pct1(lrn_mean$classif.acc[1]),
    learner_top2       = lrn_mean$learner[2],
    learner_top2_pct   = pct1(lrn_mean$classif.acc[2]),
    fig4_headline_pct  = pct0(max(stack_mean$classif.acc)),
    n_field_total      = sum(training_index$n[training_index$tag ==
                               training_index$tag[1]]),
    min_class_n        = min(training_index$min_class_n)
  )
}
