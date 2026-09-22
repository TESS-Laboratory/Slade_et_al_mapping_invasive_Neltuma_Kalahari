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
#' @param class_index per-fit Neltuma recall/precision (drone arm)
#' @param wv2_scores,sat_scores satellite benchmark scores, all arms
#' @param sat_class_index per-fit Neltuma recall for the satellite arms
#' @param wv2_drone_areas per-site drone vs WV2 class areas (S10 producer)
#' @param wv2_confusion,wv2_confusion_raw the S10 matrices, smoothed and raw
#' @param wv2_phase_table invasion-phase areas (Table 1 producer)
#' @param plant_validation_summary plant-scale accuracy (Table S9 producer)
#' @return named list consumed by paper/manuscript.qmd
build_paper_values <- function(score_index, best_models, class_areas, training_index,
                               class_index = NULL, wv2_scores = NULL, sat_scores = NULL,
                               sat_class_index = NULL, wv2_drone_areas = NULL,
                               wv2_confusion = NULL, wv2_confusion_raw = NULL,
                               wv2_phase_table = NULL, plant_validation_summary = NULL,
                               cover_area_index = NULL, cover_coverage_index = NULL,
                               cover_phase_index = NULL) {
  pct0 <- function(x) sprintf("%.0f", 100 * x)
  pct1 <- function(x) sprintf("%.1f", 100 * x)

  site_best <- stats::aggregate(classif.acc ~ site, best_models, max)
  lrn_mean  <- stats::aggregate(classif.acc ~ learner, score_index, mean)
  lrn_mean  <- lrn_mean[order(-lrn_mean$classif.acc), ]
  stack_mean <- stats::aggregate(classif.acc ~ tag, score_index, mean)

  out <- list(
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
  if (!is.null(wv2_scores))
    out <- c(out, satellite_paper_values(best_models, class_index, wv2_scores, sat_scores,
                                         sat_class_index, wv2_drone_areas, wv2_confusion,
                                         wv2_confusion_raw, wv2_phase_table,
                                         plant_validation_summary))
  if (!is.null(cover_area_index))
    out <- c(out, cover_paper_values(cover_area_index, cover_coverage_index, cover_phase_index))
  out
}


#' Sub-pixel cover values for the manuscript (D11: cover as satellite primary)
#'
#' The continuous cover arm's landscape numbers, per sensor: PPI-corrected Neltuma
#' area with its stratified-rectifier CI, naive sum-of-fractions, AOA fraction,
#' empirical DI-stratified coverage at nominal 90%, and the cover-based invasion
#' phase shares. WorldView-2 is the primary satellite product; S2/Planet are the
#' sensor-grain comparison. Fields are `cover_<sensor>_<quantity>`.
#'
#' @param cover_area_index rbind of `cover_scene_area` rows (one per sensor)
#' @param cover_coverage_index rbind of `cover_coverage_table` rows
#' @param cover_phase_index rbind of `cover_phase_summary` rows
#' @return named list of formatted strings for the qmd
cover_paper_values <- function(cover_area_index, cover_coverage_index = NULL,
                               cover_phase_index = NULL) {
  ha0  <- function(x) sprintf("%.0f", x)
  pct1 <- function(x) sprintf("%.1f", x)
  ar <- cover_area_index
  cov <- cover_coverage_index; ph <- cover_phase_index
  cov90 <- function(s) {
    if (is.null(cov)) return(NA_character_)
    d <- cov[cov$sensor == s & abs(cov$alpha - 0.10) < 1e-9, ]
    if (nrow(d)) pct1(100 * d$overall[1]) else NA_character_
  }
  phpct <- function(s, p) {
    if (is.null(ph)) return(NA_character_)
    d <- ph[ph$sensor == s & ph$phase == p, ]
    if (nrow(d)) pct1(sum(d$pct_of_area)) else "0.0"
  }
  out <- list(cover_primary_sensor = "WorldView-2")
  for (s in intersect(c("wv2", "planet", "s2"), ar$sensor)) {
    r <- ar[ar$sensor == s, ][1, ]
    out[[paste0("cover_", s, "_ha")]]      <- ha0(r$ppi_ha)
    out[[paste0("cover_", s, "_lo")]]      <- ha0(r$ppi_lo_ha)
    out[[paste0("cover_", s, "_hi")]]      <- ha0(r$ppi_hi_ha)
    out[[paste0("cover_", s, "_naive")]]   <- ha0(r$naive_ha)
    out[[paste0("cover_", s, "_within_aoa")]] <- ha0(r$cover_aoa_ha)
    out[[paste0("cover_", s, "_aoa_pct")]] <- pct1(100 * r$aoa_frac)
    out[[paste0("cover_", s, "_ntrain")]]  <- format(r$n_overlap, big.mark = ",", trim = TRUE)
    out[[paste0("cover_", s, "_cov90")]]   <- cov90(s)
    out[[paste0("cover_", s, "_incursion_pct")]] <- phpct(s, "Initial Incursion")
    out[[paste0("cover_", s, "_expansion_pct")]] <- phpct(s, "Expansion")
    out[[paste0("cover_", s, "_dominance_pct")]] <- phpct(s, "Dominance")
  }
  out
}


#' Satellite-side values for the manuscript (sections 3.2-3.4, abstract)
#'
#' Same substitution policy as above. Where the manuscript's number depends
#' on the smoothing choice (S10, the phases) both surfaces are supplied and
#' the qmd states which it shows.
satellite_paper_values <- function(best_models, class_index, wv2_scores, sat_scores,
                                   sat_class_index, wv2_drone_areas, wv2_confusion,
                                   wv2_confusion_raw, wv2_phase_table,
                                   plant_validation_summary) {
  pct0 <- function(x) sprintf("%.0f", 100 * x)
  pct1 <- function(x) sprintf("%.1f", 100 * x)
  sc <- rbind(wv2_scores, sat_scores)
  best_of <- function(arm) { d <- sc[sc$site == arm, ]; d[which.max(d$classif.acc), ] }
  lrn_of  <- function(arm, learner) sc[sc$site == arm & sc$learner == learner, ]
  recall_of <- function(arm, learner) {
    d <- sat_class_index[sat_class_index$site == arm & sat_class_index$learner == learner, ]
    d$recall[1]
  }
  # Neltuma recall of the winning drone learner per site, averaged by stack
  key <- paste(best_models$site, best_models$tag, best_models$learner)
  ci  <- class_index[match(key, paste(class_index$site, class_index$tag, class_index$learner)), ]
  nel_by_stack <- tapply(ci$recall, best_models$tag, mean, na.rm = TRUE)

  mr <- xtabs(n_pixels ~ wv2_class + drone_class, wv2_confusion_raw)
  woody <- c("5", "6", "7")
  # A class can be absent from a matrix (the fast profile's aggregated
  # surfaces, or a sensor without S.mellifera): index by name, tolerating
  # absence, instead of failing with "subscript out of bounds".
  cell <- function(mat, rows, cols) {
    r <- intersect(rows, rownames(mat)); c <- intersect(cols, colnames(mat))
    if (!length(r) || !length(c)) return(0)
    sum(mat[r, c, drop = FALSE])
  }
  nel_recall <- function(mat) { d <- cell(mat, "1", colnames(mat)); if (d > 0) cell(mat, "1", "1") / d else NA_real_ }
  woody_share <- function(mat) { d <- cell(mat, rownames(mat), "1"); if (d > 0) cell(mat, woody, "1") / d else NA_real_ }
  # The 34.6%-analogue and recall are the column ("drone says Neltuma") view.
  drone_recall <- function(mat) { d <- cell(mat, rownames(mat), "1"); if (d > 0) cell(mat, "1", "1") / d else NA_real_ }
  nel_area <- function(sensor, surface) {
    d <- wv2_drone_areas[wv2_drone_areas$Type == 1 & wv2_drone_areas$sensor == sensor &
                         wv2_drone_areas$surface == surface, ]
    sum(d$area_ha)
  }
  ph <- function(surface, phase) {
    r <- wv2_phase_table[wv2_phase_table$surface == surface & wv2_phase_table$phase == phase, ]
    if (!nrow(r)) data.frame(area_ha = 0, pct_of_area = 0) else r
  }
  wa <- best_of("wv2_archived"); wf <- best_of("wv2_field")
  pa <- best_of("planet_archived"); sa <- best_of("s2_archived")
  s9 <- plant_validation_summary[plant_validation_summary$surface == "raw" &
                                 plant_validation_summary$Type == 1, ]
  list(
    neltuma_drone_pct       = pct0(max(nel_by_stack)),
    neltuma_drone_stack     = names(nel_by_stack)[which.max(nel_by_stack)],
    neltuma_drone_chm_pct   = pct1(if ("5_CHM" %in% names(nel_by_stack)) nel_by_stack[["5_CHM"]] else NA_real_),
    wv2_best_pct            = pct1(wa$classif.acc), wv2_best_learner = wa$learner,
    wv2_svm_pct             = pct1(lrn_of("wv2_archived", "svm")$classif.acc),
    wv2_ensemble_pct        = pct1(lrn_of("wv2_archived", "ensemble")$classif.acc),
    wv2_field_pct           = pct1(wf$classif.acc), wv2_field_learner = wf$learner,
    wv2_field_minus_purity  = sprintf("%+.1f", 100 * (wf$classif.acc - wa$classif.acc)),
    wv2_neltuma_recall_pct  = pct0(recall_of("wv2_archived", wa$learner)),
    wv2_field_neltuma_recall_pct = pct0(recall_of("wv2_field", wf$learner)),
    planet_best_pct         = pct1(pa$classif.acc), planet_best_learner = pa$learner,
    s2_best_pct             = pct1(sa$classif.acc), s2_best_learner = sa$learner,
    planet_neltuma_recall_pct = pct0(recall_of("planet_archived", pa$learner)),
    s2_neltuma_recall_pct   = pct0(recall_of("s2_archived", sa$learner)),
    s10_over_raw_pct        = sprintf("%.1f", 100 * (nel_area("wv2", "raw") / nel_area("drone", "raw") - 1)),
    s10_woody_raw_pct       = pct1(woody_share(mr)),
    s10_recall_raw          = sprintf("%.2f", drone_recall(mr)),
    dominance_km2_raw       = sprintf("%.0f", ph("raw", "Dominance")$area_ha / 100),
    dominance_pct_raw       = sprintf("%.1f", ph("raw", "Dominance")$pct_of_area),
    expanding_km2_raw       = sprintf("%.0f", (ph("raw", "Expansion")$area_ha + ph("raw", "Initial Incursion")$area_ha) / 100),
    expanding_pct_raw       = sprintf("%.1f", ph("raw", "Expansion")$pct_of_area + ph("raw", "Initial Incursion")$pct_of_area),
    s9_neltuma_pct          = pct1(s9$accuracy), s9_neltuma_n = s9$n
  )
}


#' Overall accuracy and Neltuma recall per sensor, for Figure 7E
#'
#' Drone = the chosen site's winner on the prediction stack; satellites =
#' the archived arm's best learner. Returns a named list sensor -> c(overall,
#' neltuma_recall) in the order the figure draws them.
sensor_accuracy_summary <- function(site, tag, best_models, class_index,
                                    wv2_scores, sat_scores, wv2_class_index,
                                    sat_class_index) {
  b <- best_models[best_models$site == site & best_models$tag == tag, ][1, ]
  ci <- class_index[class_index$site == site & class_index$tag == tag &
                    class_index$learner == b$learner, ][1, ]
  sc <- rbind(wv2_scores, sat_scores); cx <- rbind(wv2_class_index, sat_class_index)
  arm <- function(a) {
    d <- sc[sc$site == a, ]; w <- d[which.max(d$classif.acc), ]
    r <- cx[cx$site == a & cx$learner == w$learner, ]$recall[1]
    c(w$classif.acc, r)
  }
  list(drone = c(b$classif.acc, ci$recall), wv2 = arm("wv2_archived"),
       planet = arm("planet_archived"), s2 = arm("s2_archived"))
}
