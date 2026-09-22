#' Values the manuscript reads from the pipeline
#'
#' The paper is a pipeline product: paper/manuscript.qmd and paper/supplement.qmd
#' read this one list, and every number in them is an inline expression. Fields
#' are grouped by arm: drone classification, discrete satellite classification
#' (the comparison "foil"), and the sub-pixel cover product. Everything is a
#' formatted string ready to drop into prose; the tables the qmds render come from
#' their own targets (`sensor_summary`, `cover_phase_index`, ...).
#'
#' Rewritten 2026-09-22 with the manuscript rewrite: the previous version kept the
#' submitted numbers and boxed the contradictions; this one supplies the numbers
#' the rewritten text needs (finding list in paper/changes-from-submitted.md).
#'
#' @param score_index drone learner x task scores (incl. the soft-vote "average")
#' @param best_models per site x stack winners (drone)
#' @param class_index per-fit Neltuma recall/precision (drone arm)
#' @param training_index per-task drone training summaries (field tables)
#' @param training_class_counts per site x class field counts
#' @param cv_index the kNNDM design table (W per task)
#' @param wv2_scores,sat_scores satellite benchmark scores, all arms
#' @param sat_class_index per-fit Neltuma recall for the satellite arms (all sensors)
#' @param softvote_scores,softvote_classes the averaged model's own scores/recall
#' @param confusions named list (wv2, planet, s2) of pixel-level sensor-vs-drone
#'   confusion tables (`wv2_drone_confusion()` output)
#' @param conformal_coverage_honest cross-conformal coverage/set-size table
#' @param learner_area_index per-learner class areas of every predicted unit
#' @param subpixel_stats per-sensor shares of pixels with any / pure Neltuma (Fig 5)
#' @param cover_area_index,cover_coverage_index,cover_site_coverage_index,
#'   cover_error_index,cover_phase_index,cover_gradient_index the cover arm tables
#' @param cover_calibrator the Platt calibrator (for its coefficients)
#' @param resampling the resolved resampling config (tuning budget)
#' @param sensors_cfg sensors.yml (class sizes, purity thresholds)
#' @param pred_tag the mapped drone stack
#' @return named list consumed by the qmds
build_paper_values <- function(score_index, best_models, class_index, training_index,
                               training_class_counts = NULL, cv_index = NULL,
                               wv2_scores = NULL, sat_scores = NULL, sat_class_index = NULL,
                               softvote_scores = NULL, softvote_classes = NULL,
                               confusions = NULL, conformal_coverage_honest = NULL,
                               learner_area_index = NULL, subpixel_stats = NULL,
                               cover_area_index = NULL, cover_coverage_index = NULL,
                               cover_site_coverage_index = NULL, cover_error_index = NULL,
                               cover_phase_index = NULL, cover_gradient_index = NULL,
                               cover_calibrator = NULL, resampling = NULL, sensors_cfg = NULL,
                               wv2_phase_table = NULL, pred_tag = "5_CHM_ALLVI") {
  pct0 <- function(x) sprintf("%.0f", 100 * x)
  pct1 <- function(x) sprintf("%.1f", 100 * x)
  num1 <- function(x) sprintf("%.1f", x)
  int  <- function(x) format(round(x), big.mark = ",", trim = TRUE)

  # ---- drone classification -------------------------------------------------
  fits <- score_index[score_index$learner != "average", ]      # the fitted learners
  site_best <- stats::aggregate(classif.acc ~ site, best_models, max)
  lrn_mean  <- stats::aggregate(classif.acc ~ learner, fits, mean)
  lrn_mean  <- lrn_mean[order(-lrn_mean$classif.acc), ]
  stack_win <- stats::aggregate(classif.acc ~ tag, best_models, mean)   # per-site winners by stack
  stack_all <- stats::aggregate(classif.acc ~ tag, fits, mean)
  avg_rows  <- score_index[score_index$learner == "average", ]
  # Neltuma recall of the winning drone learner per site, averaged by stack
  key <- paste(best_models$site, best_models$tag, best_models$learner)
  ci  <- class_index[match(key, paste(class_index$site, class_index$tag, class_index$learner)), ]
  nel_by_stack <- tapply(ci$recall, best_models$tag, mean, na.rm = TRUE)

  out <- list(
    n_sites            = length(unique(fits$site)),
    n_learners         = length(unique(fits$learner)),
    n_learners_tuned   = length(setdiff(unique(fits$learner), "ranger_untuned")),
    n_tasks            = length(unique(paste(fits$site, fits$tag))),
    n_outer_iterations = max(fits$n_iters, na.rm = TRUE),
    site_best_min      = pct0(min(site_best$classif.acc)),
    site_best_max      = pct0(max(site_best$classif.acc)),
    grand_mean_pct     = pct1(mean(fits$classif.acc)),
    stack_best         = stack_win$tag[which.max(stack_win$classif.acc)],
    stack_best_pct     = pct1(max(stack_win$classif.acc)),
    stack_min_pct      = pct1(min(stack_win$classif.acc)),
    stack_all_best     = stack_all$tag[which.max(stack_all$classif.acc)],
    learner_top1       = lrn_mean$learner[1],
    learner_top1_pct   = pct1(lrn_mean$classif.acc[1]),
    learner_top2       = lrn_mean$learner[2],
    learner_top2_pct   = pct1(lrn_mean$classif.acc[2]),
    learner_bottom_pct = pct1(min(lrn_mean$classif.acc)),
    average_pct        = if (nrow(avg_rows)) pct1(mean(avg_rows$classif.acc)) else NA_character_,
    n_clear_wins       = sum(best_models$clear_win, na.rm = TRUE),
    neltuma_drone_pct  = pct0(max(nel_by_stack)),
    neltuma_drone_stack = names(nel_by_stack)[which.max(nel_by_stack)],
    n_field_total      = sum(training_index$n[training_index$tag == training_index$tag[1]]),
    min_class_n        = min(training_index$min_class_n)
  )
  if (!is.null(training_class_counts)) {
    out$n_site_class_combos <- nrow(training_class_counts)
    out$n_site_class_below20 <- sum(training_class_counts$n < 20)
  }
  if (!is.null(softvote_scores)) {
    a <- softvote_scores[softvote_scores$sensor == "drone" & softvote_scores$tag == pred_tag, ]
    out$avg_acc_min <- pct0(min(a$classif.acc)); out$avg_acc_max <- pct0(max(a$classif.acc))
    out$avg_acc_mean <- pct1(mean(a$classif.acc))
  }
  if (!is.null(softvote_classes)) {
    r <- softvote_classes[softvote_classes$sensor == "drone" & softvote_classes$tag == pred_tag, ]
    out$avg_neltuma_recall_min <- pct0(min(r$recall, na.rm = TRUE))
    out$avg_neltuma_recall_max <- pct0(max(r$recall, na.rm = TRUE))
    out$avg_neltuma_recall_pct <- pct0(stats::weighted.mean(r$recall, r$n_truth, na.rm = TRUE))
  }
  if (!is.null(cv_index)) {
    dr <- cv_index[cv_index$sensor == "drone", ]; st <- cv_index[cv_index$sensor != "drone", ]
    out$k_unit <- unique(dr$k_outer)[1]; out$k_aoi <- unique(st$k_outer)[1]
    out$n_repeats <- unique(cv_index$repeats)[1]
    out$n_outer_iterations_sat <- max(st$iterations)
    out$drone_W_min_m <- num1(min(dr$W_mean)); out$drone_W_max_m <- num1(max(dr$W_mean))
    out$sat_W_min_km <- num1(min(st$W_mean) / 1000); out$sat_W_max_km <- num1(max(st$W_mean) / 1000)
  }
  if (!is.null(resampling)) out$tune_evals <- resampling$tuning$term_evals
  if (!is.null(conformal_coverage_honest)) {
    h <- conformal_coverage_honest[abs(conformal_coverage_honest$alpha - 0.10) < 1e-9, ]
    d <- h[h$site %in% unique(fits$site) & h$tag == pred_tag, ]     # the drone sites
    if (nrow(d)) {
      out$drone_setsize_min <- sprintf("%.2f", min(d$mean_set_size))
      out$drone_setsize_max <- sprintf("%.2f", max(d$mean_set_size))
      out$drone_cov90_min <- pct0(min(d$overall)); out$drone_cov90_max <- pct0(max(d$overall))
    }
  }

  # ---- discrete satellite classification (the foil) ------------------------
  if (!is.null(wv2_scores)) {
    out <- c(out, satellite_paper_values(rbind(wv2_scores, sat_scores), sat_class_index,
                                         conformal_coverage_honest, learner_area_index,
                                         confusions, subpixel_stats, sensors_cfg, cv_index))
  }
  if (!is.null(wv2_phase_table)) {
    pre <- wv2_phase_table[wv2_phase_table$phase == "Pre-Incursion", ]
    out$hc_wv2_preincursion_pct <- if (nrow(pre)) num1(pre$pct_of_area[1]) else NA_character_
  }
  # ---- sub-pixel cover ------------------------------------------------------
  if (!is.null(cover_area_index)) {
    out <- c(out, cover_paper_values(cover_area_index, cover_coverage_index,
                                     cover_site_coverage_index, cover_error_index,
                                     cover_phase_index, cover_gradient_index, cover_calibrator))
  }
  out
}


#' Field counts per site x class (Table S4 producer)
#'
#' @param tables named list site -> drone field training table (any stack)
#' @param classes class lookup (Type, Class)
#' @return data.frame(site, class, n)
training_class_count_table <- function(tables, classes) {
  do.call(rbind, lapply(names(tables), function(s) {
    tab <- table(as.integer(as.character(tables[[s]]$Type)))
    data.frame(site = s, Type = as.integer(names(tab)),
               class = classes$Class[match(as.integer(names(tab)), classes$Type)],
               n = as.integer(tab), stringsAsFactors = FALSE)
  }))
}


#' Shares of satellite pixels over the drone sites with any / pure Neltuma (Fig 5)
#'
#' @param exts named list (wv2, planet, s2) of purity-extraction tables (`frac_1`)
#' @param sensors_cfg sensors.yml
#' @return data.frame(sensor, n_pixels, any_pct, pure_pct, purity)
subpixel_stats_table <- function(exts, sensors_cfg) {
  do.call(rbind, lapply(names(exts), function(s) {
    fr <- exts[[s]]; if (inherits(fr, "sf")) fr <- sf::st_drop_geometry(fr)
    cov <- fr$frac_1; cov[is.na(cov)] <- 0
    thr <- sensors_cfg[[s]]$sources$purity_raw$purity
    data.frame(sensor = s, n_pixels = length(cov), any_pct = 100 * mean(cov > 0),
               pure_pct = 100 * mean(cov >= thr), purity = thr, stringsAsFactors = FALSE)
  }))
}


#' Pixel-level agreement of a satellite scene with the drone maps
#'
#' From the `wv2_drone_confusion()` matrix (rows = satellite class, columns =
#' majority drone class per satellite pixel): Neltuma recall, precision, the
#' satellite/drone Neltuma extent ratio, the share of drone-Neltuma pixels
#' labelled woody (the manuscript's "34.6%" analogue) and overall agreement.
#'
#' @param m confusion table (wv2_class, drone_class, n_pixels)
#' @param sensor label; @param neltuma_code Neltuma code; @param woody woody codes
#' @return one-row data.frame
pixel_agreement <- function(m, sensor = NA_character_, neltuma_code = 1L, woody = c(5L, 6L, 7L)) {
  tab <- stats::xtabs(n_pixels ~ wv2_class + drone_class, m)
  cell <- function(rows, cols) {
    r <- intersect(as.character(rows), rownames(tab)); c <- intersect(as.character(cols), colnames(tab))
    if (!length(r) || !length(c)) return(0)
    sum(tab[r, c, drop = FALSE])
  }
  nk <- neltuma_code
  tp <- cell(nk, nk); drone_n <- cell(rownames(tab), nk); sat_n <- cell(nk, colnames(tab))
  both <- intersect(rownames(tab), colnames(tab))
  data.frame(sensor = sensor, n = sum(tab),
             recall = if (drone_n) tp / drone_n else NA_real_,
             precision = if (sat_n) tp / sat_n else NA_real_,
             ratio = if (drone_n) sat_n / drone_n else NA_real_,
             woody_share = if (drone_n) cell(woody, nk) / drone_n else NA_real_,
             agreement = sum(diag(tab[both, both, drop = FALSE])) / sum(tab),
             stringsAsFactors = FALSE)
}


#' Satellite-side values for the manuscript (sections 3.2-3.3)
satellite_paper_values <- function(sc, sat_class_index, honest, learner_area_index,
                                   confusions, subpixel_stats, sensors_cfg, cv_index) {
  pct0 <- function(x) sprintf("%.0f", 100 * x)
  pct1 <- function(x) sprintf("%.1f", 100 * x)
  out <- list()
  arms <- c(field = "field", archived = "archived", purity = "purity_raw")
  for (s in c("wv2", "planet", "s2")) {
    for (a in names(arms)) {
      d <- sc[sc$sensor == s & sc$source == arms[[a]] & sc$learner != "average", ]
      if (!nrow(d)) next
      w <- d[which.max(d$classif.acc), ]
      out[[paste0("hc_", s, "_", a, "_pct")]] <- pct1(w$classif.acc)
      out[[paste0("hc_", s, "_", a, "_learner")]] <- w$learner
      r <- sat_class_index[sat_class_index$sensor == s & sat_class_index$source == arms[[a]] &
                           sat_class_index$learner == w$learner, ]
      out[[paste0("hc_", s, "_", a, "_recall_pct")]] <- if (nrow(r)) pct0(r$recall[1]) else NA_character_
      if (!is.null(cv_index)) {
        n <- cv_index$n[cv_index$sensor == s & cv_index$source == arms[[a]]]
        if (length(n)) out[[paste0("hc_", s, "_", a, "_ntrain")]] <- n[1]
      }
    }
    if (!is.null(honest)) {
      h <- honest[abs(honest$alpha - 0.10) < 1e-9 & honest$site == paste0(s, "_dr_raw"), ]
      if (nrow(h)) out[[paste0("hc_", s, "_setsize90")]] <- sprintf("%.1f", h$mean_set_size[1])
    }
    if (!is.null(sensors_cfg)) {
      out[[paste0("hc_", s, "_nclasses")]] <- length(sensors_cfg[[s]]$classes)
      out[[paste0("hc_", s, "_class_size")]] <- sensors_cfg[[s]]$sources$purity_raw$class_size
      out[[paste0("hc_", s, "_purity_pct")]] <- pct0(sensors_cfg[[s]]$sources$purity_raw$purity)
    }
    if (!is.null(learner_area_index)) {
      la <- learner_area_index[learner_area_index$site == paste0(s, "_scene") & learner_area_index$Type == 1, ]
      if (nrow(la)) {
        out[[paste0("hc_", s, "_area_avg")]] <- sprintf("%.0f", la$area_ha[la$learner == "average"][1])
        out[[paste0("hc_", s, "_area_min")]] <- sprintf("%.0f", min(la$area_ha[la$learner != "average"]))
        out[[paste0("hc_", s, "_area_max")]] <- sprintf("%.0f", max(la$area_ha[la$learner != "average"]))
      }
    }
    if (!is.null(confusions) && !is.null(confusions[[s]])) {
      px <- pixel_agreement(confusions[[s]], s)
      out[[paste0("px_", s, "_recall_pct")]] <- pct0(px$recall)
      out[[paste0("px_", s, "_precision_pct")]] <- pct0(px$precision)
      out[[paste0("px_", s, "_ratio")]] <- sprintf("%.1f", px$ratio)
      out[[paste0("px_", s, "_ratio_pct")]] <- sprintf("%.0f", 100 * abs(px$ratio - 1))
      out[[paste0("px_", s, "_woody_pct")]] <- pct0(px$woody_share)
      out[[paste0("px_", s, "_agree_pct")]] <- pct0(px$agreement)
    }
    if (!is.null(subpixel_stats)) {
      sp <- subpixel_stats[subpixel_stats$sensor == s, ]
      if (nrow(sp)) {
        out[[paste0("fig5_", s, "_any_pct")]] <- sprintf("%.1f", sp$any_pct)
        out[[paste0("fig5_", s, "_pure_pct")]] <- sprintf("%.2f", sp$pure_pct)
      }
    }
  }
  # the hard-class WV2 phase split (Figure S14) is supplied by the caller's table
  out$hc_wv2_field_minus_purity <- if (all(c("hc_wv2_field_pct", "hc_wv2_purity_pct") %in% names(out)))
    sprintf("%+.1f", as.numeric(out$hc_wv2_field_pct) - as.numeric(out$hc_wv2_purity_pct)) else NA_character_
  out
}


#' Sub-pixel cover values for the manuscript (section 3.4-3.5)
cover_paper_values <- function(cover_area_index, cover_coverage_index = NULL,
                               cover_site_coverage_index = NULL, cover_error_index = NULL,
                               cover_phase_index = NULL, cover_gradient_index = NULL,
                               cover_calibrator = NULL) {
  ha0  <- function(x) sprintf("%.0f", x)
  num1 <- function(x) sprintf("%.1f", x)
  ar <- cover_area_index; cov <- cover_coverage_index; scv <- cover_site_coverage_index
  er <- cover_error_index; ph <- cover_phase_index; gr <- cover_gradient_index
  phpct <- function(s, p, col = "pct_of_area") {
    if (is.null(ph)) return(NA_character_)
    d <- ph[ph$sensor == s & ph$phase == p, ]
    if (nrow(d)) num1(sum(d[[col]])) else "0.0"
  }
  grad <- function(s, feature, band) {
    if (is.null(gr)) return(NA_character_)
    d <- gr[gr$sensor == s & gr$feature == feature & gr$band == band, ]
    if (nrow(d)) num1(d$mean_cover_pct[1]) else NA_character_
  }
  out <- list(cover_primary_sensor = "WorldView-2")
  if (!is.null(cover_calibrator)) {
    cf <- attr(cover_calibrator, "coef")
    out$cal_slope <- sprintf("%.2f", cf[["slope"]]); out$cal_intercept <- sprintf("%.2f", cf[["intercept"]])
  }
  dense <- if (!is.null(er)) er[er$site == "struizendam_4", ] else NULL
  if (!is.null(dense) && nrow(dense)) out$dense_site_truth_pct <- num1(dense$mean_truth_pct[1])
  for (s in intersect(c("wv2", "planet", "s2"), ar$sensor)) {
    r <- ar[ar$sensor == s, ][1, ]
    out[[paste0("cover_", s, "_ha")]]      <- ha0(r$ppi_ha)
    out[[paste0("cover_", s, "_lo")]]      <- ha0(r$ppi_lo_ha)
    out[[paste0("cover_", s, "_hi")]]      <- ha0(r$ppi_hi_ha)
    out[[paste0("cover_", s, "_naive")]]   <- ha0(r$naive_ha)
    out[[paste0("cover_", s, "_within_aoa")]] <- ha0(r$cover_aoa_ha)
    out[[paste0("cover_", s, "_aoa_pct")]] <- num1(100 * r$aoa_frac)
    out[[paste0("cover_", s, "_mean_pct")]] <- sprintf("%.2f", 100 * r$cover_aoa_ha / r$aoa_ha)
    out[[paste0("cover_", s, "_bias_pp")]] <- sprintf("%+.2f", r$bias_pp)
    out[[paste0("cover_", s, "_p_below_aoa_pct")]] <- if ("p_below_aoa_sum" %in% names(r)) sprintf("%.0f", 100 * r$p_below_aoa_sum) else NA_character_
    out[[paste0("cover_", s, "_thr_rule")]] <- if ("threshold_rule" %in% names(r)) r$threshold_rule else NA_character_
    out[[paste0("cover_", s, "_ntrain")]]  <- format(r$n_overlap, big.mark = ",", trim = TRUE)
    if (!is.null(cov)) {
      d <- cov[cov$sensor == s & abs(cov$alpha - 0.10) < 1e-9, ]
      if (nrow(d)) {
        out[[paste0("cover_", s, "_cov90")]] <- num1(100 * d$apparent[1])
        out[[paste0("cover_", s, "_cov90_honest")]] <- num1(100 * d$honest[1])
        out[[paste0("cover_", s, "_width90_pp")]] <- num1(100 * d$mean_width[1])
      }
    }
    if (!is.null(scv)) {
      d <- scv[scv$sensor == s & abs(scv$alpha - 0.10) < 1e-9, ]
      if (nrow(d)) {
        out[[paste0("cover_", s, "_cov90_site_min")]] <- num1(100 * min(d$coverage, na.rm = TRUE))
        out[[paste0("cover_", s, "_cov90_site_max")]] <- num1(100 * max(d$coverage, na.rm = TRUE))
        ds <- d[d$site == "struizendam_4", ]
        if (nrow(ds)) out[[paste0("cover_", s, "_dense_cov90")]] <- num1(100 * ds$coverage[1])
      }
    }
    if (!is.null(er)) {
      d <- er[er$sensor == s, ]
      pooled <- d[d$site == "pooled", ]
      if (nrow(pooled)) {
        out[[paste0("cover_", s, "_rmse_pp")]] <- num1(pooled$rmse_pp[1])
        out[[paste0("cover_", s, "_r2")]] <- sprintf("%.2f", pooled$r2[1])
        out[[paste0("cover_", s, "_floor_pct")]] <- num1(pooled$noise_floor_pct[1])
      }
      ds <- d[d$site == "struizendam_4", ]
      if (nrow(ds)) out[[paste0("cover_", s, "_dense_bias_pp")]] <- num1(abs(ds$bias_pp[1]))
    }
    out[[paste0("cover_", s, "_incursion_pct")]] <- phpct(s, "Initial Incursion")
    out[[paste0("cover_", s, "_expansion_pct")]] <- phpct(s, "Expansion")
    out[[paste0("cover_", s, "_dominance_pct")]] <- phpct(s, "Dominance")
    out[[paste0("cover_", s, "_dominance_hi_pct")]] <- phpct(s, "Dominance", "pct_upper")
    out[[paste0("cover_", s, "_preincursion_lo_pct")]] <- phpct(s, "Pre-Incursion", "pct_lower")
    out[[paste0("cover_", s, "_beyond_aoa_pct")]] <- phpct(s, "Beyond applicability")
    out[[paste0("cover_", s, "_below_floor_pct")]] <- phpct(s, "Below detection floor")
    out[[paste0("cover_", s, "_road_near_pct")]] <- grad(s, "road", "0-250 m")
    out[[paste0("cover_", s, "_road_far_pct")]]  <- grad(s, "road", "1-3 km")
    out[[paste0("cover_", s, "_sett_near_pct")]] <- grad(s, "settlement", "0-1 km")
    out[[paste0("cover_", s, "_sett_far_pct")]]  <- grad(s, "settlement", "3-6 km")
  }
  out
}


#' Table 2: one row per sensor across both satellite arms
#'
#' @param sensors_cfg sensors.yml; @param cv_index kNNDM designs
#' @param confusions named list of sensor-vs-drone confusion tables
#' @param honest cross-conformal coverage table
#' @param cover_area_index,cover_coverage_index,cover_error_index cover tables
#' @return data.frame in manuscript column order
sensor_results_table <- function(sensors_cfg, cv_index, confusions, honest,
                                 cover_area_index, cover_coverage_index, cover_error_index) {
  lab <- c(wv2 = "WorldView-2", planet = "PlanetScope", s2 = "Sentinel-2")
  do.call(rbind, lapply(intersect(names(lab), cover_area_index$sensor), function(s) {
    ar <- cover_area_index[cover_area_index$sensor == s, ][1, ]
    px <- if (!is.null(confusions[[s]])) pixel_agreement(confusions[[s]], s) else NULL
    h <- honest[abs(honest$alpha - 0.10) < 1e-9 & honest$site == paste0(s, "_dr_raw"), ]
    cv <- cover_coverage_index[cover_coverage_index$sensor == s & abs(cover_coverage_index$alpha - 0.10) < 1e-9, ]
    er <- cover_error_index[cover_error_index$sensor == s & cover_error_index$site == "pooled", ]
    W <- cv_index$W_mean[cv_index$sensor == s & cv_index$source == "purity_raw"]
    data.frame(sensor = lab[[s]], grain_m = sensors_cfg[[s]]$pixel_m,
               train_cells = ar$n_overlap, W_km = if (length(W)) W[1] / 1000 else NA_real_,
               recall_pct = if (!is.null(px)) 100 * px$recall else NA_real_,
               precision_pct = if (!is.null(px)) 100 * px$precision else NA_real_,
               set_size = if (nrow(h)) h$mean_set_size[1] else NA_real_,
               rmse_pp = if (nrow(er)) er$rmse_pp[1] else NA_real_,
               coverage_pct = if (nrow(cv)) 100 * cv$honest[1] else NA_real_,
               aoa_pct = 100 * ar$aoa_frac,
               area_ha = ar$ppi_ha, ci_lo = ar$ppi_lo_ha, ci_hi = ar$ppi_hi_ha,
               stringsAsFactors = FALSE)
  }))
}


#' Overall accuracy and Neltuma recall per sensor (legacy Figure 7E producer)
#'
#' Retained for the SI benchmark figure; the main-text sensor panel now draws
#' pixel-level recall/precision against the drone maps (`sensor_pixel_summary`).
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
  list(drone = c(b$classif.acc, ci$recall), wv2 = arm("wv2_dr_raw"),
       planet = arm("planet_dr_raw"), s2 = arm("s2_dr_raw"))
}


#' Pixel-level recall/precision vs the drone maps and set size, per sensor (Fig 6E)
#'
#' @param confusions named list (wv2, planet, s2) of confusion tables
#' @param honest cross-conformal coverage table
#' @param drone_row the drone's own recall/precision (soft-vote at the shown site)
#' @return data.frame(sensor, recall, precision, set_size)
sensor_pixel_summary <- function(confusions, honest, drone_row = NULL) {
  rows <- lapply(names(confusions), function(s) {
    px <- pixel_agreement(confusions[[s]], s)
    h <- honest[abs(honest$alpha - 0.10) < 1e-9 & honest$site == paste0(s, "_dr_raw"), ]
    data.frame(sensor = s, recall = px$recall, precision = px$precision,
               set_size = if (nrow(h)) h$mean_set_size[1] else NA_real_, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  if (!is.null(drone_row)) out <- rbind(drone_row, out)
  out
}
