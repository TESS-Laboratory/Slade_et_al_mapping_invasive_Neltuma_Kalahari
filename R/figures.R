#' Figures: landscape classification maps
#'
#' The Figure 4A analogue: per-site classified surfaces from the winning
#' learner, on the validated class palette.
#'
#' Colors come from classes.json - the palette there is CVD-validated (all 36
#' pairs of the nine map classes reach OKLab dE >= 15 for normal vision and
#' >= 8 under protan/deutan simulation; finding 7.30). Neltuma is magenta
#' precisely so the invader survives red-green CVD against every woody class,
#' including the V. erioloba confusion pair the paper turns on. Do not restyle
#' by eye; rerun the validation if any hex changes.

#' Class code -> color, from classes.json
#'
#' @return named character vector, names are class codes
class_palette <- function(path = CLASSES_JSON) {
  j <- read_class_scheme(path)
  cols <- vapply(j$classes, function(x) x$color, character(1))
  names(cols) <- vapply(j$classes, function(x) as.character(x$code), character(1))
  cols
}


#' Legend labels as plotmath expressions
#'
#' label_md is markdown ("*Neltuma*"); ggplot legends need plotmath for
#' italics without pulling in ggtext. Binomials (fully starred labels) become
#' italic(...); everything else stays plain. Serves finding 8.6: labels always
#' come from the scheme, so the Rhigosum misspelling cannot recur.
#'
#' @param codes integer class codes
#' @return list of expressions, one per code
class_plot_labels <- function(codes, path = CLASSES_JSON) {
  md <- class_labels(codes, path = path)
  lapply(md, function(x) {
    if (grepl("^\\*.*\\*$", x)) {
      bquote(italic(.(gsub("\\*", "", x))))
    } else {
      x
    }
  })
}


#' One site's map panel
#'
#' The full-resolution surface is modal-aggregated to roughly `target_px`
#' pixels across before plotting - a 9,000-16,000 px raster into a ~1,400 px
#' panel would alias arbitrarily otherwise, and modal keeps it categorical.
#'
#' @param class_tif path to the site's _class.tif
#' @param site site id, for the title
#' @param subtitle annotation under the title (learner, accuracy)
#' @param palette from class_palette()
#' @param target_px approximate panel width in source pixels after aggregation
#' @return a ggplot
map_panel <- function(class_tif, site, subtitle, palette, target_px = 1400,
                      scale_m = 100) {
  r <- terra::rast(class_tif)
  fact <- max(1L, floor(terra::ncol(r) / target_px))
  if (fact > 1L) r <- terra::aggregate(r, fact = fact, fun = "modal", na.rm = TRUE)

  df <- terra::as.data.frame(r, xy = TRUE, na.rm = TRUE)
  names(df)[3] <- "code"
  df$code <- factor(as.integer(df$code))

  # Scale bar, bottom-left: 100 m for a drone site, km for a satellite scene.
  e <- terra::ext(r)
  bar_x <- e$xmin + 0.05 * (e$xmax - e$xmin)
  bar_y <- e$ymin + 0.05 * (e$ymax - e$ymin)

  ggplot2::ggplot(df, ggplot2::aes(x = x, y = y, fill = code)) +
    ggplot2::geom_raster() +
    ggplot2::scale_fill_manual(values = palette, drop = TRUE) +
    ggplot2::annotate("segment", x = bar_x, xend = bar_x + scale_m,
                      y = bar_y, yend = bar_y, linewidth = 1.2, colour = "grey15") +
    ggplot2::annotate("text", x = bar_x + scale_m / 2, y = bar_y,
                      label = if (scale_m >= 1000) paste0(scale_m / 1000, " km") else paste0(scale_m, " m"), vjust = -0.8, size = 2.6, colour = "grey15") +
    ggplot2::coord_equal(expand = FALSE) +
    ggplot2::labs(title = gsub("_", " ", tools::toTitleCase(site)),
                  subtitle = subtitle) +
    ggplot2::theme_void(base_size = 9) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 9, hjust = 0),
      plot.subtitle = ggplot2::element_text(size = 7, colour = "grey35", hjust = 0),
      panel.border = ggplot2::element_rect(colour = "grey80", fill = NA,
                                           linewidth = 0.3),
      legend.position = "none",
      plot.margin = ggplot2::margin(2, 4, 2, 4)
    )
}


#' The combined seven-site map figure
#'
#' @param pred_paths named list/vector: site -> c(class_tif, prob_tif)
#' @param best_models the selection table, for per-panel annotations
#' @param tag the predicted stack tag
#' @param out_png output path
#' @return the output path
fig_landscape_maps <- function(pred_paths, best_models, tag,
                               out_png = "data-out/figures/fig_maps.png") {
  dir.create(dirname(out_png), recursive = TRUE, showWarnings = FALSE)
  pal <- class_palette()

  sites <- names(pred_paths)
  panels <- lapply(sites, function(s) {
    b <- best_models[best_models$site == s & best_models$tag == tag, , drop = FALSE]
    sub <- sprintf("%s  acc %.2f ± %.2f", b$learner, b$classif.acc, b$acc_sd)
    map_panel(pred_paths[[s]][1], s, sub, pal)
  })

  # Shared legend built from the codes actually present across all surfaces,
  # in code order, labels from the class scheme (8.6).
  codes <- sort(unique(unlist(lapply(pred_paths, function(p) {
    terra::unique(terra::rast(p[1]))[[1]]
  }))))
  leg_df <- data.frame(code = factor(codes), y = seq_along(codes))
  leg <- ggplot2::ggplot(leg_df, ggplot2::aes(x = 1, y = y, fill = code)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_manual(
      values = pal, name = NULL,
      labels = class_plot_labels(codes),
      guide = ggplot2::guide_legend(ncol = 1, override.aes = list(colour = "grey70"))
    ) +
    ggplot2::theme_void(base_size = 9) +
    ggplot2::theme(legend.position = "right",
                   legend.key.size = ggplot2::unit(4.5, "mm"),
                   legend.text = ggplot2::element_text(size = 8))
  legend_grob <- suppressWarnings(patchwork::wrap_elements(
    full = cowplot_get_legend(leg)))

  fig <- patchwork::wrap_plots(c(panels, list(legend_grob)), ncol = 4) +
    patchwork::plot_annotation(
      title = "Drone-derived vegetation classification, winning learner per site",
      subtitle = sprintf("stack %s · 10×10 repeated spatial CV · %s",
                         tag, "colours CVD-validated"),
      theme = ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", size = 12),
        plot.subtitle = ggplot2::element_text(size = 9, colour = "grey35")
      )
    )

  # device pinned to cairo png: the installed ragg fails with "Graphics API
  # version mismatch" against this R's graphics engine, and ggsave auto-selects
  # ragg when present. Cairo renders identically for a raster map.
  ggplot2::ggsave(out_png, fig, width = 13, height = 7.2, dpi = 200, bg = "white",
                  device = grDevices::png, type = "cairo")
  out_png
}


#' Extract a legend grob without depending on cowplot
#'
#' @param p a ggplot with a legend
#' @return the legend grob
cowplot_get_legend <- function(p) {
  g <- ggplot2::ggplotGrob(p)
  hit <- which(vapply(g$grobs, function(x) grepl("guide", x$name), logical(1)))
  if (!length(hit)) stop("No legend found to extract.", call. = FALSE)
  g$grobs[[hit[1]]]
}


#' Accuracy figure: the Figure 4B analogue, with the spread the paper omitted
#'
#' Two panels. (A) accuracy by predictor stack - the paper's Fig 4B claim -
#' shown as the seven per-site winning accuracies plus their mean, so the
#' ordering carries its uncertainty instead of a bare bar. (B) accuracy by
#' learner across all 28 site x stack tasks, which is the linearity result
#' (7.28) in one picture.
#'
#' Design notes: identity lives on the y axis, so no legend and no categorical
#' palette are needed - grey observation dots, one accent for the mean, means
#' direct-labelled. The dashed reference line is the manuscript's "~90%%
#' overall accuracy" claim, so the figure states the comparison it invites.
#'
#' @param best_models the per site x stack winners
#' @param score_index all learner x task scores
#' @param out_png output path
#' @return the output path
fig_accuracy <- function(best_models, score_index,
                         out_png = "data-out/figures/fig_accuracy.png") {
  dir.create(dirname(out_png), recursive = TRUE, showWarnings = FALSE)
  accent <- "#B5179E"   # the Neltuma magenta doubles as the house accent
  ink <- "grey25"

  base_theme <- ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(colour = "grey92"),
      axis.title.y = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold", size = 10),
      plot.subtitle = ggplot2::element_text(size = 8, colour = "grey40")
    )

  # -- A: by stack, per-site winners ----------------------------------------
  a <- best_models
  a_mean <- stats::aggregate(classif.acc ~ tag, a, mean)
  a$tag <- factor(a$tag, levels = a_mean$tag[order(a_mean$classif.acc)])
  a_mean$tag <- factor(a_mean$tag, levels = levels(a$tag))

  pA <- ggplot2::ggplot(a, ggplot2::aes(x = classif.acc, y = tag)) +
    ggplot2::geom_vline(xintercept = 0.9, linetype = "22",
                        colour = "grey70", linewidth = 0.4) +
    ggplot2::geom_point(colour = "grey55", size = 1.8, alpha = 0.8) +
    ggplot2::geom_point(data = a_mean, colour = accent, size = 3.2) +
    ggplot2::geom_text(data = a_mean,
                       ggplot2::aes(label = sprintf("%.3f", classif.acc)),
                       vjust = -1.1, size = 2.9, colour = ink) +
    ggplot2::annotate("text", x = 0.9, y = 0.6, label = "manuscript “~90%”",
                      hjust = -0.05, size = 2.6, colour = "grey55") +
    ggplot2::scale_x_continuous(limits = c(NA, 1)) +
    ggplot2::labs(title = "A · Accuracy by predictor stack",
                  subtitle = "winning learner per site (grey) and stack mean (magenta)",
                  x = "overall accuracy, 10×10 repeated spatial CV") +
    base_theme

  # -- B: by learner, all tasks ---------------------------------------------
  b <- score_index
  b_mean <- stats::aggregate(classif.acc ~ learner, b, mean)
  b$learner <- factor(b$learner, levels = b_mean$learner[order(b_mean$classif.acc)])
  b_mean$learner <- factor(b_mean$learner, levels = levels(b$learner))

  pB <- ggplot2::ggplot(b, ggplot2::aes(x = classif.acc, y = learner)) +
    ggplot2::geom_point(colour = "grey55", size = 1.4, alpha = 0.55,
                        position = ggplot2::position_jitter(height = 0.12, seed = 1)) +
    ggplot2::geom_point(data = b_mean, colour = accent, size = 3.2) +
    ggplot2::geom_text(data = b_mean,
                       ggplot2::aes(label = sprintf("%.3f", classif.acc)),
                       vjust = -1.1, size = 2.9, colour = ink) +
    ggplot2::labs(title = "B · Accuracy by learner",
                  subtitle = "all 28 site × stack tasks (grey) and learner mean (magenta)",
                  x = "overall accuracy, 10×10 repeated spatial CV") +
    base_theme

  # explicit namespacing: patchwork is installed but not attached in workers,
  # and ggplot2 4.x dispatches `/` through S7 only when patchwork is attached
  fig <- patchwork::wrap_plots(list(pA, pB), ncol = 1, heights = c(1, 1.4))
  ggplot2::ggsave(out_png, fig, width = 7.2, height = 6.4, dpi = 200,
                  bg = "white", device = grDevices::png, type = "cairo")
  out_png
}


#' Figure 5 analogue: sub-pixel Neltuma cover per sensor grain
#'
#' One histogram per sensor of the Neltuma fraction inside each satellite
#' pixel over the drone sites, from the purity extractions (raw drone
#' surfaces). Zero-cover pixels dominate every sensor and would flatten the
#' plot, so the panels show pixels with any Neltuma and state the zero share
#' and the share reaching that sensor's training purity threshold directly -
#' the numbers the caption's claim ("coarser grain sharply reduces
#' high-cover pixels") actually rests on. Single series, so no legend; the
#' Neltuma magenta is the only hue.
#'
#' @param exts named list (wv2, planet, s2) of purity-extraction tables with
#'   a `frac_1` column
#' @param sensors the sensors table (pixel_m, purity_threshold)
#' @param out_png output path
#' @return `out_png`
fig_subpixel_cover <- function(exts, sensors, out_png = "data-out/figures/fig5_subpixel_cover.png") {
  accent <- "#B5179E"
  labels <- c(wv2 = "WorldView-2", planet = "PlanetScope", s2 = "Sentinel-2")
  rows <- lapply(names(exts), function(s) {
    fr <- exts[[s]]
    if (inherits(fr, "sf")) fr <- sf::st_drop_geometry(fr)
    cov <- fr$frac_1; cov[is.na(cov)] <- 0
    px  <- sensors$pixel_m[sensors$sensor == s]
    thr <- sensors$purity_threshold[sensors$sensor == s]
    list(
      data = data.frame(sensor = s, cover = cov[cov > 0]),
      note = data.frame(
        sensor = s,
        label = sprintf("%s (%g m)\n%s pixels; %.1f%% with any Neltuma;\n%.2f%% at the %.0f%% purity threshold",
                        labels[[s]], px, format(length(cov), big.mark = ","),
                        100 * mean(cov > 0), 100 * mean(cov >= thr), 100 * thr),
        thr = thr)
    )
  })
  d <- do.call(rbind, lapply(rows, `[[`, "data"))
  n <- do.call(rbind, lapply(rows, `[[`, "note"))
  d$sensor <- factor(d$sensor, levels = names(exts)); n$sensor <- factor(n$sensor, levels = names(exts))

  fig <- ggplot2::ggplot(d, ggplot2::aes(cover)) +
    ggplot2::geom_histogram(binwidth = 0.05, boundary = 0, fill = accent,
                            colour = "white", linewidth = 0.3) +
    ggplot2::geom_vline(data = n, ggplot2::aes(xintercept = thr),
                        linetype = "dashed", colour = "grey40", linewidth = 0.4) +
    ggplot2::geom_text(data = n, ggplot2::aes(x = 0.3, y = Inf, label = label),
                       hjust = 0, vjust = 1.15, size = 2.7, colour = "grey20", lineheight = 0.95) +
    ggplot2::facet_wrap(~ sensor, ncol = 1, scales = "free_y") +
    ggplot2::scale_x_continuous("Sub-pixel Neltuma cover (fraction of pixel)",
                                limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
    ggplot2::scale_y_continuous("Pixels with any Neltuma", labels = scales::comma) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_blank(),
                   panel.grid.major.y = ggplot2::element_line(colour = "grey90", linewidth = 0.3),
                   strip.text = ggplot2::element_blank(),
                   axis.title = ggplot2::element_text(colour = "grey30"))

  dir.create(dirname(out_png), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(out_png, fig, width = 6, height = 7.5, dpi = 200,
                  bg = "white", device = grDevices::png, type = "cairo")
  out_png
}


#' Figure 6C / 7 analogue: a satellite classification of the study area
#'
#' One panel per surface (raw and, where a filter was applied, smoothed), the
#' shared class legend, the Neltuma area in the subtitle. Aggregated to ~1400
#' px wide by modal vote for plotting only; areas come from the full surface.
#'
#' @param surfaces named list: panel label -> class raster path
#' @param sensor_label e.g. "WorldView-2 (1.6 m)"
#' @param out_png output path
#' @return `out_png`
fig_satellite_map <- function(surfaces, sensor_label, out_png) {
  palette <- class_palette()
  panels <- lapply(names(surfaces), function(nm) {
    r <- terra::rast(surfaces[[nm]][1])
    px_ha <- prod(terra::res(r)) / 1e4
    f <- terra::freq(r); nel <- f$count[f$value == 1]
    nel_ha <- if (length(nel)) nel * px_ha else 0
    map_panel(surfaces[[nm]][1], sensor_label,
              sprintf("%s - Neltuma %s ha", nm, format(round(nel_ha), big.mark = ",")),
              palette, target_px = 1400, scale_m = 5000)
  })
  codes <- sort(unique(unlist(lapply(surfaces, function(p)
    terra::freq(terra::rast(p[1]))$value))))
  legend_p <- ggplot2::ggplot(data.frame(code = factor(codes)),
                              ggplot2::aes(x = 1, y = code, fill = code)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_manual(values = palette, labels = class_plot_labels(codes),
                               name = NULL) +
    ggplot2::theme_void(base_size = 9) +
    ggplot2::theme(legend.position = "right", legend.key.size = ggplot2::unit(9, "pt"),
                   legend.text = ggplot2::element_text(size = 8))
  legend <- cowplot_get_legend(legend_p)
  fig <- patchwork::wrap_plots(c(panels, list(legend)), nrow = 1,
                               widths = c(rep(1, length(panels)), 0.35))
  dir.create(dirname(out_png), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(out_png, fig, width = 3.6 * length(panels) + 1.6, height = 7,
                  dpi = 200, bg = "white", device = grDevices::png, type = "cairo")
  out_png
}


#' Figure 7 analogue: one drone site seen by four sensors, plus accuracies
#'
#' Panels A-D: the same site classified from drone, WV2, Planet and S2 (raw
#' surfaces - the pipeline's stance, stated in the subtitle). Panel E: overall
#' accuracy and Neltuma recall per sensor from the archived-arm benchmarks
#' (drone = the site's own winner). Rough by design (decision 2026-09-16
#' [HUGH]): the reproduction draft, not the refactored paper.
#'
#' @param site the drone site to show
#' @param aoi_path its boundary
#' @param surfaces named list sensor -> class raster path (drone first)
#' @param scores named list sensor -> c(overall, neltuma_recall)
#' @param out_png output path
#' @return `out_png`
fig_sensor_comparison <- function(site, aoi_path, surfaces, scores,
                                  out_png = "data-out/figures/fig7_sensor_comparison.png") {
  palette <- class_palette(); accent <- "#B5179E"
  aoi <- terra::vect(aoi_path)
  tmp <- file.path(dirname(out_png), "tmp_fig7"); dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  labels <- c(drone = "Drone (7 cm)", wv2 = "WorldView-2 (1.6 m)",
              planet = "PlanetScope (3 m)", s2 = "Sentinel-2 (10 m)")
  panels <- lapply(names(surfaces), function(s) {
    r <- terra::rast(surfaces[[s]][1])[[1]]
    r <- terra::mask(terra::crop(r, aoi), aoi)
    p <- file.path(tmp, paste0(site, "_", s, ".tif")); terra::writeRaster(r, p, overwrite = TRUE)
    map_panel(p, labels[[s]], "raw surface", palette, target_px = 900, scale_m = 100)
  })
  sc <- data.frame(sensor = rep(names(scores), each = 2),
                   measure = rep(c("Overall accuracy", "Neltuma recall"), length(scores)),
                   value = unlist(scores, use.names = FALSE))
  sc$sensor <- factor(sc$sensor, levels = names(scores), labels = labels[names(scores)])
  panel_e <- ggplot2::ggplot(sc, ggplot2::aes(x = sensor, y = value, colour = measure, group = measure)) +
    ggplot2::geom_line(linewidth = 0.6) + ggplot2::geom_point(size = 2.6) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.0f%%", 100 * value)), vjust = -1, size = 2.6, show.legend = FALSE) +
    ggplot2::scale_colour_manual(values = c("Overall accuracy" = "#3B6EA8", "Neltuma recall" = accent), name = NULL) +
    ggplot2::scale_y_continuous(NULL, labels = scales::percent, limits = c(0, 1)) +
    ggplot2::labs(x = NULL, title = "E  Accuracy by sensor", subtitle = "10 x 10 spatial CV, archived training arms") +
    ggplot2::theme_minimal(base_size = 9) +
    ggplot2::theme(legend.position = "bottom", panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(face = "bold", size = 9))
  codes <- sort(unique(unlist(lapply(surfaces, function(p) terra::freq(terra::rast(p[1])[[1]])$value))))
  legend_p <- ggplot2::ggplot(data.frame(code = factor(codes)), ggplot2::aes(x = 1, y = code, fill = code)) +
    ggplot2::geom_tile() + ggplot2::scale_fill_manual(values = palette, labels = class_plot_labels(codes), name = NULL) +
    ggplot2::theme_void(base_size = 9) + ggplot2::theme(legend.position = "right", legend.key.size = ggplot2::unit(9, "pt"))
  legend <- cowplot_get_legend(legend_p)
  top <- patchwork::wrap_plots(c(panels, list(legend)), nrow = 1, widths = c(1, 1, 1, 1, 0.45))
  fig <- patchwork::wrap_plots(top, panel_e, ncol = 1, heights = c(1.4, 1))
  ggplot2::ggsave(out_png, fig, width = 12, height = 8.5, dpi = 200, bg = "white",
                  device = grDevices::png, type = "cairo")
  out_png
}


#' Figure 8 analogue: Neltuma prevalence (100 m) and invasion phase (250 m)
#'
#' Both from the RAW WV2 surface (7.35: the phase floor is exactly what the
#' filter erases; the smoothed variants are in the same layers). Prevalence
#' is one hue light-to-dark; phases are the same hue as an ordered 4-step
#' ramp with a neutral floor. Rough draft by design.
#'
#' @param prevalence_path,phase_path the .fgb layers from build_phase_layer()
#' @param out_png output path
#' @return `out_png`
fig_phase_maps <- function(prevalence_path, phase_path,
                           out_png = "data-out/figures/fig8_phase_maps.png") {
  prev <- sf::st_read(prevalence_path, quiet = TRUE)
  phs  <- sf::st_read(phase_path, quiet = TRUE)
  phase_cols <- c("Pre-Incursion" = "#EFE9E4", "Initial Incursion" = "#E9A9D8",
                  "Expansion" = "#B5179E", "Dominance" = "#5A0B4E")
  base <- ggplot2::theme_void(base_size = 9) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 9),
                   legend.key.size = ggplot2::unit(9, "pt"), legend.text = ggplot2::element_text(size = 8))
  pa <- ggplot2::ggplot(prev) +
    ggplot2::geom_sf(ggplot2::aes(fill = pmin(cover_raw, 30)), colour = NA) +
    ggplot2::scale_fill_gradient(low = "#F6E3F1", high = "#5A0B4E", name = "Neltuma cover (%)\n100 m cells, capped at 30",
                                 breaks = c(0, 10, 20, 30), labels = c("0", "10", "20", "30+")) +
    ggplot2::labs(title = "A  Neltuma prevalence (raw WV2 surface)") + base
  pb <- ggplot2::ggplot(phs) +
    ggplot2::geom_sf(ggplot2::aes(fill = phase_raw), colour = NA) +
    ggplot2::scale_fill_manual(values = phase_cols, name = "Invasion phase\n250 m hexagons, Table S8", drop = FALSE) +
    ggplot2::labs(title = "B  Invasion phase (raw WV2 surface)") + base
  fig <- patchwork::wrap_plots(pa, pb, nrow = 1)
  dir.create(dirname(out_png), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(out_png, fig, width = 11, height = 8, dpi = 200, bg = "white",
                  device = grDevices::png, type = "cairo")
  out_png
}


#' Figure 1 analogue: the study area and the seven drone sites
#'
#' Boundary from the CRS-fixed WV2 clip, site AOIs from the drone inputs.
#' Rough draft: no basemap, no settlements (those layers are not in the
#' pipeline); enough to place the sites.
#'
#' @param aoi_path study-area .fgb
#' @param site_aois named list site -> aoi shapefile path
#' @param out_png output path
#' @return `out_png`
fig_study_area <- function(aoi_path, site_aois, out_png = "data-out/figures/fig1_study_area.png") {
  accent <- "#B5179E"
  aoi <- sf::st_read(aoi_path, quiet = TRUE)
  sites <- do.call(rbind, lapply(names(site_aois), function(s) {
    v <- sf::st_read(site_aois[[s]][1], quiet = TRUE); v <- sf::st_union(v)
    sf::st_sf(site = gsub("_", " ", tools::toTitleCase(s)), geometry = sf::st_transform(v, sf::st_crs(aoi)))
  }))
  cen <- sf::st_coordinates(sf::st_centroid(sf::st_geometry(sites)))
  sites$x <- cen[, 1]; sites$y <- cen[, 2]
  fig <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = aoi, fill = "grey96", colour = "grey40", linewidth = 0.4) +
    ggplot2::geom_sf(data = sites, fill = accent, colour = accent, alpha = 0.6, linewidth = 0.8) +
    ggplot2::geom_text(data = sf::st_drop_geometry(sites), ggplot2::aes(x, y, label = site),
                       size = 2.8, hjust = -0.15, colour = "grey15") +
    ggplot2::labs(title = "Study area (WV2 boundary, 445 km²) and the seven drone survey sites",
                  subtitle = "EPSG:32734; sites drawn at true size (0.1-1 km²) so they read as dots at this scale") +
    ggplot2::theme_void(base_size = 9) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 10),
                   plot.subtitle = ggplot2::element_text(size = 8, colour = "grey35"))
  dir.create(dirname(out_png), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(out_png, fig, width = 6, height = 9, dpi = 200, bg = "white",
                  device = grDevices::png, type = "cairo")
  out_png
}


#' Figure 6A/B analogue: WV2 learner benchmark and training-arm comparison
#'
#' A: overall accuracy per learner on the archived arm (mean over 100
#' spatial-CV iterations, min-max range). B: the four WV2 training arms, best
#' learner each - the field-only arm against the drone-purity arms, i.e. the
#' +6.1% test (finding 7.37). Rough draft by design.
#'
#' @param wv2_scores all WV2 arm scores
#' @param out_png output path
#' @return `out_png`
fig_wv2_benchmark <- function(wv2_scores, out_png = "data-out/figures/fig6ab_wv2_benchmark.png") {
  accent <- "#B5179E"
  a <- wv2_scores[wv2_scores$site == "wv2_archived", ]; a <- a[order(a$classif.acc), ]
  a$learner <- factor(a$learner, levels = a$learner)
  pa <- ggplot2::ggplot(a, ggplot2::aes(x = classif.acc, y = learner)) +
    ggplot2::geom_segment(ggplot2::aes(x = acc_min, xend = acc_max, yend = learner), colour = "grey75", linewidth = 0.6) +
    ggplot2::geom_point(colour = accent, size = 2.8) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f%%", 100 * classif.acc)), vjust = -1, size = 2.5, colour = "grey20") +
    ggplot2::scale_x_continuous("Overall accuracy (mean; bar = min-max over 100 iterations)", labels = scales::percent, limits = c(0, 1)) +
    ggplot2::labs(title = "A  WV2 learners, archived training arm", y = NULL) +
    ggplot2::theme_minimal(base_size = 9) + ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                                                            plot.title = ggplot2::element_text(face = "bold", size = 9))
  best <- do.call(rbind, lapply(split(wv2_scores, wv2_scores$site), function(d) d[which.max(d$classif.acc), ]))
  best$arm <- factor(sub("^wv2_", "", best$site), levels = c("field", "archived", "dr_raw", "dr_smooth"),
                     labels = c("Field points only\n(0.8 m buffers)", "Drone purity\n(archived extraction)",
                                "Drone purity\n(our raw surfaces)", "Drone purity\n(our filtered surfaces)"))
  pb <- ggplot2::ggplot(best, ggplot2::aes(x = arm, y = classif.acc)) +
    ggplot2::geom_col(fill = accent, width = 0.55) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f%%\n%s", 100 * classif.acc, learner)), vjust = -0.3, size = 2.5, colour = "grey20") +
    ggplot2::scale_y_continuous("Overall accuracy, best learner", labels = scales::percent, limits = c(0, 1)) +
    ggplot2::labs(title = "B  Training arm comparison (the +6.1% test)", x = NULL) +
    ggplot2::theme_minimal(base_size = 9) + ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                                                            panel.grid.major.x = ggplot2::element_blank(),
                                                            plot.title = ggplot2::element_text(face = "bold", size = 9))
  fig <- patchwork::wrap_plots(pa, pb, nrow = 1, widths = c(1, 1.1))
  dir.create(dirname(out_png), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(out_png, fig, width = 11, height = 4.6, dpi = 200, bg = "white",
                  device = grDevices::png, type = "cairo")
  out_png
}
