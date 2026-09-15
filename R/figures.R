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
map_panel <- function(class_tif, site, subtitle, palette, target_px = 1400) {
  r <- terra::rast(class_tif)
  fact <- max(1L, floor(terra::ncol(r) / target_px))
  if (fact > 1L) r <- terra::aggregate(r, fact = fact, fun = "modal", na.rm = TRUE)

  df <- terra::as.data.frame(r, xy = TRUE, na.rm = TRUE)
  names(df)[3] <- "code"
  df$code <- factor(as.integer(df$code))

  # 100 m scale bar, bottom-left inside the panel
  e <- terra::ext(r)
  bar_x <- e$xmin + 0.05 * (e$xmax - e$xmin)
  bar_y <- e$ymin + 0.05 * (e$ymax - e$ymin)

  ggplot2::ggplot(df, ggplot2::aes(x = x, y = y, fill = code)) +
    ggplot2::geom_raster() +
    ggplot2::scale_fill_manual(values = palette, drop = TRUE) +
    ggplot2::annotate("segment", x = bar_x, xend = bar_x + 100,
                      y = bar_y, yend = bar_y, linewidth = 1.2, colour = "grey15") +
    ggplot2::annotate("text", x = bar_x + 50, y = bar_y,
                      label = "100 m", vjust = -0.8, size = 2.6, colour = "grey15") +
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
    ggplot2::geom_text(data = n, ggplot2::aes(x = 0.02, y = Inf, label = label),
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
