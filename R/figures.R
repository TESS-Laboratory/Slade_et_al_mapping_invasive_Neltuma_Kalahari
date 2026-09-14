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
