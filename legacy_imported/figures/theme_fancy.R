# ===========================================================================
# IMPORTED - not part of the original published archive.
#
#   source repo   slade-prosopis
#   source path   theme_fancy.R
#   ref           main @ 671e56f
#   sha256        7d8bcefeed0c18cc44983e4c6287cbb65e7b77d3a8032ca1e1ef4afd3966935e
#
#   why           Canonical copy of the ggplot theme that is duplicated verbatim into 15+ scripts.
#
# Content below is VERBATIM and does not run as-is: it depends on Windows
# absolute paths under E:/Glenn/Botswana/ and on archived packages (rgeos,
# rgdal), and calls windowsFonts(). It is kept as the reference
# implementation to port into the targets pipeline, not to execute.
# See refactor-findings.md and audit/source-recovery-map.md.
# ===========================================================================

```{r echo = FALSE, warning = FALSE, message=FALSE} 


## Plotting theme
theme_fancy <- function() {
  theme_bw() +
    theme(
      text = element_text(family = "Helvetica"),
      axis.text = element_text(size = 9, color = "black"),
      axis.title = element_text(size = 9, color = "black"),
      axis.line.x = element_line(size = 0.3, color = "black"),
      axis.line.y = element_line(size = 0.3, color = "black"),
      axis.ticks = element_line(size = 0.3, color = "black"),
      axis.text.x = element_text(angle = 90),
      panel.border = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank(),
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
      plot.title = element_text(
        size = 10,
        vjust = 1,
        hjust = 0.5,
        color = "black"
      ),
      legend.text = element_text(size = 9, color = "black"),
      legend.title = element_text(size = 9, color = "black"),
      legend.position = c(0.9, 0.9),
      legend.key.size = unit(0.9, "line"),
      legend.background = element_rect(
        color = "black",
        fill = "transparent",
        size = 2,
        linetype = "blank"
      )
    )
}
windowsFonts("Helvetica" = windowsFont("Helvetica")) # Ensure font is mapped correctly

```
