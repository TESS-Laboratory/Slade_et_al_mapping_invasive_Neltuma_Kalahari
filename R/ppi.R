#' Prediction-powered inference for Neltuma area (Phase C, decision D6)
#'
#' Angelopoulos et al. (2023). The satellite map covers the whole 445 km2 scene
#' but is biased (7.35: WV2 over-predicts Neltuma against the drone maps). Inside
#' the seven drone sites every satellite pixel has BOTH a satellite prediction
#' and a drone "label", so the bias is measurable there. PPI corrects the
#' scene-wide estimate by that measured bias and returns a confidence interval
#' that accounts for the correction's own uncertainty - the direct answer to
#' Reviewer 2 (propagate the discrepancy to the area claims) and Reviewer 1
#' (report a range, not an absolute).
#'
#' For the Neltuma-fraction mean with binary indicators f(Yhat)=I(sat=Neltuma),
#' Y=I(drone=Neltuma):
#'   theta_tilde = fraction of the SCENE predicted Neltuma            (unlabelled)
#'   Delta       = mean over the OVERLAP of I(sat=Nel) - I(drone=Nel) (rectifier)
#'   theta_PPI   = theta_tilde - Delta
#'   Var(theta_PPI) = theta_tilde(1-theta_tilde)/N + Var(D)/n
#' The rectifier moments come straight from the S10 confusion matrix, so no
#' per-pixel pass over the overlap is needed.
#'
#' @param class_path the scene's averaged hard-class raster
#' @param confusion the raw-vs-raw WV2/drone confusion (wv2_class, drone_class,
#'   n_pixels), from `wv2_drone_confusion()`
#' @param neltuma_code Neltuma class code
#' @param sensor label for the row
#' @param alpha two-sided CI level (0.05 -> 95%)
#' @return one-row data.frame: sensor, naive_ha, ppi_ha, ppi_lo_ha, ppi_hi_ha,
#'   bias_pp (percentage-point over-prediction), n_overlap, n_scene
ppi_neltuma_area <- function(class_path, confusion, neltuma_code, sensor, alpha = 0.05) {
  r <- terra::rast(class_path[1])[[1]]
  px_ha <- prod(terra::res(r)) / 1e4
  f <- terra::freq(r)
  N <- sum(f$count)
  n_nel <- sum(f$count[f$value == neltuma_code])
  theta_tilde <- n_nel / N
  total_ha <- N * px_ha

  m <- stats::xtabs(n_pixels ~ wv2_class + drone_class, confusion)
  nk <- as.character(neltuma_code)
  n <- sum(m)
  sat_nel   <- if (nk %in% rownames(m)) sum(m[nk, ]) else 0   # sat says Neltuma
  drone_nel <- if (nk %in% colnames(m)) sum(m[, nk]) else 0   # drone says Neltuma
  a <- sat_nel - (if (nk %in% rownames(m) && nk %in% colnames(m)) m[nk, nk] else 0)  # sat Nel, drone not (+1)
  b <- drone_nel - (if (nk %in% rownames(m) && nk %in% colnames(m)) m[nk, nk] else 0) # sat not, drone Nel (-1)
  delta <- (a - b) / n
  var_d <- (a + b) / n - delta^2

  theta_ppi <- min(max(theta_tilde - delta, 0), 1)
  se <- sqrt(theta_tilde * (1 - theta_tilde) / N + var_d / n)
  z <- stats::qnorm(1 - alpha / 2)

  data.frame(
    sensor = sensor,
    naive_ha = theta_tilde * total_ha,
    ppi_ha = theta_ppi * total_ha,
    ppi_lo_ha = max(theta_ppi - z * se, 0) * total_ha,
    ppi_hi_ha = min(theta_ppi + z * se, 1) * total_ha,
    bias_pp = 100 * delta,
    n_overlap = n, n_scene = N,
    stringsAsFactors = FALSE
  )
}
