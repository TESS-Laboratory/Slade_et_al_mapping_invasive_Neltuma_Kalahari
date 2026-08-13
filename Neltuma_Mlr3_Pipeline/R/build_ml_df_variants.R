# ---------------------------------------------------------------------------
# PLACEHOLDER FILE - not part of the original published archive.
#
# The five sensor-specific training-table builders below are called by
# scripts/Shortcuts_to_data_specific_analysis/ but exist in no repository
# (searched: this repo, TESS-Laboratory/slade-prosopis,
# GlennSlade/MLR3_pipeline, TESS-Laboratory/Glenn-Prosopis-ML @ the refs
# recorded in audit/source-recovery-map.md).
#
# Unlike the cube variants, these cannot be responsibly reconstructed: the
# satellite arm does NOT train from field points, it trains from the balanced
# pixel sets built by scripts/extract_{WV2,Planet,S2}_pixel.R, and several
# choices (which of the five saved variants each run consumed, how frac_*
# columns were handled, whether geometry was reduced to centroids) leave no
# trace in surviving code. Guessing would silently produce a different training
# set and invalidate every accuracy figure downstream.
#
# So each stub stops with the evidence needed to implement it properly during
# the rewrite (plan phase 4). Failing loudly beats failing quietly.
# ---------------------------------------------------------------------------


#' Stop with a structured "not implemented" report
#'
#' @param fn name of the missing function
#' @param consumed_by character vector of scripts that call it
#' @param inputs likely input files
#' @param hypothesis what we believe the implementation did
#' @param unknowns the specific decisions that are unrecoverable
.missing_impl <- function(fn, consumed_by, inputs, hypothesis, unknowns) {
  msg <- paste0(
    "\n", fn, "() is NOT IMPLEMENTED.\n",
    "\nThis function was called by the original workflow but its source is",
    "\nmissing from every known repository. See audit/source-recovery-map.md.\n",
    "\n  Called by:\n", paste0("    - ", consumed_by, collapse = "\n"),
    "\n\n  Expected inputs:\n", paste0("    - ", inputs, collapse = "\n"),
    "\n\n  Working hypothesis:\n", paste(paste0("    ", hypothesis), collapse = "\n"),
    "\n\n  Unrecoverable decisions - must be resolved before implementing:\n",
    paste(paste0("    - ", unknowns), collapse = "\n"),
    "\n\n  Do not implement by guessing: the satellite accuracy figures in the",
    "\n  manuscript depend on exactly which balanced training set was used.\n"
  )
  stop(msg, call. = FALSE)
}


# Shared context for all the satellite variants -------------------------------
#
# scripts/extract_WV2_pixel.R (and its Planet/S2 siblings) produce a ladder of
# five sf objects per sensor, each with a `Type` class code, `frac_<class>`
# columns giving sub-pixel cover, and the satellite pixel polygon as geometry:
#
#   <SENSOR>_pixel_extract_Full_DF.rds          all pixels, unfiltered
#   <SENSOR>_pixel_extract_full_train_<P>.rds   purity-filtered
#   <SENSOR>_equal_class_size_<N>_train_<P>.rds balanced, frac_4/8/10 dropped
#   ..._clean.rds                               all frac_* columns dropped
#   ..._simple.rds                              classes 5 and 7 collapsed into
#                                               6, giving 4 classes, N per class
#
# `build_task()` requires: a data.frame with `Type` (factor target), numeric
# predictor columns, `x` and `y` coordinate columns, and attr(df, "CRS") set.
# Whatever these builders did, they had to end there.

.SAT_TRAINING_LADDER <- c(
  "<SENSOR>_pixel_extract_Full_DF.rds",
  "<SENSOR>_pixel_extract_full_train_<P>.rds",
  "<SENSOR>_equal_class_size_<N>_train_<P>.rds",
  "<SENSOR>_equal_class_size_<N>_train_<P>_clean.rds",
  "<SENSOR>_equal_class_size_<N>_train_<P>_simple.rds"
)

.SAT_COMMON_UNKNOWNS <- c(
  "Which rung of the training ladder each run consumed. runWV2.R writes an output tagged '30_99_simple', which points at the _simple variant, but this is inference from a filename.",
  "Whether the frac_* sub-pixel cover columns were used as PREDICTORS or only as filters. If used as predictors the satellite models saw drone-derived information directly, which would materially change how 'cross-scale calibration' should be described in section 2.5.",
  "How pixel polygons were reduced to the x/y that build_task() needs - centroid, or the exact_extract coordinate.",
  "Whether cube values were joined by exact_extract('mean') over the polygon or by point sampling at the centre."
)


#' WorldView-2 training table (drone-calibrated variant)
build_ml_df_WV2e <- function(cube, site_name, data_dir = "data_in",
                             out_data_dir = "data_out", df_type = "grid", ...) {
  .missing_impl(
    fn = "build_ml_df_WV2e",
    consumed_by = "scripts/Shortcuts_to_data_specific_analysis/runWV2.R:15",
    inputs = c(
      "data_in/WV2e/WV2_equal_class_size_30_train_99_simple.rds  (most likely)",
      "data_in/WV2e/WV2_equal_class_size_30_train_99.rds",
      "data_in/WV2e/WV2_equal_class_size_30_train_99_clean.rds",
      "cube: data_out/WV2e/WV2e_stack.tif"
    ),
    hypothesis = c(
      "Reads a balanced training set produced by scripts/extract_WV2_pixel.R,",
      "extracts cube values over the WV2 pixel polygons, attaches x/y and the",
      "CRS attribute, and saves data_out/WV2e/WV2eML_in_grid_level.rds."
    ),
    unknowns = c(.SAT_COMMON_UNKNOWNS,
      "N=30 per class at 99% purity is what extract_WV2_pixel.R writes, but Table S7 of the manuscript states 280 pixels per class at 95% purity for WorldView-2. These cannot both describe the reported models.")
  )
}


#' WorldView-2 training table (field-points-only variant)
build_ml_df_WV2 <- function(cube, site_name, data_dir = "data_in",
                            out_data_dir = "data_out", df_type = "grid", ...) {
  .missing_impl(
    fn = "build_ml_df_WV2",
    consumed_by = "scripts/Shortcuts_to_data_specific_analysis/runWV2_E.R:15",
    inputs = c(
      "Field observations buffered to 0.8 m (manuscript section 2.5)",
      "Possibly Final_Drone_Survey_Data/Boravast_all_train_val_combined_b30_WV2.shp",
      "cube: data_out/WV2/WV2_stack.tif"
    ),
    hypothesis = c(
      "This is the FIELD-ONLY baseline - the 69.7% arm of the 6.1% improvement",
      "reported in section 3.2 and Figure 6B. benchmark_analysis_MLR.R tags its",
      "results input_data = 'points', versus '30_99' for the drone-calibrated run."
    ),
    unknowns = c(
      "Whether training points were buffered to 0.8 m here or upstream in GIS.",
      "Which combined point layer was used; Boravast_all_train_val_combined_b30_WV2 is referenced only in WV2_training_points_extract.R.",
      "This is the single most important stub to resolve: Figure 6B and the headline 6.1% improvement both depend on it."
    )
  )
}


#' PlanetScope training table
build_ml_df_Planet_e <- function(cube, site_name, data_dir = "data_in",
                                 out_data_dir = "data_out", df_type = "grid", ...) {
  .missing_impl(
    fn = "build_ml_df_Planet_e",
    consumed_by = "scripts/Shortcuts_to_data_specific_analysis/runPlanet.R:15",
    inputs = c(
      "data_in/Planet_2022/Planet_equal_class_size_400_train_85*.rds",
      "data_in/Planet/Planet_equal_class_size_400_train_85_simple.rds",
      "cube: data_out/Boravast_2022_09/Boravast_2022_09_stack.tif"
    ),
    hypothesis = c(
      "As build_ml_df_WV2e but for PlanetScope, N=400 per class at 85% purity.",
      "Output tag on runPlanet.R's prediction is '400_85', consistent with that."
    ),
    unknowns = c(.SAT_COMMON_UNKNOWNS,
      "extract_Planet_pixel.R writes to data_in/Planet_2022/ then reads from data_in/Planet/ - a directory that nothing writes. One of the two paths is wrong.",
      "Table S7 states 200 pixels per class at 85% purity for PlanetScope; the code says 400.")
  )
}


#' Sentinel-2 training table
build_ml_df_S2 <- function(cube, site_name, data_dir = "data_in",
                           out_data_dir = "data_out", df_type = "grid", ...) {
  .missing_impl(
    fn = "build_ml_df_S2",
    consumed_by = "scripts/Shortcuts_to_data_specific_analysis/runS2.R:15",
    inputs = c(
      "data_in/S2/S2_equal_class_size_60_train_65*.rds",
      "cube: data_out/S2/S2_stack.tif"
    ),
    hypothesis = c(
      "As build_ml_df_WV2e but for Sentinel-2, N=60 per class at 65% purity.",
      "Output tag on runS2.R's prediction is '60_65', consistent with that."
    ),
    unknowns = c(.SAT_COMMON_UNKNOWNS,
      "extract_S2_pixel.R writes ..._60_train_65_clean.rds but then READS ..._400_train_85_clean.rds, which it never produces. The S2 'simple' training set cannot currently be built at all.",
      "Purity thresholds are inconsistent within extract_S2_pixel.R: 0.75 generally, 0.55 for class 6, and 0.65 in the output filename. Table S7 states 65%.")
  )
}


#' Landsat-8 / WorldView-2 time-series training table
build_ml_df_LS8 <- function(cube, site_name, data_dir = "data_in",
                            out_data_dir = "data_out", df_type = "grid", ...) {
  .missing_impl(
    fn = "build_ml_df_LS8",
    consumed_by = "scripts/Shortcuts_to_data_specific_analysis/runLSTR.R:16",
    inputs = c("Unknown - no extract_LS8_pixel.R exists in any repository.",
               "cube: data_out/LS8/LS8_stack.tif via build_cube_LS8_T()"),
    hypothesis = c(
      "The Landsat arm is NOT reported in the manuscript. Landsat appears only",
      "as the optional fourth panel of the Figure 5 script and in the discussion",
      "as a caution about historical archives. This stub is likely out of scope",
      "for the revision - confirm before spending effort on it."
    ),
    unknowns = c(
      "Whether the Landsat arm should be reproduced at all.",
      "build_cube_LS8_T() is equally missing and equally undocumented.",
      "slade-prosopis:Landsat/ holds the related extraction scripts if this is revived."
    )
  )
}


#' Landsat-8 time-series cube
build_cube_LS8_T <- function(site_name, data_dir = "data_in",
                             out_data_dir = "data_out") {
  .missing_impl(
    fn = "build_cube_LS8_T",
    consumed_by = "scripts/Shortcuts_to_data_specific_analysis/runLSTR.R:11",
    inputs = c("Unknown. Related material in slade-prosopis:Landsat/ and",
               "Trend_Image_Data/*/Trend_Img_NDVI_2000_2022b.tif"),
    hypothesis = c(
      "A Landsat-8 NDVI time-series stack, possibly with the WorldView-2",
      "classification as an additional band ('_WV2_Timeseries' appears in the",
      "output filename). Not reported in the manuscript."
    ),
    unknowns = c("Whether the Landsat arm is in scope for the revision at all.")
  )
}
