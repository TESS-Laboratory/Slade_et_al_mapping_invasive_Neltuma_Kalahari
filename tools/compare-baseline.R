# Phase A gate: does the refactor-3.0 full run reproduce the v2.0 numbers?
#
#   Rscript tools/compare-baseline.R [store]        (default store: _targets)
#
# Reads the v2.0 tables snapshotted in data-out/results/v2_baseline/ and the
# same quantities from the new store, joins on the v2-compatible keys and
# reports the differences. Seeds and data are unchanged, so the drone arm and
# the archived satellite arms should match to floating point; the re-derived
# arms match if the drone surfaces are byte-identical.
suppressMessages({ for (f in list.files("R", full.names = TRUE)) source(f) })
store <- if (length(commandArgs(TRUE))) commandArgs(TRUE)[1] else "_targets"
base <- function(n) readRDS(file.path("data-out/results/v2_baseline", paste0(n, ".rds")))
new  <- function(n) targets::tar_read_raw(n, store = store)

cmp_scores <- function(name) {
  b <- base(name); n <- new(name)
  m <- merge(b[, c("site", "tag", "learner", "classif.acc")],
             n[, c("site", "tag", "learner", "classif.acc")],
             by = c("site", "tag", "learner"), suffixes = c("_v2", "_v3"))
  m$diff <- m$classif.acc_v3 - m$classif.acc_v2
  cat(sprintf("%-12s %3d rows matched (v2 %d, v3 %d); max |diff| = %.4f; mean diff = %+.4f\n",
              name, nrow(m), nrow(b), nrow(n), max(abs(m$diff)), mean(m$diff)))
  big <- m[abs(m$diff) > 0.005, ]
  if (nrow(big)) print(big[order(-abs(big$diff)), ][1:min(8, nrow(big)), ], digits = 3, row.names = FALSE)
  invisible(m)
}
cat("== learner scores (mean accuracy over 100 spatial-CV iterations) ==\n")
for (nm in c("score_index", "wv2_scores", "sat_scores")) cmp_scores(nm)

cat("\n== winners ==\n")
b <- base("best_models"); n <- new("best_models")
m <- merge(b[, c("site", "tag", "learner")], n[, c("site", "tag", "learner")], by = c("site", "tag"), suffixes = c("_v2", "_v3"))
cat(sprintf("drone winners unchanged: %d of %d\n", sum(m$learner_v2 == m$learner_v3), nrow(m)))

cat("\n== Neltuma areas (ha), raw surfaces ==\n")
b <- base("class_areas"); n <- new("class_areas")
b1 <- b[b$Type == 1, c("site", "area_ha")]; n1 <- n[n$Type == 1, c("site", "area_ha")]
n1$site <- sub("^drone_", "", n1$site)
m <- merge(b1, n1, by = "site", suffixes = c("_v2", "_v3")); m$diff_pct <- 100 * (m$area_ha_v3 / m$area_ha_v2 - 1)
print(m, digits = 4, row.names = FALSE)
for (nm in c("wv2_pred_summary", "sat_pred_index")) {
  b <- base(nm); n <- new(nm); b <- b[b$Type == 1, ]; n <- n[n$Type == 1, ]
  cat(sprintf("%s Neltuma ha: v2 %s | v3 %s\n", nm, paste(round(b$area_ha), collapse = ", "), paste(round(n$area_ha), collapse = ", ")))
}

cat("\n== phases (WV2, 250 m hex) ==\n")
b <- base("wv2_phase_table"); n <- new("wv2_phase_table")
m <- merge(b[, c("surface", "phase", "pct_of_area")], n[, c("surface", "phase", "pct_of_area")], by = c("surface", "phase"), suffixes = c("_v2", "_v3"))
print(m, digits = 3, row.names = FALSE)

cat("\n== plant-scale (S9) Neltuma ==\n")
b <- base("plant_validation_summary"); n <- new("plant_validation_summary")
print(merge(b[b$Type == 1, c("surface", "n", "accuracy")], n[n$Type == 1, c("surface", "n", "accuracy")], by = "surface", suffixes = c("_v2", "_v3")), digits = 4, row.names = FALSE)

cat("\n== compute ==\n")
mb <- base("tar_meta"); mn <- targets::tar_meta(fields = c("name", "seconds"), store = store)
cat(sprintf("CPU-hours in store: v2 %.1f | v3 %.1f\n", sum(mb$seconds, na.rm = TRUE) / 3600, sum(mn$seconds, na.rm = TRUE) / 3600))
