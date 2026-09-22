# Reviewer handoff — Neltuma multi-scale mapping (refactor-3.0)

*Prepared 2026-09-22. Branch `refactor-3.0`. This document orients a reviewer to
the analysis, the manuscript, what changed from the original study, the decisions
behind those changes, how to reproduce everything, and what remains open.*

---

## 1. What this is

A fully reproducible reworking of Slade et al., *"Detecting sparse biological
invasions through multi-scale remote sensing: a drone–satellite framework for
mapping Neltuma in dryland ecosystems"* (SW Kalahari, Botswana). The manuscript
(`paper/manuscript.qmd`) is a **pipeline product**: every number is an inline
expression reading a single list (`paper_values`), and the figures/tables are
`targets` outputs. Nothing in the results is hand-entered.

The work has two layers:

1. **Reproduction** of the original hard-class classification results (what
   reproduces, what does not, and why).
2. **A methodological upgrade** that reframes the satellite product from a
   discrete land-cover classification to a **continuous sub-pixel Neltuma cover**
   map with **distribution-free (conformal) uncertainty** and an explicit
   **area of applicability (AOA)**.

Design decisions and findings are logged in `refactor-findings.md` and
`docs/refactor-3.0-plan.md`; the running state is in
`.claude/.../memory/neltuma-project-state.md`.

---

## 2. Headline results

**Drone (plant/local scale).** ~87–90% overall accuracy for 6-class vegetation
mapping; learner choice is within noise (glmnet/SVM/ensemble all comparable);
best stack = 5-band multispectral + CHM + vegetation indices. Independent
plant-scale validation = 84.7% Neltuma detection. Neltuma co-occurs under/within
1 m of 63% of *V. erioloba* canopies.

**Satellite hard-class (the comparison "foil").** Under honest spatial CV,
WorldView-2 ≈ 64% overall (vs the originally reported 75.8%, which is recovered
only under non-spatial CV / the friendliest fold geometry). PlanetScope ≈ 0.74,
Sentinel-2 ≈ 0.87 (originally reported values reproduced). See §4 for the claims
that do **not** reproduce.

**Satellite sub-pixel cover (the new primary product).** Bias-corrected Neltuma
cover area, within each sensor's coverage-driven AOA, with 95% CIs:

| sensor | area (ha) | 95% CI | AOA fraction | coverage @0.95/0.90/0.80 |
|---|---|---|---|---|
| WorldView-2 (1.6 m) | 1110 | [323, 2166] | 0.98 | 0.950 / 0.900 / 0.799 |
| PlanetScope (3 m) | 1540 | [509, 3806] | 0.93 | 0.951 / 0.901 / 0.801 |
| Sentinel-2 (10 m) | 1264 | [448, 2494] | 0.83 | 0.954 / 0.908 / 0.813 |

The conformal intervals hold their nominal coverage out of sample for all three
sensors. Invasion is overwhelmingly low-cover: WV2 phases ≈ 87% expansion, 13%
early incursion, ~0% dense dominance.

**The reframed narrative (spine of the paper).** *Sensor grain determines where
sub-pixel cover can be mapped with confidence.* Fine grain resolves individual
canopies, so dense-corridor pixels resemble the dense training and stay within
the AOA; coarse grains push the densest invasion (river-valley / settlement
corridors) beyond applicability. From the same drone surveys each grain yields a
very different training density — **690,649 / 179,488 / 16,860** labelled cells
for WV2/Planet/S2 — so mapping the dense fronts with a coarse sensor would require
training that densely spans the invasion gradient; at fine grain the resolution
does that work. The AOA's beyond-applicability zones therefore double as a
model-driven map of *where to target the next drone survey*.

---

## 3. Methods (analysis pipeline)

All statistical code is in `R/`; the graph is generated from
`inst/config/sensors.yml` via `R/graph.R` into `_targets.R`.

**3.1 Drone arm.** Per-site multispectral + CHM cubes → 6-class classification
(mlr3), evaluated with repeated spatial CV (kNNDM, CAST; leave-one-site-out where
kNNDM's k-means fails on dense cells). Winner-per-site drone maps feed the cover
calibration.

**3.2 Satellite hard-class arm (foil).** WV2/Planet/S2 cubes (spectral + VIs),
trained on field + own-raw-surface purity extractions; kNNDM spatial CV; per-sensor
benchmarks; landscape classification maps. **The post-classification modal filter
is retired** (it halves WV2 Neltuma recall for no precision gain). Retained as the
sensor-comparison foil, to be relegated to SI in the next authorial pass (§7).

**3.3 Sub-pixel cover regression (C2, `R/cover.R`).** (a) Calibrate drone Neltuma
probability (Platt scaling on out-of-fold predictions) so mean predicted
probability is an unbiased estimate of cover; (b) warp-average the calibrated
probability onto each satellite grid (applying the calibrator *before* averaging)
= per-cell fractional-cover target; (c) regress on the satellite predictors with an
equal-weight ensemble of `glmnet` + `ranger` + `lightgbm` (SVM dropped — worst RMSE
and slowest); equal weights are fixed a priori so the conformal wrapper stays honest
(no nested-selection leakage).

**3.4 AOA × conformal (C3, the novel piece).** Dissimilarity index (DI) via an FNN
KD-tree (CAST's `aoa` is too slow at scene scale). **DI-stratified (Mondrian) split
conformal** on leave-one-site-out folds gives per-pixel prediction intervals; the
coverage guarantee is *earned empirically* (kNNDM breaks exchangeability), and the
out-of-sample coverage validation is a first-class result.

**3.5 Area inference (PPI).** Landscape area = Σ(cover × pixel) within the AOA,
bias-corrected by a **cover-stratified, prediction-powered rectifier** measured on
the held-out drone cells (a *local* rectifier, binned by predicted cover, with a
fallback to the global bias for thin strata — this replaced a single global
rectifier that mis-behaved; see §4). The CI is a **site cluster bootstrap** of the
per-stratum biases (each drone survey area is the unit of spatial replication).

**3.6 Coverage-driven AOA threshold (`cover_aoa_threshold`).** The AOA cutoff is
tied to the guarantee: it extends the trusted domain from low DI up to the last DI
band whose empirical out-of-sample coverage still meets a floor (0.85, at the
deployed conformal), capped at the 99th percentile of training DI. This is
per-sensor and self-limiting (S2 stops early because its coverage degrades; WV2
extends to the corridors). See §4 for why this replaced the previous fence.

---

## 4. What changed from the original (and why)

Contradicted original claims are marked in the qmd with `.contradicted` spans and
explained in adjacent `PIPELINE-CONTRADICTS` / pipeline-note boxes; several carry an
`[ANDY]` tag flagging a genuine authorial decision.

- **Learner ranking / 75.8% WV2 accuracy.** 75.8% is reproduced only under
  non-spatial CV; under 10×10 spatial CV WV2 ≈ 64%. The reported figure is the
  friendliest fold geometry. (qmd §3.2, boxed.)
- **The +6.1% cross-calibration boost does not reproduce.** The field-only arm
  scores *above* the drone-calibrated arm under honest CV. (qmd §3.2, boxed.)
- **Modal filter retired.** The original 24.8% over-prediction came from a
  smoothed-vs-smoothed comparison; the filter halves WV2 Neltuma recall for no
  precision gain. Only the raw (unfiltered) surface is used.
- **34.6% woody-confusion figure** is not recoverable from the S10 matrix as
  reported; the pipeline's analogue differs. ([ANDY].)
- **Hard-class PPI abandoned.** A global scalar rectifier measured in high-prevalence
  drone sites and extrapolated to the sparse scene collapses (S2 → ~0, Planet → ~16
  ha); this drove the move to sub-pixel cover regression (D11).
- **Stratified rectifier (2026-09-22).** The first cover-area CI put the WV2 lower
  bound at 0 — a manufactured artefact of a single global rectifier plus a fragile
  7-cluster bootstrap dominated by one dense site (`struizendam_4`, −16 pp). The
  cover-stratified rectifier fixes it; areas ~stable, zeros gone.
- **Coverage-driven AOA (2026-09-22).** The original `Q3 + 1.5·IQR` DI fence was set
  by the sparse dune matrix and excluded the dense-Neltuma corridors — 15/39/26% of
  predicted cover (WV2/Planet/S2) and ~48% of the >25%-cover *training* cells fell
  beyond it, a large impact underestimate, even though out-of-sample coverage there
  was still 85–88%. The coverage-driven threshold corrected the areas upward (WV2
  890→1110, Planet 1187→1540, S2 1239→1264) while keeping nominal coverage.

---

## 5. Figures & tables (current state)

**Main text.** Fig 1 study area; Fig 2 framework; Fig 3 drone classification detail;
**Fig 4** drone accuracy (per-site maps + accuracy panels); **Fig 5** sub-pixel cover
distribution within coarse pixels (the "scale-mismatch" motivation); Fig 6 WV2
classification benchmark + landscape map (hard-class); Fig 7 sensor-grain
classification comparison; **Fig 8** the 12-panel sub-pixel-cover figure
(`fig_cover_grain`: A cover within AOA, B DI relative to AOA threshold, C conformal
pixel interval width, D bivariate detection confidence; WV2/Planet/S2 columns; OSM
roads + villages overlaid); **Fig 9** cover area + interval-calibration plots;
**Table 1** cover-based invasion phases.

**Supplementary.** Fig S12 hard-class WV2 prevalence/phase map (foil); Fig S13
un-greyed cover (cover shown across the whole scene, signposted from Fig 8);
Table S11 hard-class phase areas; plus the existing Tables S1–S10.

**Planned (next authorial pass, §7):** move the satellite *classification* arm
(Figs 6–7, hard-class tables) to SI, keeping Fig 5 and the fuller §3.2/§3.3 prose in
the main text as the reviewer requested.

---

## 6. Reproduce it

```bash
source tools/uvr-env.sh              # REQUIRED: forces source builds (host GDAL .37
                                     # vs P3M binaries .34), wires pandoc/quarto
# fast smoke profile (minutes, its OWN store):
NELTUMA_PROFILE=fast NELTUMA_STORE=_targets_fast \
  R -e 'targets::tar_make(store="_targets_fast")'
# full profile:
NELTUMA_PROFILE=full R -e 'targets::tar_make()'
# render the manuscript (Quarto subprocess reads NELTUMA_STORE):
NELTUMA_STORE=_targets R -e 'targets::tar_make(names="paper")'
```

- Cover arm targets: `cover_calibrator`, per-sensor `cover_cells_* → cover_train_* →
  cover_oof_/cover_di_/cover_threshold_/cover_coverage_/cover_scene_/cover_di_raster_/
  cover_area_/cover_phase_*`, indexed by `cover_area_index` / `cover_coverage_index` /
  `cover_phase_index`.
- The 12-panel figure (`fig_cover_grain`) and its SI companion (`fig_cover_full`) read
  the scene cover/DI rasters + OSM overlays (`osm_roads`, `osm_settlements`, fetched
  once via `tools/fetch-osm.R`; the `.fgb` live under `data-in/` which is not tracked).
- Tests: `Rscript tests/testthat.R` (pure-function unit tests, incl. `test-cover.R`).

**Compute notes.** WV2 scene prediction is memory-heavy: use the single-copy
*threaded* predict engine (`raster_predict_parallel(engine="threaded")`, ~25 GB, ~3 h)
— the mirai daemon engine copies the model per worker and OOMs. Details in
`.claude/.../memory/neltuma-server-ops.md`.

---

## 7. Open questions / parked items

- **Cross-sensor ensemble.** The three cover maps agree on totals (means 2.3–2.7%)
  but only moderately at pixel level (Pearson r 0.63–0.74), with disagreement
  concentrated at high-cover corridor edges. A consensus fusion is attractive but
  sacrifices WV2 resolution; parked pending a decision.
- **PlanetScope composite artefact.** A scene-composite radiometric seam imprints a
  block of inflated cover into the Planet prediction (visible in Fig 8C–D). It should
  be masked/flagged; it partly inflates Planet's recovered area. Notably S2's
  intervals beyond the AOA are no wider than Planet's — grain and radiometric
  consistency interact (a discussion point already in the text).
- **Satellite classification → SI.** Agreed in principle (cover is the primary
  satellite product); deferred to the reviewer's in-depth pass. Concrete plan ready.
- **C3 methods write-up.** The AOA × conformal method is kept internal for now; a
  separate methods paper is planned ([ANDY], after this paper).
- **Abstract cover framing / cross-references** — a polish pass is outstanding.

---

## 8. Caveats a reviewer should know

- Several original numbers are *contradicted* by honest spatial CV; these are flagged
  in-text and await authorial decisions (search the qmd for `.contradicted` and
  `[ANDY]`).
- The conformal coverage guarantee is *empirical*, not assumption-based: kNNDM breaks
  exchangeability, so coverage is earned via out-of-sample validation (reported).
- The AOA is a *trust region*, not a hard error map; within-AOA areas are the
  conservative, guaranteed numbers, and beyond-AOA cover is shown (Fig S13) but not
  counted in the headline areas.
- Figure functions for the cover arm are named `make_fig_*` (not `fig_*`) to avoid a
  `targets` metadata collision with their same-named file targets; the other `fig_*`
  functions retain the older convention and can collide under repeated interrupted
  builds (rebuild cleanly if a `store_class_format` error appears).
