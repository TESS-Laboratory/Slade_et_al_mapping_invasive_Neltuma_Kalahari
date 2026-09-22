# Review of the refactor-3.0 rewrite (manuscript, statistics, code)

*2026-09-22, branch `refactor-3.0`, reviewed against `refactor-v2.0` and the
submitted manuscript, reviewer letter (`manuscript/Responses to reviewers.pdf`)
and `docs/reviewer-handoff.md`. Everything below was verified against the
`_targets` store or the code; scratch scripts that reproduce the numbers are
noted where they exist. Sections 5 and 6 record what was changed in this pass
and what the single final full rerun must still do.*

---

## 1. Headline

- **The science holds, the honesty of two claims did not.** The sub-pixel cover
  product, the grain → applicability story and the reframed narrative survive
  review. But (a) the "empirical out-of-sample coverage" of the cover intervals
  was *apparent* coverage (quantiles fit and evaluated on the same residuals; it
  equals the nominal rate by construction), and (b) the coverage-driven AOA
  threshold was selected with that same apparent coverage, so it always ran to
  its cap for WV2/Planet. Both are fixed: coverage and the threshold are now
  nested leave-one-site-out. Pooled honest coverage is 88–90% at the 90% level;
  per-site coverage is 52–99%, and the dense site (Struizendam 4) is
  under-predicted by 16–18 pp at every grain.
- **The full store was contaminated on 21 Sep** by a run without
  `NELTUMA_PROFILE` (default `fast`) against `_targets`: `resampling`,
  `eval_shared`, `tune_settings`, the bokspits_1 5_CHM_ALLVI fits and
  `cover_calibrator` are smoke-profile values. The cover numbers were computed
  *before* that (verified: the stored cover cells match a 7-site calibrator, not
  the stored one), so the reported areas are right, but the store is internally
  inconsistent and `tar_outdated` lists 1752/1868 targets. `_targets.R` now
  refuses the fast profile against `_targets`. The final rerun repairs it.
- **The manuscript was still the verbatim submitted text with contradiction
  boxes**, plus a rewritten §3.4. It is now a clean rewrite (`paper/manuscript.qmd`)
  with a separate Supplement (`paper/supplement.qmd`) and a co-author change log
  (`paper/changes-from-submitted.md`). Every number is a pipeline value.
- **Wiring bugs** rendered "NA-iteration" cross-validation, an empty ensemble
  value, "overpredicted by -63.8%", and a 98% "independent validation" that is a
  resubstitution check of the training polygons. All fixed or removed.

## 2. Reviewer requests: coverage after this pass

| Reviewer request | Status | Where |
|---|---|---|
| R1: show the spatial blocking; which areas held out (L239–242) | Done: kNNDM designs with *W* per task; whole-site hold-outs for the satellites | §2.6, Table S12 |
| R1: an uncertainty layer (L253) | Done: conformal set size (discrete), DI-stratified conformal interval width and DI/AOA (cover) | Fig 7 B–D, Fig S13 |
| R1: ranges of km²/% invaded, not absolutes (L272) | Done: area CI (site bootstrap) and phase ranges from the interval bounds | §3.4–3.5, Table 3 |
| R1: inference on model output (PPI, L278) | Done: prediction-powered area correction; settlement/road gradient kept descriptive (as the authors' response) and regenerated from OSM | §2.7, §3.5, Figs S10–S11 |
| R1: UA and PA for Table S9 | Done, with the caveat that the archived layer is the training polygons (resubstitution), so the independent 84.7% stays an authored number | Table S9 + note |
| R1: name the treatments in a table | Done | Table 1 |
| R1: nested design / conceptual figure | Partly: the design is stated in §2.5–2.6 and Table S12; Figure 2 (workflow) remains an authoring item with proposed content | §2.7 note |
| R1: transferability + recommendations | Done | §4.5, Conclusion |
| R1: cost/benefit of drone incorporation | Addressed qualitatively (drone data as cover labels; targeted acquisition via the AOA); no cost model | §4.2 |
| R1: Rhigozum spelling, italics, 3 m vs 4 m | Done (labels from the class scheme; PlanetScope 3 m everywhere) | figures |
| R2: satellite performance overstated; report the discrepancy metrics in the Abstract and Conclusion; management costs of FP/FN | Done: recall/precision vs drone maps and extent ratios in the Abstract, §3.2–3.3 and Conclusion; FP/FN costs in §4.2 and §4.4 | |
| Both: probabilistic / conformal / fractional cover | Done: the primary product | §2.7, §3.4 |

Gaps that remain authorial: Figures 2 and 3 (diagram, photographs), the
independent plant-scale survey layer (not archived), reference-list additions,
and the three **[DECISION]** items in `paper/changes-from-submitted.md`.

## 3. Statistical findings (cover arm)

Numbers from the current store; scratch reproductions in the session scratchpad
(`honest_coverage.R`, `honest_by_di.R`, `more_numbers.R`).

1. **Apparent vs honest coverage (fixed).** `cover_coverage_table` passed the
   calibration residuals as the "new" points, so coverage at 0.95/0.90/0.80 was
   0.950/0.900/0.799 (WV2) by construction. Nested by site (each site's quantiles
   from the other six): 0.919/0.888/0.787 (WV2), 0.935/0.880/0.769 (Planet),
   0.942/0.896/0.807 (S2). Per site at 90%: WV2 0.65 (Struizendam 2) and 0.69
   (Struizendam 4), others 0.94–0.98; Planet 0.68–0.99; S2 0.52 (Struizendam 4)
   to 0.99. Honest coverage is flat (0.84–0.92) across DI bands 1–9 and
   collapses only in the top band, which is 48–65% Struizendam 4 cells: site-level
   shift, not feature-space dissimilarity, is what the intervals miss.
2. **AOA threshold (fixed).** With apparent coverage the walk never stopped
   before the q99 cap (WV2 0.123, Planet 0.164); with honest coverage and the
   old contiguity rule, WV2's lowest band (0.842 < 0.85) discarded the whole
   procedure. New rule: last DI band whose *honest* coverage ≥ 0.85, capped at
   q99, no contiguity. Expected: WV2 and S2 unchanged; Planet's threshold drops to
   ~0.063 (AOA 93% → ~82%; share of scene cover inside 78% → 57%). Planet's area
   will move on the rerun and the "Planet highest" note in the handoff lapses.
3. **Rectifier sign rests on one site (flagged, DECISION 1).** Per-stratum LOSO
   biases in the dense strata come from predicting Struizendam 4 (true cover 21%)
   with models that never saw dense cover (−16 to −18 pp); every other site is
   over-predicted by 0.3–3.7 pp. About a third of the site bootstrap falls below
   the uncorrected within-AOA sum. The manuscript now reports the uncorrected sum
   beside the corrected estimate (1051 vs 1110 ha WV2; 1313 vs 1540 Planet; 981
   vs 1264 S2) and says the correction's sign depends on that site. The bootstrap
   fallback delta is now recomputed per draw (was the full-sample value).
4. **Phases (flagged, DECISION 2).** Pre-incursion was unattainable: the
   regression never predicts zero and the calibrated target never reaches zero
   (non-Neltuma polygons average g(p) = 5.5%). The model's detection floor (mean
   OOF prediction where drone cover < 0.1%) is 1.7 / 2.8 / 2.1% for WV2 / Planet /
   S2, i.e. at or above the 1.5% incursion/expansion boundary. Phases now carry
   the interval range (hexagon means of the lower/upper bounds), a "beyond
   applicability" row and a "below detection floor" share; the text says the
   incursion/expansion split is not resolved by satellite.
5. **Calibrator prior shift (flagged, DECISION 3).** Platt scaling is fit on the
   field sample (20% Neltuma) and applied to pixels (~3%); a Saerens-style prior
   correction could lower every cover target. Not implemented.
6. **Per-pixel skill is modest**: leave-site-out RMSE 10.7 / 11.5 / 7.9 pp against
   a mean cover of 3.7%, R² 0.31 / 0.07 / 0.19. The product is informative in
   aggregate and at the corridors, noisy per pixel; the text says so.
7. **Correct as implemented**: the conformal quantile, OOF DI excluding the own
   site, equal-weight ensemble without tuning leakage, `n_overlap` = labelled
   cells, bootstrap centring/positivity.

## 4. Wiring and figure findings (fixed unless noted)

- `n_outer_iterations` NA (soft-vote row counted as a learner); ensemble value
  empty (D3 removed it); `s10_over_raw_pct` −63.8% (drone rows triple-counted by
  the sensor × site comparison map, and a 5 cm area compared with a 1.6 m class);
  `learner_top2 = "average"`. The values module was rewritten (`R/paper.R`);
  pixel-level agreement now comes from the confusion matrices per sensor.
- Plant-scale "validation" (98.2%, n = 217) is the classifier's training/validation
  polygons; the qmd reports the submitted 84.7% as authored and the Supplement
  labels the pipeline table a resubstitution check.
- Satellite tasks run 50 iterations (5 folds × 10 repeats), not 100; labels fixed.
- Fig 4 caption had panels A/B swapped; Fig 6 (was 7) panel E showed archived-arm CV
  accuracy with a stale "10 × 10" label and mixed arms; redesigned to pixel-level
  recall/precision vs the drone maps + set size. Fig 8 (was 9) B now shows honest
  pooled and per-site coverage; A shows the uncorrected within-AOA sum too.
- Figure S13 was never included; the hard-class conformal phase envelope was
  vacuous (Neltuma in every set → 100% dominance upper / 100% pre-incursion lower)
  and is pruned with the per-unit conformal surfaces and area bounds; `R/ppi.R`
  and its test were orphans and are removed; the slow 100 m prevalence layer is
  retired and `build_phase_layer` materialises its boolean raster first.
- `test-phases.R` called a removed signature (suite was failing); fixed. New tests
  cover nested coverage, the threshold rule, the error table and the phase envelope.
- The fast profile had one site, so the cover arm's leave-one-site-out folds
  could not be built; it now runs two sites and has not been exercised since the
  cover arm was added (18 Sep) — see §6.

### 4b. Incident during this pass: shared output paths

The cover arm wrote its scene rasters, training layers and phase grids to the same
`data-out` paths in both profiles (the hard-class arm suffixes its fast outputs).
Running the two-site fast gate therefore overwrote the full-resolution Sentinel-2
and PlanetScope cover and DI rasters and the WorldView-2 DI raster on disk (the
WorldView-2 cover raster survived). Nothing in the store's science objects was
touched, the lost rasters are regenerated from the stored training tables in under
an hour (and by the final rerun regardless), but the values quoted in §3 were read
before the overwrite. Fixes: every output path now goes through `out_path()` with a
per-profile root (`data-out` / `data-out-fast`, override `NELTUMA_OUT`); tile
directories are keyed to the cube geometry so a resume never mixes tiles from a
different run; the cover scene and DI raster honour the fast profile's aggregation
factor (they used to predict 175M pixels in a smoke test).

## 5. Changes made in this pass

Code: `R/cover.R` (nested coverage, honest threshold rule, error table, interval
phases, gradient table, rectifier sensitivity columns), `R/paper.R` (rewritten),
`R/figures.R` (labels, Fig 6, Fig 8, gradient figure), `R/satellite.R`
(materialised boolean raster; dead conformal-phase code removed), `_targets.R`
(profile guard, new targets, pruning, `supplement` target), `inst/config/resampling.yml`
(two fast sites), tests. Manuscript: full rewrite; Supplement; change log; README.

## 6. What the final full rerun must do (and what it will change)

`NELTUMA_PROFILE=full R -e 'targets::tar_make()'` after the fast gate is green:

1. Restore `resampling`/`eval_shared`/`tune_settings` to full values → the
   bokspits_1 5_CHM_ALLVI fits, soft-vote and `cover_calibrator` rebuild (all
   other fits match their recorded hashes and are skipped).
2. Rebuild the three satellite scene predictions once (phantom-stale since the
   scale-tag commit), ~8 h for WV2.
3. Rebuild the cover chain from `cover_cells_*` (new calibrator), including the
   WV2 cover scene (~3 h) and DI raster; the new threshold/coverage/site/error/
   phase/gradient targets; all figures; paper and supplement.

Expect: WV2 and S2 areas ≈ unchanged; Planet's area down (threshold ~0.063);
coverage values become the honest ones; phase percentages restricted to
AOA-supported hexagons; calibrator coefficients change slightly (7-site fit
gives intercept −0.01, slope 1.24 vs the stored 0.19 / 1.17).

## 7. Performance review of the prediction arms

Measured on real tiles (scratchpad `b1_wrap … b5_drone.R`, `bench_forest.R`). Store
timings: WV2 hard-class scene 7.8 h; the seven drone scenes 139k s in total; WV2
cover scene 3.4 h; WV2 DI raster 10.5 min; Planet 1.3 h; S2 0.7 h.

**Where the time went.** ranger's predict was 58–72% of all prediction CPU
(562 µs/row of the 777 µs/row five-learner WV2 wrap; 293 of 505 µs/row on the
drone). Two mechanisms: (i) its C++ predict fills a `num.trees × n × 8 B`
terminal-node buffer per call (13 GB per WV2 block, 8 GB per 2M-cell cover
tile — the source of the 25–30 GB peaks and the OOM saga) and then gathers per
row; (ii) every call re-marshals the R forest into C++ (~10 s per call for the
690k-row cover forest; 352 tiles ≈ 1 h of the 3.4 h run). Refuted as levers:
mlr3 overhead (<1%), data-frame copies, the Int16/DEFLATE write path (codec
independent, 6–9 s per 96M values), the FNN KD-tree (17 µs/query). Also found:
`terra::predict(cores=)` builds a PSOCK cluster (not forks) with five model copies
per worker and no resumability; the drone footprint equals the AOI, so AOI
cropping helps only the satellites (S2 6.4×, Planet 1.4×, WV2 1.13× fewer pixels).

**What was implemented (all value-neutral; tests in `test-forest.R`):**

1. `R/forest.R`: the fitted ranger forest is flattened once and predicted by a
   compiled tree-major traversal (Rcpp, OpenMP over trees, per-thread accumulators,
   row-major copy of the tile) — no terminal buffer, no marshalling. Exact to
   1e-15 against ranger on real WV2 rows. A first row-major version was 3× *slower*
   than ranger (cold trees per row); the tree-major version is 1.8× faster at 8
   threads on a 60k-row forest (9.2 vs 16.4 s per 300k rows) and the advantage
   grows with forest size because the marshalling cost vanishes. `NELTUMA_FAST_FOREST=0`
   falls back to ranger.
2. One tiled engine for all three predictions (`raster_predict_parallel(tile_fn)`):
   the hard-class five-learner average now runs on the threaded single-copy engine
   (one model copy, AOI crop first, resumable atomic tiles) instead of
   `terra::predict(cores = 8)`; lightgbm/xgboost thread on their own pools; svm/glmnet
   stay serial (9% / 5% of CPU).
3. Tiling is skipped on resume when the input tiles exist; DI clamped at 32.767
   before the Int16 write (an overflow would have read as nodata, not beyond-AOA);
   dead `predict_site` and the smoothing-era helpers removed.

**Expected effect on the final rerun** (to be measured): WV2 hard-class ~7.8 h →
3–4 h; drone predictions roughly halved; WV2 cover 3.4 h → ≤ 1 h; S2/Planet
hard-class 6× / 1.4× faster from the crop alone. Not done: cutting `num.trees`
(not value-neutral) and the daemon-engine memory notes remain in server-ops.
