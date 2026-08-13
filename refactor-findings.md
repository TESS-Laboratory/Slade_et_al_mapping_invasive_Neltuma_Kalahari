# Refactor findings

Living record of everything found while rebuilding this analysis as a `targets`
pipeline. Append as work proceeds; do not rewrite history.

**Audience.** Hugh and Andy. Items marked **[ANDY]** need an authorial decision
about what the manuscript should say, not a coding decision.

**Status legend.** `CONFIRMED` verified by reading code or recomputing ·
`LIKELY` strong inference, not yet verified · `OPEN` needs a decision.

**Branch:** `refactor-v2.0`. `main` untouched.

---

## 0. Headline

**The published code archive (Zenodo `10.5281/zenodo.16681147`) cannot reproduce
the paper.** Three independent reasons, all `CONFIRMED`:

1. Thirteen functions are called but defined nowhere in it, so most run scripts
   could not execute at all.
2. Several reported outputs have no generating code in *any* repository:
   invasion phase (Table 1, Table S8, Fig 8B), the Table S9 and S10
   cross-tabulations, Fig S8, Fig 6B, Fig 7E.
3. Substantial analysis lives only in `TESS-Laboratory/slade-prosopis`, which was
   never archived. 94 script basenames there are absent from the published repo,
   including the canonical Figure 4 script, which is **newer** than the copy that
   was archived.

Full recovery detail in [`audit/source-recovery-map.md`](audit/source-recovery-map.md).
Imported reference code in [`legacy_imported/`](legacy_imported/).

---

## 1. Manuscript describes something different from what the code does

| # | Manuscript says | Code does | Status |
|---|---|---|---|
| 1.1 | "200-iteration spatial block cross-validation" (§2.5, §2.6, Figs 4 and 6 captions) | `rsmp("spcv_coords", folds = 20)` for tuning and benchmarking; `rsmp("repeated_spcv_coords", folds = 10, repeats = 10)` = **100** iterations for final accuracy. **200 appears nowhere.** | CONFIRMED |
| 1.2 | "spatial **block** cross-validation" | `spcv_coords` is k-means clustering of coordinates (Brenning 2012), not blocking. `spcv_block` appears in exactly one file across all four repositories, and it is a teaching notebook, not the pipeline. | CONFIRMED |
| 1.3 | "seven spatial blocks" implied by R1's question at L239-242 | No construct in the code corresponds to seven blocks. The seven survey areas are pooled, then partitioned by k-means on coordinates, so a cluster may span survey areas. | CONFIRMED |
| 1.4 | "benchmarking models ... with Bayesian optimisation" (§2.5) | `tnr("random_search")` in every production script. `tnr("mbo")` appears only in the `run.R` demo, for xgboost only. | CONFIRMED |
| 1.5 | "robust uncertainty estimation" (§2.6); abstract implies uncertainty assessment | Only the spread of accuracy across resampling iterations. `predict_type` is never set to `"prob"`. No probability surface, no per-pixel uncertainty. R1 asked directly whether an uncertainty layer was produced; the answer is no. | CONFIRMED |
| 1.6 | "denoising final drone classifications using the **sieve filter** in terra ... converting classified patches >1 m² to polygons, and analysing nearest-neighbour distances ... using sf" (§2.6) | No `sieve()`, no `as.polygons(dissolve = TRUE)`, no `st_nearest*` anywhere in any repository. The only surviving filter is `legacy_imported/preprocessing/Majority_filter.R`, which applies `terra::focal(w = 25, fun = "modal")` to drone classifications and `w = 9` to WV2. **A modal focal smoother is not a sieve filter**: sieve removes connected patches below a size threshold, focal-modal is a moving-window majority. The described method was never implemented. | CONFIRMED |
| 1.7 | "a minimum of 20 observations per class in each drone survey area" (§2.2) | Table S4 contradicts this in the same document: *V. erioloba* = 2 in Bokspits 1, *Boscia* = 14, *Stipagrostis* = 10, and many zeros. | CONFIRMED |
| 1.8 | Hyperparameter tuning budget not stated | `term_evals` ranges 10 to 50 across scripts, and the superseded `ML-pipeline.R` defaults to 5. Needs documenting whatever the final answer is. | CONFIRMED |

**[ANDY] 1.9.** Items 1.1 to 1.4 all point the same way: the methods section
describes a more rigorous procedure than the one that produced the numbers. Each
needs either a text correction or a re-run. See §5 for the recommended route.

---

## 2. Numbers that do not reconcile

| # | Claim | Recomputed | Status |
|---|---|---|---|
| 2.1 | Study area "445 km²" | Table 1 phases sum to 449.98 km² | CONFIRMED |
| 2.2 | Abstract: expanding across "356 km² (79.9%)" | Table 1 Expansion + Initial Incursion = 78.36 + 281.34 = **359.7 km²**. The percentage (17.4 + 62.5 = 79.9) matches; the area does not. | CONFIRMED |
| 2.3 | "overpredicted Neltuma extent by 24.8%" (§3.2, §4.2) | From Table S10: WV2 predicts *Neltuma* on 32,056 cells vs 24,456 in the drone classification. That is **+31.1%** relative to drone, or **23.7%** of the WV2 total. Neither is 24.8%. | CONFIRMED |
| 2.4 | "34.6% of Neltuma plants ... liable to be classified as V. erioloba or other woody" | From Table S10: of 24,456 drone-*Neltuma* cells, 9,085 (**37.2%**) are not *Neltuma* in WV2, of which 5,333 (**21.8%**) are specifically *V. erioloba* or other woody. Neither is 34.6%. | CONFIRMED |
| 2.5 | Table S10 grand total | Printed as 32,056, which is the *Neltuma* row total. Column totals sum to **639,377**. | CONFIRMED |
| 2.6 | Table S4 total training observations = 1,024 | The seven shipped shapefiles hold 136+180+222+136+146+154+82 = **1,056** records. The 32-record difference is unexplained. | CONFIRMED |
| 2.7 | Table S7 training-pixel selection | Code disagrees for every sensor. WV2: table says 280 at 95%, code says 30 at 99% (also 280 and 500 at 85% and 95% in other script versions). Planet: table says 200 at 85%, code says 400 at 85% (and 280/270/47 elsewhere). S2: table says 100 at 65%, code says 60 with thresholds of 0.75, 0.55 and 0.65 in the same script. | CONFIRMED |
| 2.8 | §2.4: threshold yielding ">= 100 pixels per class" | Inconsistent with Table S7's per-sensor 280/200/100. | CONFIRMED |

**2.3 and 2.4 are the two numbers R2 asked to be promoted to the Abstract and
Conclusions.** Neither is computed anywhere in R, and neither reproduces from the
published table. They must be recomputed and defined explicitly (relative to
what denominator) before they are given more prominence. **[ANDY]**

---

## 3. Missing implementations

### 3.1 Functions called but never defined

Resolved on this branch. See commit `e6a1d49`.

| Function | Resolution |
|---|---|
| `predict_terra_tile` (28 call sites) | **Recovered verbatim** from `GlennSlade/MLR3_pipeline@fa61810` |
| `build_cube_5`, `_5_CHM`, `_5_CHM_NDVI`, `_5_CHM_ALLVI` | **Reconstructed** as band subsets per §2.5's four predictor stacks |
| `build_cube_WV2`, `build_cube_Planet`, `build_cube_S2` | **Reconstructed**, generalised from the never-called `build_cube2` |
| `build_ml_df_WV2`, `_WV2e`, `_S2`, `_Planet_e`, `_LS8`; `build_cube_LS8_T` | **Placeholders** that `stop()` with call sites, likely inputs, hypothesis and unrecoverable decisions |

The `build_ml_df_*` stubs cannot be responsibly reconstructed. The satellite arm
trains from the balanced pixel sets built by `extract_*_pixel.R`, which produce a
five-rung ladder per sensor (`Full_DF` → `full_train_<P>` →
`equal_class_size_<N>` → `_clean` → `_simple`). Which rung each run consumed
leaves no trace. Two consequences:

- **`build_ml_df_WV2` is the highest-priority stub.** It is the field-only
  baseline, so the headline "6.1% improvement, 69.7% to 75.8%" and all of
  Figure 6B rest on it.
- **OPEN:** whether the `frac_*` sub-pixel cover columns were used as
  *predictors* or only as filters. If predictors, the satellite models consumed
  drone-derived information directly, which changes how "cross-scale calibration"
  should be described in §2.5. **[ANDY]**

### 3.2 Outputs with no generating code anywhere

| Output | Note |
|---|---|
| Table 1, Table S8, Fig 8B (invasion phase) | Exhaustive search of all four repos for `dominance`, `incursion`, `invasion phase`, `pre-incursion`: **zero hits**. Produced outside R. |
| Fig 8A (100 m cover) | The hex extraction exists; the cover figure does not. |
| Table S9 cross-tab | Inputs exist (n=184 confirmed against the two shipped height shapefiles). The 5×5 table and the 84.7% / 8.7% / 3.8% figures are not computed in R. |
| Table S10 cross-tab | Paired columns exist; the table and its percentages are not computed in R. |
| Fig 6B | Data exist (`benchmark_analysis_MLR.R` tags `points` vs `30_99`); nothing plots the contrast. |
| Fig 7E | `Full_accuracy_result_analysis.R` is the nearest analogue, is broken past line 100, and covers Planet only. |
| Fig S8 (drone PCA) | `prcomp`/`ggbiplot` exist for WV2 and Planet only, both with magic column indices. |
| §2.6 co-occurrence, 63% of *V. erioloba* (n=192) | Hardcoded `geom_text(label = "63%")` and `label = "27%"` annotations in `Analysis/Eco_Analysis/Camel_thorn.R`, plotted from a hand-edited `temp.xlsx`. Note 63 + 27 = 90, not 100. |

---

## 4. Code defects worth recording

| # | Defect | Impact | Status |
|---|---|---|---|
| 4.1 | `build_ml_df()` reads the class lookup with `col_names=` but no `skip = 1`, so the header row is ingested as data and `Type` becomes character | Silently corrupts the join between field points and class labels | CONFIRMED |
| 4.2 | `benchmark_analysis.R` assigns `df_master <- df` instead of `bind_rows` at the start of every survey block | Only the last survey area (Struizendam_4) survives into `bench_master.xlsx`. Fixed in the `Neltuma_Mlr3_Pipeline/scripts/` copy; still broken in `Analysis/MLR_analysis/` | CONFIRMED |
| 4.3 | `Development_test/run_choices*.R` pass `test_scale = "FALSE"` as a **string**; `tune_lrnr` gates on `isTRUE()`, and `isTRUE("TRUE")` is `FALSE` | Scale and PCA branches never activate, even in the variants whose config says `"TRUE"`. That family is not comparable to the `Shortcuts` family, which passes real logicals | CONFIRMED |
| 4.4 | `Development_test/run_choices*.R` use `rsmp("cv")`, with the spatial line commented out directly above | Those runs are non-spatial CV despite sitting alongside spatial ones | CONFIRMED |
| 4.5 | All four drone stack variants wrote to the same `data_out/<site>/<site>_stack.tif` | Running two for one site silently clobbered the first. Fixed on this branch: variants now write `<site>_stack_<tag>.tif` | CONFIRMED |
| 4.6 | `tune_lrnr` crashes when `.test.pca = TRUE` and `.test.scale = FALSE`: `gr` is only initialised inside the scale branch | Latent, since 4.3 means the branch never fires | CONFIRMED |
| 4.7 | `saveRDS(x, "path_", paste0(N), "_suffix.rds")` in all three `extract_*_pixel.R` and two WV2 extract scripts: no `paste0()` around the path, so extra args land in `ascii`/`version` | The mixed-density training sets are never written | CONFIRMED |
| 4.8 | `extract_S2_pixel.R` writes `..._60_train_65_clean.rds` but reads `..._400_train_85_clean.rds`, which it never produces | The S2 "simple" training set cannot be built at all | CONFIRMED |
| 4.9 | `ens_rf` wraps the SVM learner and `ens_svm` wraps ranger in the stacked ensemble | Cosmetic, but `learner_id` output is misleading | CONFIRMED |
| 4.10 | `MSAVI` as coded uses `- (2*RED)` where Qi et al. (1994) has `- RED`. The correct form is what the code calls `MSAVI2`. `MTVI` as coded is Haboudane's MTVI2 | Labelling error. Downstream consumers use MSAVI2 and NDVI, so results are likely unaffected, but the VI rasters must not be regenerated under the wrong names | CONFIRMED |
| 4.11 | `legacy_imported/preprocessing/Majority_filter.R` line 58: `focal(WV2_Prosopis w=9, ...)` missing a comma | Script does not parse. Pre-existing, faithfully preserved on import | CONFIRMED |
| 4.12 | `slade-prosopis:theme_fancy.R` is an Rmd chunk saved with a `.R` extension | Does not parse standalone. The copies embedded in the analysis scripts are fine | CONFIRMED |
| 4.13 | Archived and unavailable packages: `rgeos` (16+ files), `rgdal` (5), `xlsx` (Java), `ggbiplot`/`bbplot` (GitHub-only), `library(read_xl)` (not a package). `windowsFonts()`/`windowsFont()` in 15+ scripts hard-fails on Linux | Nothing runs on current R without cleanup | CONFIRMED |

---

## 5. Recommended resolutions

### 5.1 The SPCV problem has a clean fix

`legacy_imported/spatial_cv/spatial-autocorrelation.Rmd` is the only place in any
repository where `spcv_block` appears. It compares random CV, block CV,
coordinate k-means and disc resampling on Bokspits_1, and calls
`autoplot(block_cv, task = task, fold_id = 1:4, show_blocks = TRUE)`.

That is exactly the figure R1 asked for at L239-242, and it is almost certainly
where the manuscript's "spatial block cross-validation" wording and its
random-vs-spatial claim came from. But it runs on **one site**, on a
**5× aggregated** cube, with an **untuned `classif.randomForest`**, at **6 folds
× 3 repeats**. It does not describe the reported models.

**Recommendation:** promote it to a first-class pipeline target and re-run the
comparison across all seven sites with the reported learners. That single change

- answers R1's blocking question with a real figure,
- substantiates the abstract's random-vs-spatial claim properly,
- and settles whether §2.5 should say "block" or "coordinate-based k-means
  clustering".

Pair it with `legacy_imported/variogram/` to justify a defensible block `range`
if block CV is adopted.

### 5.2 Reviewer comments still without a drafted response

Each needs a pipeline output, not just prose.

| Comment | Needs | Blocked on |
|---|---|---|
| R1 L239-242, show the spatial blocking | Blocking figure across all sites | §5.1 |
| R1 L272, ranges of km² and % invaded | Invasion phase recomputed with uncertainty | Phase 5, §3.2 |
| R1 Table S9, give UA and PA | Cross-tab computed in R | Phase 3 |
| R1 Fig 6C / 8A / 8B, clarify axis values | Figure regeneration | Phases 4 and 5 |
| R2 main, promote 24.8% and 34.6% | Both recomputed and defined | §2.3, §2.4 |

---

## 6. Open questions

| # | Question | For |
|---|---|---|
| 6.1 | Do Figure 8 and Table 1 derive from the **Random Forest** product (`RF_WV2_all_train_val_combined_b30_additional_WV2_merged_mosaic.tif`, what the hex extraction reads) while Figure 6C reports the **mlr3 SVM/ensemble** product? If so the paper mixes two classifications without saying so, and R2's objection lands on a number that does not describe the map it defends. | **[ANDY]** |
| 6.2 | Were `frac_*` sub-pixel cover columns predictors or filters in the satellite models? Changes what "cross-scale calibration" means. | **[ANDY]** |
| 6.3 | Is the Landsat arm in scope? It is not reported in the manuscript, appearing only as an optional fourth panel of the Fig 5 script and as a caution in the discussion. | **[ANDY]** |
| 6.4 | Which of the five training-set rungs did each satellite run consume? Determines whether Table S7 or the code is right. | Hugh, from archived outputs |
| 6.5 | What accounts for 1,056 shapefile records vs 1,024 in Table S4? | Hugh |
| 6.6 | Should §2.6 be rewritten to describe the modal focal filter that was actually run, or should the sieve-and-nearest-neighbour analysis be implemented as described? | **[ANDY]** |

---

## Changelog

- **2026-08-13** Phase 0.1 to 0.3. Cross-repo audit; recovered and reconstructed
  the missing pipeline functions; imported 23 orphaned scripts into
  `legacy_imported/`. Findings 1.1 to 1.8, 2.1 to 2.8, 3.1 to 3.2, 4.1 to 4.13
  recorded.
