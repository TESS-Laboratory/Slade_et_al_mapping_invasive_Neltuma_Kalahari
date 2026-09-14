# Refactor findings

Living record of everything found while rebuilding this analysis as a `targets`
pipeline. Append as work proceeds; do not rewrite history.

**Audience.** Hugh and Andy. Items marked **[ANDY]** need an authorial decision
about what the manuscript should say, not a coding decision.

**Status legend.** `CONFIRMED` verified by reading code or recomputing ·
`LIKELY` strong inference, not yet verified · `OPEN` needs a decision.

**Branch:** `refactor-v2.0`. `main` untouched.

**Continuing on the server?** Start at
[`docs/server-handover.md`](docs/server-handover.md).

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
| 1.7 | "a minimum of 20 observations per class in each drone survey area" (§2.2) | Table S4 contradicts this in the same document, and **the pipeline now recomputes it from the field layers: 8 of 41 site-class combinations fall below 20**. Worst is *V. erioloba* at Struizendam 4 with **n = 2**. Also *Stipagrostis amabilis* n=10 and *S. mellifera* n=16 at Bokspits 2, *Boscia albitrunca* n=14 at Struizendam 3, *V. erioloba* n=14 at Struizendam 1. See 7.21. | CONFIRMED, recomputed |
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
| 2.7 | Table S7 training-pixel selection | Code disagrees for every sensor. WV2: table says 280 at 95%, code says 30 at 99% (also 280 and 500 at 85% and 95% in other script versions). Planet: table says 200 at 85%, code says 400 at 85% (and 280/270/47 elsewhere). S2: table says 100 at 65%, code says 60 with thresholds of 0.75, 0.55 and 0.65 in the same script. | **RESOLVED, see 7.18 / 7.19.** The archived runs settle it: thresholds 95/85/65 in Table S7 are all correct; the class sizes 280/200/100 are all wrong and were 400/400/60. |
| 2.8 | §2.4: threshold yielding ">= 100 pixels per class" | Inconsistent with Table S7's per-sensor 280/200/100. | CONFIRMED |
| 2.9 | §3: WV2 "SVM and ensemble models ... mean accuracies of **75.9%** and **75.1%**" | The headline 75.8% reproduces exactly (`400_95_WV2e` ensemble = 0.7579), but these two per-learner figures appear in **no** surviving workbook. That run gives SVM 0.7556 and ensemble 0.7579 — so the text also has the ranking inverted, crediting SVM with the higher score when the ensemble scored higher. Not a swap either: 0.759 and 0.751 occur nowhere across any WV2 benchmark. | CONFIRMED, new |
| 2.10 | Pixel size of PlanetScope | The manuscript gives **3 m** in the Abstract (L20) and §2 (L198) and in Table S7, but **4 m** in the Figure 7 caption (L369). One of them is wrong; PlanetScope surface reflectance is delivered at 3 m. | CONFIRMED, new |

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

### 3.3 `build_ml_df` recovered from the console history, and it disagrees with the repo copy

The server scan found `Glenn-Prosopis-ML/.Rhistory`, into which Glen had pasted
whole function bodies rather than only calls. Lines 30 to 98 contain
**`build_ml_df` complete with its roxygen block**.

It is not the same function as the one already on this branch in
`Neltuma_Mlr3_Pipeline/R/build_ml_df.R`. Two differences change behaviour:

| | Repo copy | Console-history copy |
|---|---|---|
| `df_type` default | `"grid"` | `"point"` |
| Response variable | `Class = as.factor(Class)` | `Type = factor(Type, levels = unique(Type))`, preceded by `dplyr::arrange(Type)` |

The second is not cosmetic: **the two versions model different response
variables**, and the history copy fixes factor level order by sorted first
appearance where the repo copy takes alphabetical order from `as.factor`. Any
reproduction built on the repo copy trains against a different target than the
console session did.

Which one produced the reported numbers is **RESOLVED, 2026-08-17: the console
version is right, and the response was `Type`.** Settled from the archived
outputs already mirrored, without needing the `.rds` extracts. Every one of the
**29** surviving drone confusion workbooks carries *numeric* dimnames — the union
across all of them is codes `1, 2, 3, 4, 5, 6, 7, 10, 13`, and the set varies by
site with what was actually surveyed there (Bokspits_1 has `1,2,3,4,5,6`,
Struizendam_3 has `1,2,3,6,7,10`). **Zero** workbooks use class-name strings.

Had `Class` been the response, those dimnames would read `Neltuma`,
`Bare Ground` and so on, because `Class` is the name column in the lookup and
`Type` is the integer code. So the drone models were trained on the field `Type`
code, subset per site to the classes present.

Two consequences for the port. The response is an integer code carried as a
factor, so `class_labels()` must be applied at presentation time and never
before. And the per-site class sets differ, which means a benchmark table
combining sites is comparing models over different label spaces — worth stating
explicitly wherever site results are pooled.

The recovery does **not** rescue the five `build_ml_df_*` satellite variants.
The history calls `build_ml_df_WV2e(cube = x, site_name = "WV2e", df_type = "points")`
but never defines it. Note `"points"`, plural, which matches neither branch of
the recovered function's `if/else` — so the variants took a different
`df_type` vocabulary, further evidence they are genuinely separate
implementations rather than thin wrappers. The placeholders stand.

The history also confirms the tuning setup independently of the scripts:
`rsmp("spcv_coords", folds = 20)`, `tnr("random_search")`, and xgboost / SVM /
ranger / a stacked ensemble benchmarked against an untuned ranger baseline. That
corroborates findings 1.1 and 1.4.

---

## 4. Code defects worth recording

| # | Defect | Impact | Status |
|---|---|---|---|
| 4.1 | **CORRECTED, see 8.2.** `build_ml_df()` reads the class lookup with `col_names=` and no `skip = 1`. This was *correct* against the original headerless file. It became a fault only when the lookup gained a header row in commit `567696e` | Silently corrupts the join between field points and class labels, but as a regression from data curation, not a latent bug | CONFIRMED |
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
| 4.14 | `build_ml_df` writes `paste0(site_name, "ML_in_Point_level.rds")` with a capital P, but all six surviving files on the server are lowercase `ML_in_point_level.rds`, and every read site uses lowercase | Silent on Windows, hard failure on Linux. Must be normalised during the port, independently of which `build_ml_df` copy wins (see 3.3) | CONFIRMED |
| 4.15 | The `*_Field_data_points_All_b30` layers are **Polygon** geometry, not points — `_b30` is a 30 cm buffer | Any port that assumes point geometry silently changes the extraction: `build_ml_df` depends on `exact_extract(fun = "mean")` over the buffer, then `st_centroid()`. Mirrored as `field_points.*`, keeping the upstream name; the geometry is recorded in the manifest | CONFIRMED |

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

### 5.3 The CHM is an input. Store the drone bands independently and stack by VRT

**Decision, 2026-08-17 [HUGH].** The CHM is treated as a raw input alongside the
reflectance bands, not as something this pipeline derives. Predictor stacks are
assembled on demand by VRT rather than stored as duplicated multi-band GeoTIFFs.

**An earlier position in this document is withdrawn.** A first reading of the
evidence suggested promoting DSM and DTM to raw inputs and deriving the CHM as an
intermediate. Reading the producer script settles it the other way.
`Image_Processing/makeReflStacks_+CHM_terra.R` performs no differencing at all —
it reads a CHM that already exists:

```r
Bokspits_1_CHM  <- rast("E:/Glenn/Botswana/ReflStacks/Bokspits_1_CHM.tif")
Bokspits_1_CHMF <- resample(Bokspits_1_CHMCrop, Bokspits_1_ReflStackCrop,
                            method = "bilinear")
Bokspits_1_Stack_CHM <- c(Bokspits_1_ReflStackCrop, Bokspits_1_CHMF)
```

Two consequences, both correcting statements made earlier:

- **DSM and DTM are not used by the drone arm.** The only elevation product it
  consumes is `<site>_CHM.tif`. `drone_pix4d_dsm` and `drone_pix4d_dtm` are read
  solely by `Terrain_Analysis.R`. Their recovery is therefore **not** blocking,
  and they stay filed as lost unless terrain analysis returns to scope.
- **Finding 7.5 is partly wrong** and is corrected in place below. The CHM *did*
  have standalone existence, as `ReflStacks/<site>_CHM.tif`. It simply did not
  survive to the server. What stands is that no script in any repository produces
  it — it arrives from outside the codebase, and that remains unexplained.

**What survives constrains the layout.** Nothing upstream of the assembled stacks
is left: no standalone `<site>_CHM.tif`, no per-band
`*_transparent_reflectance_*.tif`, no `4_index/` path anywhere (7.16). The
earliest surviving drone rasters are the two stacks themselves.

The target layout is therefore reachable only by decomposition, and that
decomposition is provably lossless: bands 1 to 5 of `refl_stack_chm.tif` are
**bit-identical** to `refl_stack.tif` (`max|diff| = 0` on every band, same grid,
same extent, verified at Bokspits_1). So

```
chm.tif          band 6, extracted once
refl_stack.tif   already independent, 5 bands
stack_5_CHM.vrt  gdalbuildvrt -separate, no resampling required
```

reproduces `refl_stack_chm.tif` exactly. No warping is involved because the CHM
band was already resampled onto the reflectance grid upstream.

**That upstream resample is baked in and cannot be undone.** The archived CHM
band is post-`crop`, post-`mask` and post-bilinear-`resample`. A native-resolution
CHM recovered later would *not* be a drop-in replacement, and any comparison
against band 6 must apply the same three operations first. Recorded so this is
not rediscovered as a discrepancy.

Secondary benefit: every site currently stores the five reflectance bands twice,
which accounts for most of the 64 GB mirror.

### 5.4 File formats to standardise on

**Decision, 2026-08-17 [HUGH].** Deferred housekeeping, recorded so it is not
lost. No Excel and no shapefiles in the new pipeline:

| Current | Target | Note |
|---|---|---|
| `.xlsx` (lookups, bench and confusion workbooks) | CSV, JSON or Parquet as most appropriate | Lookup already done: `inst/config/classes.json`. `readxl` stays, for reading the surviving legacy workbooks only |
| `.shp` + 7 sidecars | **FlatGeobuf** (`.fgb`) or **GeoParquet** | Also removes the missing-`.prj` problem in `WV2_clip.shp` (7.13) |
| `.rds` / `.RData` intermediates | `targets` store, `qs2` | `.RData` is deliberately not being opened |

`writexl` was omitted from the environment for this reason. `targets` 1.12.0
offers `tarchetypes::tar_format_nanoparquet()` if Parquet is wanted for tabular
targets.

---

## 6. Open questions

| # | Question | For |
|---|---|---|
| 6.1 | Do Figure 8 and Table 1 derive from the **Random Forest** product (`RF_WV2_all_train_val_combined_b30_additional_WV2_merged_mosaic.tif`, what the hex extraction reads) while Figure 6C reports the **mlr3 SVM/ensemble** product? If so the paper mixes two classifications without saying so, and R2's objection lands on a number that does not describe the map it defends. | **[ANDY]** |
| 6.2 | Were `frac_*` sub-pixel cover columns predictors or filters in the satellite models? Changes what "cross-scale calibration" means. | **[ANDY]** |
| 6.3 | Is the Landsat arm in scope? It is not reported in the manuscript, appearing only as an optional fourth panel of the Fig 5 script and as a caution in the discussion. | **[ANDY]** |
| 6.4 | ~~Which of the five training-set rungs did each satellite run consume?~~ | **CLOSED.** WV2 400@95, Planet 400@85, S2 60@65 — see 7.18 and 7.19. Table S7's thresholds are right, its class sizes are wrong. Manuscript correction **[ANDY]** |
| 6.5 | What accounts for 1,056 shapefile records vs 1,024 in Table S4? | Hugh |
| 6.6 | Should §2.6 be rewritten to describe the modal focal filter that was actually run, or should the sieve-and-nearest-neighbour analysis be implemented as described? | **[ANDY]** |

---

## 7. Data collation

66 required inputs are catalogued in
[`inst/manifest/data_manifest.csv`](inst/manifest/data_manifest.csv), with a
dependency-free discovery script at
[`tools/discover-inputs.sh`](tools/discover-inputs.sh) and a server handover
guide at [`docs/data-collation.md`](docs/data-collation.md).

Of the 66 entries, only 4 are in this repository. The rest sit on a Windows
external drive we do not have, across five assumed project roots. Roughly a
quarter are re-downloadable from Zenodo `10.5281/zenodo.18506271`; the satellite
scenes, every vector layer, the co-registered mosaics, the hexagonal grids and
all intermediate result workbooks are not archived anywhere.

**7.2 CONFIRMED.** Only **25 of the 66 entries are true inputs**. The other 41
are derived, meaning the pipeline produces them or could. That materially reduces
what has to be recovered: a missing derived file is usually acceptable because we
rebuild it, and rebuilding is preferable anyway since it brings the product under
the pipeline's control.

**7.3 CONFIRMED. Six entries are derived in principle but have no producer
anywhere in the codebase, so they behave as inputs:** `drone_chm`,
`wv2_corrected_16m`, `wv2_train_points_combined`, `planet_grid_95`, `cover_all`,
`cover_resolution_simple`. Only `drone_chm` has a plausible rebuild path (DSM
minus DTM), which is why both Pix4D elevation products are on the manifest even
though no surviving script reads the DSM.

Highest-value entries to locate, because they block the most:

| # | id | Why |
|---|---|---|
| 1 | `bench_workbooks`, `confusion_workbooks` | Every reported accuracy figure was read from these 64 workbooks. Without them, no headline number can be checked against its own source |
| 2 | `wv2_corrected_16m` | Co-registration reference for the entire satellite arm. No script produces it |
| 3 | `drone_chm` | Required by every Drone+CHM stack. No script produces it |
| 4 | `wv2_train_points_combined` | The field-only baseline behind the 6.1% improvement claim |

**7.1 CONFIRMED.** Two rainfall workbooks ship in `Analysis/Data/` and are
referenced by no script in any repository. Either an analysis is missing or they
are vestigial.

### 7.4 The data was found. Server scan, 2026-08-16

Run on `TESS` / `10.164.88.16`. The machine has two filesystems, `/` and `/raid`;
no NAS mounts and no Samba shares, so `/raid` was the only candidate. Three
Neltuma project trees survive in `/raid/home/gs558` (Glen Slade), all of them git
repositories:

| Tree | Files | Contents |
|---|---|---|
| `Glenn-Prosopis-ML` | 1,066 | **The data root.** `data_in/` (528) and `data_out/` (281) |
| `MLR3_pipeline` | 370 | Pipeline code plus a Bokspits_1-only data subset |
| `slade-prosopis` | 258 | The unarchived analysis repo |

Outcome against the 66 manifest entries: **23 found, 9 partial, 34 missing.**

Two traps in the scan itself, recorded so the numbers are not misread:

- An `E:` directory in Glen's home raised hopes of the lost `E:/Glenn/Botswana/`
  tree. It holds 5 Metashape files and nothing else.
- `Reproducibility/` and `share/` in the same home belong to a **different
  project** (LAZ point clouds, 64-plot CHMs, GCP surveys). Their `*_chm.tif`
  files collide case-insensitively with the manifest's `*_CHM.tif` glob and
  inflated `drone_chm` from 7 matches to 4,707. Rescoped to the three real trees,
  every entry count is stable. **The discovery script matches on filename only**;
  on a shared machine that needs a scoped index, not a whole-filesystem one.

**7.5 RESOLVED. `drone_chm` does not exist as a standalone product.** Its 7
matches are the same files as `drone_refl_stack_chm` — the CHM is a *band inside*
`*_Refl_StackCrop_CHM.tif`. Since `drone_pix4d_dsm` and `drone_pix4d_dtm` are
both absent from the machine, the DSM-minus-DTM rebuild path assumed in §7.3 and
in `docs/data-collation.md` has no inputs. The CHM can only be extracted from the
stacks that already exist. Supersedes the rebuild note in 7.3.

> **CORRECTION, 2026-08-17.** The heading overstates this. `drone_chm` does not
> exist as a standalone product *on the server*, but it did exist in the original
> project tree: `makeReflStacks_+CHM_terra.R` reads
> `E:/Glenn/Botswana/ReflStacks/<site>_CHM.tif` as an already-complete file. The
> CHM was an independent raster that failed to survive, not a band that never had
> separate existence.
>
> The inference drawn from that overstatement was also wrong. This entry treats
> the missing DSM and DTM as breaking a rebuild path the drone arm depends on. It
> does not: no script anywhere performs DSM minus DTM, and the drone arm reads
> only the CHM. The elevation products are consumed solely by
> `Terrain_Analysis.R`.
>
> What survives unchanged: no script in any repository produces the CHM, so its
> provenance is genuinely outside the codebase and still unexplained. See 5.3 and
> 7.16.

**7.6 RESOLVED. `wv2_raw_order3` is not a real delivery.** The ESA archive
contains exactly two orders, `050132961020_01` and `050132961010_01`. The ID
`050132273010_01_P002`, which appears only as band names in
`WV2_training_points_extract.R`, is nowhere in it. Its acquisition stamp
`21OCT19084820` is *identical to order 1*, so it is a stale reference to an
earlier delivery of the same scene, not a third acquisition. Manifest entry
flipped to `unknown_lost` with the reasoning recorded.

**7.7 The headline recovery: every accuracy workbook survived.** 49
`bench_workbooks` and 76 `confusion_workbooks`, against 32 expected each. Every
reported accuracy figure can now be checked against the file it was read from,
rather than only recomputed. The excess is informative in itself: it includes an
**eighth site, `Dinaka`**, absent from the manuscript's seven, and 7 Landsat runs
at multiple purity thresholds.

**7.8 The Landsat arm was run.** 7 LS8 bench workbooks and 7 LS8 classification
rasters exist. Whether it belongs in the paper remains Andy's call (open question
6.3), but "not reported" no longer implies "not done".

**7.9 Confirmed absent from the entire machine.** No hex grid of any kind, and no
QGIS project (`.qgz`/`.qgs`) either — so Fig 8, Table 1 and the invasion-phase
geometry cannot be recovered here, and 3.2's "produced outside R" finding stands
with no surviving artefact. Also absent: all four Pix4D raw products, any
Sentinel-2 `.SAFE` or `*T34JDR*` scene, `wv2_classification_rf`,
`wv2_train_points_combined`, the settlement and road buffers, and the cover
tables. 27 entries were flipped to `unknown_lost`.

One candidate worth verifying: `slade-prosopis/output_data/WV2_train_point_extracted.csv`
may be `wv2_train_points_combined` under a different name. Unconfirmed.

**7.10 Open question 6.4 was not settled by the histories.** The home-directory
`.Rhistory` belongs to the other project entirely. `slade-prosopis/.Rhistory` is
empty. `Glenn-Prosopis-ML/.Rhistory` is substantial but contains no
`equal_class_size` reference, so which training rung each satellite run consumed
is still unknown. Its real value was 3.3. Two `.RData` files (165 MB and 261 MB)
hold live session state and were **deliberately not opened** — the risk of
reasoning from an undocumented session snapshot outweighs the chance it answers
6.4.

### 7.11 Where the data now lives

Mirrored into a gitignored `data-in/` in this repository, sensor-then-location,
by [`tools/mirror-inputs.sh`](tools/mirror-inputs.sh):

```
data-in/
  drone/{site}/       aoi.* field_points.* refl_stack.tif  chm.tif
                      refl_stack_chm.tif ndvi/savi/msavi/msavi2/mtvi.tif
  wv2/raw/{order}_01/ {order}_01_P001_MUL/  {order}_01_P001_PAN/  GIS_FILES/
  wv2|s2|planet/grids/{site}.*
  shared/veg_type_lookup.xlsx
  provenance.csv
```

`chm.tif` is written by [`tools/split-chm.sh`](tools/split-chm.sh) after
mirroring, not by the mirror itself — see 7.17. `refl_stack_chm.tif` is retained
for now and is redundant once that split is verified.

198 files for the drone arm and grids, ~47 GB. Raw deliveries keep their Maxar
directory names: `Mosaic_tiles.R` globs those literal paths, and the order ID is
the ESA licence provenance record. `provenance.csv` records source path, byte
size and mtime for every mirrored file.

The raw WorldView-2 data came **not** from this machine but from a SharePoint
archive, and is the one genuinely irreplaceable input that the server never had.
Verified against the manifest on arrival:

| Product | Tiles | Pixel | Bands | CRS |
|---|---|---|---|---|
| Order 1 MUL `050132961020` | 15 (R1–R5 × C1–C3) | 1.6 m | 4, UInt16 | EPSG:32734 |
| Order 2 MUL `050132961010` | 4 (R1–R4 × C1) | 2.0 m | 4, UInt16 | EPSG:32734 |
| PAN, both orders | 19 | 0.4 m | 1, UInt16 | EPSG:32734 |

Both MUL counts match `expected_count` exactly.

**7.12 Two data facts the manifest did not record.** The 19 panchromatic tiles
shipped with both orders and were never catalogued; added as `wv2_raw_pan`. No
code reads them, so pan-sharpening was available and appears not to have been
used. Separately, **the MUL product is 4-band, not WorldView-2's native 8**. That
is consistent with the `_1`…`_4` band-name suffixes in
`WV2_training_points_extract.R`, but it should be checked against whatever §2
claims about the WV2 predictor set. **[ANDY]**

**7.13 A CRS gap.** `WV2/WV2_clip.shp` ships with no `.prj` sidecar, so the AOI
that every WV2 mask and area figure depends on has no declared CRS. Bears on the
unresolved 445 vs 450 km² discrepancy.

**7.14 The CHM band is mislabelled `dsm` throughout.** Band 6 of
`*_Refl_StackCrop_CHM.tif` carries the description `<site>_MS_RGB_dsm`, and
`DRONE_STACK_BANDS` in `build_cube_variants.R` faithfully calls it `dsm`. The
values are *not* a DSM: they range −0.52 to 14.18 m at Bokspits_1 and −0.54 to
9.48 m at Struizendam_1, which is height above ground, not elevation above
datum. So the data is a genuine CHM and the label is inherited from the Pix4D
DSM file it was differenced from. Harmless as long as it is understood, actively
dangerous if someone later "fixes" the pipeline to treat that band as elevation.
Recorded explicitly in `inst/config/stacks.csv`. Refines 7.5: the differencing
happened upstream in Pix4D, which is why no script in any repository performs it.

**7.15 Finding 2.6 re-verified against the mirror, and one thing added.** Counting
features directly out of `data-in/` reproduces 136, 180, 222, 136, 146, 154, 82 =
**1,056**, independently confirming 2.6 and confirming the mirror preserved every
record. Open question 6.5 is therefore not something the scan can close: the data
side is settled and always was, and the missing 32 must be accounted for on the
manuscript side. **[ANDY]**

What *is* new: these layers are **POLYGON** geometry despite `points` in their
filename. The `_b30` suffix is a 30 cm buffer, which is why `build_ml_df` extracts
with `exact_extract(fun = "mean")` before taking centroids. See finding 4.15.

**7.16 CONFIRMED. Nothing upstream of the assembled drone stacks survived.**
Checked exhaustively against the full server file index while settling the CHM
question (5.3), because the earlier conclusion had been inferred from the absence
of a producer script rather than from a search:

| Probe | Hits on server |
|---|---|
| standalone `<site>_CHM.tif` | **0** |
| per-band `*_transparent_reflectance_{blue,green,red,red edge,nir}.tif` | **0** |
| any `4_index/` path | **0** |
| `3_dsm_ortho` anywhere | **0** |
| `*_dsm.tif` / `*_dtm.tif` matching a Neltuma site name | **0** |
| `*_DTM.tif` total | 133, **all** under `share/Reproducibility/Plot/DTM/` |
| `ReflStacks` | 4 hits, all of them *script filenames*, no directory |

The 133 DTM hits are plot-numbered (`P24_DTM.tif`, `Pbuf53_DTM.tif`) and belong
to the 64-plot LAZ study — the same directory whose case-insensitive `*_chm.tif`
files inflated `drone_chm` to 4,707 matches during the scan. Nothing was missed
the first time.

So every Pix4D-side product is gone: the individual reflectance bands, the
standalone CHM, the DSM and the DTM. The earliest surviving drone rasters are
`refl_stack.tif` and `refl_stack_chm.tif`, which is why the layout in 5.3 has to
be reached by decomposing them rather than by rebuilding from parts.

This does **not** make DSM/DTM recovery blocking — they are read only by
`Terrain_Analysis.R`, not by the drone modelling path (5.3). Recovering the Pix4D
projects would still be valuable: it would restore the native-resolution CHM and
allow the baked-in bilinear resample to be assessed rather than assumed. `E:/Glenn/`
held five Metashape files, so photogrammetry projects existed outside the git
repositories and may survive on external media. Worth asking Glen. **[HUGH]**

**7.17 DONE. The CHM is now a standalone raster again, provably losslessly.**
[`tools/split-chm.sh`](tools/split-chm.sh) extracts band 6 of
`refl_stack_chm.tif` into `data-in/drone/{site}/chm.tif`, so every drone product
is one independent file and predictor stacks can be assembled in the graph
instead of stored pre-combined. Seven files, 2.5 GB, 2m30s. Two exact
verifications, both passing at all seven sites:

| Check | Result |
|---|---|
| `chm.tif` vs band 6 of the source (`--verify`) | `max\|diff\| = 0`, `nodata_mismatch = 0` |
| bands 1-5 of `refl_stack_chm.tif` vs `refl_stack.tif` (`--verify-spectral`) | `max\|diff\| = 0`, `nodata_mismatch = 0` |

So `refl_stack.tif + chm.tif` carries exactly the information of the 6-band
stack, and the ~30 GB of retained `refl_stack_chm.tif` is now redundant. They are
kept until someone decides to drop them; nothing reads them.

Three details worth keeping, because each is a place this could have gone wrong
quietly:

- **The comparison is exact, not checksummed.** A first pass used GDAL band
  checksums and they agreed everywhere — but that value is 16-bit and collides
  freely here: three bands of `bokspits_2` share `41043`, and `struizendam_4` is
  the only site where all five differ from one another. Checksums were kept as a
  smoke test; the lossless claim rests on chunked exact arithmetic. The NaN masks
  are compared separately, since a difference raster cannot reveal a nodata cell
  that moved.
- **The output band is described `chm`, not `dsm`.** The source label
  `<site>_MS_RGB_dsm` is preserved in `ORIGINAL_BAND_DESCRIPTION` metadata
  alongside `SOURCE_FILE`, `SOURCE_BAND` and a `NOTE` recording the resample. The
  rename is deliberate: it removes the failure mode in 7.14 where someone
  "corrects" the pipeline to treat the band as elevation.
- **`chm.tif` is a reconstruction, not a recovery.** It is byte-exact with what
  the analysis consumed, but it is not the original `ReflStacks/<site>_CHM.tif`,
  which is gone. It is already cropped, masked and bilinear-resampled, so it is
  not interchangeable with a native-resolution CHM if the Pix4D projects turn up.
  The manifest records it as `availability = reconstructed` rather than
  `mirrored`, and `provenance.csv` marks the seven rows `DERIVED`.

`tools/mirror-inputs.sh` now points at this script, because a re-mirror without
it silently returns the CHM to having no standalone existence.

### 7.20 CONFIRMED. The vegetation-index rasters are on a different footprint

Found while assembling predictor cubes. The VI rasters were computed on the
**uncropped** mosaic, while `refl_stack.tif` and the CHM are cropped and masked
to the site AOI. At Bokspits_1:

| Raster | Dimensions | Extent (x) |
|---|---|---|
| `refl_stack.tif`, `chm.tif` | 9151 x 8221 | 470044.2 – 470563.9 |
| `ndvi/savi/msavi/msavi2/mtvi.tif` | 12209 x 10309 | 469955.1 – 470648.4 |

The grids *are* pixel-aligned — the offsets are whole pixel counts (1570, 1120,
−1488, −968) and the pixel size matches to floating-point noise — so the rasters
are co-registered. But the extents differ, and **`gdalbuildvrt` defaults to the
union of its inputs**. Assembling a cube without pinning the extent produces a
12209 x 10309 grid with the reflectance and CHM bands padded out in nodata: a
larger raster, every pixel index shifted, and nothing about the result looking
wrong. Every extracted training value would change.

`R/cubes.R` pins each cube to the reflectance grid with `-te` and then verifies
the assembled VRT against it rather than trusting the pin. Verified at
Bokspits_1 across all 11 bands of `5_CHM_ALLVI` against their source rasters at
3,000 sample points: `max|diff| = 0`, no nodata mismatches.

**A consequence worth carrying forward.** Because the VI rasters are unmasked,
the VI bands of a cube carry data where the reflectance and CHM bands are nodata
— inside the extent but outside the AOI mask. In the sample above, 2,727 of
3,000 points had VI values against 1,919 with reflectance. This is faithful to
the original, which combined the same products, and it does not affect training,
since the field polygons all sit inside the AOI. It *would* matter for
landscape-scale prediction, where a model could otherwise be asked to predict on
pixels with VI values and no reflectance. Masking is therefore a prediction-time
concern, recorded here so it is a decision rather than a surprise.

### 7.18 Open question 6.4 answered for WorldView-2, and 2.7 largely resolved

152 benchmark and confusion workbooks mirrored into `data-in/results/` by
[`tools/mirror-results.sh`](tools/mirror-results.sh) (1.6 MB). They settle what
the histories could not (7.10), without opening the `.RData` files.

**Method.** The confusion workbooks are class-by-class prediction counts. Under
`repeated_spcv_coords` with 10 repeats every observation is predicted once per
repeat, so for a balanced training set the column sums are
`equal_class_size × 10`. That makes the class size directly readable, and it is
self-checking: where sampling was balanced the column sums are *exactly* equal.

**The filename convention differs between repositories**, which is why the
filenames alone were ambiguous, and the arithmetic disambiguates it:

| Workbook | Column sums | Implied size | Therefore |
|---|---|---|---|
| `res.preds_svm_60_65` (Glenn-Prosopis-ML) | 600 | 60 | `<size>_<threshold>` |
| `Confusion_res.preds_S2_ens_65_100` (MLR3_pipeline) | 1000 | 100 | `<threshold>_<size>` |

If `65_100` meant size 65 the sums would be 650. Confirmed independently by
`equal_class_size_400_train_85`, whose sums are exactly 4000.

**What was actually run, against Table S7:**

| Sensor | Table S7 | Runs found in the archive | Verdict |
|---|---|---|---|
| WV2 | 280 @ 95% | 30@98, 30@99, 30@99-simple, **400@95** | **280 never ran** |
| PlanetScope | 200 @ 85% | 30@90, **400@85** | **200 never ran** |
| Sentinel-2 | 100 @ 65% | 60@65, **100@65** | **corroborated** |

For WV2 and Planet the *threshold* in Table S7 is right and the *class size* is
wrong. The obvious reconciliation — that `equal_class_size` is a cap and Table S7
reports the count achieved after purity filtering — **does not hold**: the column
sums are exactly 4000 across all six classes, so 400 per class was achieved in
full with no shortfall.

**Which WV2 run fed the manuscript.** §3 reports "mean overall accuracy of
75.8%". The `400_95_WV2e` ensemble aggregates to **0.7579**, and that workbook is
also the only WV2 run whose top two learners are SVM and the ensemble, matching
"SVM and ensemble models again performed best". So the reported WV2 classification
used **`equal_class_size = 400` at a 95% purity threshold**, and Table S7's 280 is
wrong. This answers 6.4 for WV2.

**Planet and Sentinel-2 cannot be pinned the same way.** The manuscript's 71.1%
and 70.3% (§3, Figure 7) do not correspond to any surviving benchmark: the S2
runs aggregate to 0.86-0.93 and the Planet runs to 0.78-0.83. Those figures are
almost certainly a different evaluation — accuracy against the drone reference
rather than internal resampling — so they identify nothing about the training
rung. For those two sensors the manuscript's own accuracy figures decide nothing.
They are settled instead by 7.19, on evidence rather than preference.

### 7.19 All three sensors settled. `sensors.csv` written, 2.7 closed

**Decision, 2026-08-17 [HUGH]: choose from the data alone.** "Follow the code" is
not a usable rule here, because the code is what contradicts itself in 2.7 — it
offers several values per sensor with nothing to arbitrate between them. The
archive does arbitrate.

**The discriminator is the learner graph.** The benchmark workbooks record the
full `mlr3` pipeline as the learner id, and the archive splits cleanly into two
architectures:

| Family | Learner graph | Runs |
|---|---|---|
| **A** | `scale_branch…pca…pre_unbranch` | WV2 400@95, Planet 400@85, Planet 30@90, S2 60@65 |
| **B** | `nop…` | S2 100@65, Planet 400@85-clean |

The WV2 run that demonstrably produced the reported 75.8% (7.18) is **family A**.
The reported satellite results therefore come from family A's pipeline, and the
correct configuration for each sensor is family A's run:

- **Sentinel-2 is decided outright.** Family A contains exactly one S2 run,
  `60 @ 65%`. The competing `100 @ 65%` — the one that corroborates Table S7 — is
  family B, a different architecture, written three weeks later. Table S7's S2
  class size describes a real run, but not the reported one.
- **PlanetScope is decided by time.** Family A holds both candidates, written the
  same afternoon: `30 @ 90%` at 14:25 and `400 @ 85%` at 18:32. The later is the
  refinement, and its 85% threshold is the one Table S7 reports.

**Result, now in [`inst/config/sensors.csv`](inst/config/sensors.csv):**

| Sensor | Purity threshold | Class size | vs Table S7 |
|---|---|---|---|
| WV2 | 95% | **400** | threshold right, size wrong (280) |
| PlanetScope | 85% | **400** | threshold right, size wrong (200) |
| Sentinel-2 | 65% | **60** | threshold right, size wrong (100) |

**Table S7's threshold row is entirely correct and its class-size row is entirely
wrong.** That is a single, clean correction to make to the manuscript rather than
three unrelated ones. **[ANDY]**

**One weaker link, recorded rather than smoothed over.** WV2 and S2 are read
directly off exactly balanced confusion column sums (4000 = 400 × 10;
600 = 60 × 10). Planet's are *not* balanced (4450–4650), so its 400 rests on the
filename convention plus family membership plus the timestamp, not on the
arithmetic. It is the least certain of the three, and the imbalance itself is
unexplained — worth revisiting if the Planet arm is re-run.

**Action item 5 closed.** All four config files now exist and validate:
`sites.csv` (7 × 14), `stacks.csv` (4 × 5), `sensors.csv` (3 × 8),
`resampling.yml` (7 keys).

### 7.21 The training tables reproduce the original's class sets exactly

The strongest reproduction evidence so far. `R/training.R` ports `build_ml_df`
per finding 3.3 — areal mean via `exact_extract` over the buffered field
polygons, centroids for coordinates, response `Type` as an ordered factor — and
the per-site class sets it produces match the archived confusion matrices at
**all seven sites**, code for code:

| Site | n | Reconstructed | Archived confusion |
|---|---|---|---|
| bokspits_1 | 136 | 1,2,3,4,5,6 | 1,2,3,4,5,6 |
| bokspits_2 | 180 | 1,2,3,4,5,6,7,13 | 1,2,3,4,5,6,7,13 |
| bokspits_3 | 222 | 1,2,3,4,5,6,7 | 1,2,3,4,5,6,7 |
| struizendam_1 | 136 | 1,2,3,5,6 | 1,2,3,5,6 |
| struizendam_2 | 146 | 1,2,3,5,6 | 1,2,3,5,6 |
| struizendam_3 | 154 | 1,2,3,6,7,10 | 1,2,3,6,7,10 |
| struizendam_4 | 82 | 1,2,5,6 | 1,2,5,6 |

Row counts equal `n_field_features` exactly and **no row is dropped for
incompleteness** — 4,224 rows across 7 sites x 4 stacks, zero NA. So the training
data going into the models is demonstrably the data that went into the originals,
independently of any manuscript claim.

**Finding 1.7 recomputed, and it is worse than Table S4 suggests.** The
manuscript claims "a minimum of 20 observations per class in each drone survey
area". Counting from the field layers, **8 of 41 site-class combinations fall
below 20**, at every one of the seven sites:

| Site | Under 20 |
|---|---|
| bokspits_1 | Short Grass n=19 |
| bokspits_2 | *Senegalia mellifera* n=16, *Stipagrostis amabilis* n=10 |
| bokspits_3 | *S. mellifera* n=18 |
| struizendam_1 | *Vachellia erioloba* n=14 |
| struizendam_2 | *V. erioloba* n=19 |
| struizendam_3 | *Boscia albitrunca* n=14 |
| struizendam_4 | ***V. erioloba* n=2** |

Two observations cannot support a per-class accuracy estimate under any
resampling scheme, and *V. erioloba* is the class the manuscript's central
confusion claim depends on (2.4). The §2.2 sentence needs correcting, and the
per-class accuracies for these combinations need a stated caveat. **[ANDY]**

### 7.22 The learner graph reconstructs to the original's own identifier

The archived benchmark workbooks record each learner as its full `mlr3` pipeline
id, which is effectively the graph written out. Rebuilding the graph from that
description in `R/models.R` regenerates the identifier:

```
archived   scale_branch.scale.no.scale.scale_unbranch.pre_branch.pca.nop.pre_unbranch.importance.classif.ranger
ours       scale_branch.scale.no.scale.scale_unbranch.pre_branch.pca.nop.pre_unbranch.importance.classif.ranger.tuned
```

identical but for the `.tuned` suffix `auto_tuner()` appends. So the
preprocessing structure — a tuned branch between scaling and not scaling, a tuned
branch between PCA and passthrough, an importance filter, then the learner — is
recovered rather than guessed, and the SVM's lack of an `importance` token in its
archived id is why SVM is unfiltered here.

Together with 7.21, the training data and the model structure are now both
evidenced against the originals. What remains unverified is the numbers, which
needs a full-budget run.

**A result to watch, not yet a finding.** Under the fast profile the untuned
`ranger` baseline is at or above every tuned learner on all four stacks:

| Stack | Best | Baseline |
|---|---|---|
| 5 | ranger.tuned 0.809 | 0.802 |
| 5_CHM | **ranger.untuned 0.831** | 0.831 |
| 5_CHM_NDVI | **ranger.untuned 0.838** | 0.838 |
| 5_CHM_ALLVI | ranger.tuned 0.831 | 0.831 |

This is **not** evidence about the reported pipeline: the fast profile allows 5
tuning evaluations over 3 folds, so the tuned learners are barely tuned, and
three outer iterations make the differences well inside noise. It is recorded
because if the baseline still matches at the full budget — 50 evaluations over 20
folds — then the elaborate graph is not earning its place, which is worth knowing
before defending it to a reviewer. The baseline exists in `resampling.yml`
precisely to make that answerable.

### 7.23 The tuning search space was almost entirely inert. Redesigned

**Decision, 2026-08-17 [HUGH]: stop reproducing the original tuning design.**
Auditing it element by element, against the installed package defaults rather
than from memory, found a search space that tuned preprocessing which cannot
affect these learners while tuning no learner hyperparameter at all.

| Element | Verdict | Evidence |
|---|---|---|
| `scale` / `no.scale` branch | inert for **every** learner | ranger and xgboost split on thresholds, so any monotone per-feature transform leaves the model unchanged. `classif.svm` leaves `scale` unset, so e1071's own `scale = TRUE` applies and SVM already standardises internally — `po("scale")` scales twice. |
| `pca` / `nop` branch | inert for SVM, harmful for trees | `po("pca")` defaults to `rank. = NULL`, keeping **all** components: a centred orthogonal rotation, not reduction. Orthogonal rotation preserves distances, so it is a mathematical no-op for linear and RBF SVM. For trees it perturbs splits with no principled benefit, and destroys the feature importance we want to report. |
| `importance.filter.frac` 0.1–1 | destructive floor, doubles cost | `filter.frac` is the fraction **kept**: at 0.1 it keeps **one** feature of 6, or one of 11. With deliberately chosen bands and 82–222 observations there is nothing to select away, and the filter trains an extra ranger on every evaluation purely to rank features. |
| svm `tolerance` 1e-4–2 | not a capacity parameter | the optimiser's stopping criterion. Affects convergence, not the hypothesis space. |

Strip those and **ranger and xgboost have nothing left being tuned** — which is
the explanation for the observation in 7.22 that the untuned baseline matched
every tuned learner. The pipeline was elaborate exactly where it could not
matter, and absent where it could.

It also explains the tuning archive. Measured on bokspits_3 at the original
budget, the 50 evaluated configurations gave **mean 0.884, sd 0.068, min 0.608,
max 0.946**. An sd of 6.8 accuracy points across configurations that are largely
equivalent *by construction* is not a response surface being explored, it is
noise being sampled — and a non-nested workflow reports its maximum, 0.946.

**The redesign.** Real hyperparameters via `mlr3tuningspaces`:

| Learner | Now tunes |
|---|---|
| ranger | `mtry.ratio`, `num.trees`, `replace`, `sample.fraction` |
| xgboost | `eta`, `max_depth`, `nrounds`, `subsample`, `colsample_bytree`, `colsample_bylevel`, `alpha`, `lambda` |
| svm | `cost`, `kernel`, `gamma`, `degree` — `lts("classif.svm.rbv2")` minus `tolerance` |

Inner resampling drops from 20 folds to **5**: inner CV only has to rank
configurations against one another, not produce a publishable estimate, and 20
folds on 82–222 observations is ~4–11 observations per fold. Final evaluation is
unchanged at `repeated_spcv_coords` 10 × 10.

Cost falls from 1,000 fits per tuning call to **250**, and dropping the filter
removes a second ranger fit per evaluation — together roughly **8×**, taking a
fully nested run from ~900 CPU-hours to ~110, or 2–4 hours wall.

**What this costs us, stated plainly.** Our learner ids no longer match the
archived ones, so the structural evidence in 7.22 is spent. The analysis *shape*
is still reproduced — per site, per predictor stack, five learner slots, spatial
CV, the same benchmark structure — but the tuning design is now ours. The
probabilistic and conformal treatment is deferred to the next refactor;
`predict_type = "prob"` is already in place as its foundation.

One fix needed on the way: `colsample_bylevel` and its neighbours declare a
dependency on `booster == "gbtree"`, which is xgboost's own default but is left
unset by mlr3, so the dependency cannot be verified and tuning aborts. Set
explicitly in `make_learner()`.

### 7.24 Compute behaviour, measured

Recorded because three successive intuitions about parallelising this pipeline
were wrong before measurement settled it. (a) future parallelism inside a
resample reaches **4.37x at 8 workers** (multicore and multisession within 1%) —
an earlier "futures buy nothing" reading was an own goal: `mlr3.exec_chunk_size`
of 10 against a 5-iteration loop makes one chunk and serialises everything.
Chunk size is 1 at this level, where each job is already seconds long.
(b) Crew across independent targets is still the better converter of cores to
throughput, hence the per-learner fit split (140+ schedulable units).
(c) Operational: killing `tar_make` kills neither the crew dispatcher nor its
workers — the dispatcher respawns killed workers, and orphaned workers once
burned ~56 cores for 41 minutes computing results nobody collected. Kill order:
tar_make, then the callr dispatcher, then workers.

### 7.25 Tuning: once per task, Bayesian, and the manuscript's claim made true

Two decisions, 2026-08-18 [HUGH]. **Tune once, evaluate fixed**: the nested
design re-ran the 250-fit search inside each of 100 outer iterations (25,100
fits per tuned learner-task) although repeats measure accuracy's fold
sensitivity, not the search's. Now: one 5-fold x 50-eval search per task, the
chosen config evaluated under the unchanged 10x10 outer CV (~350 fits, ~70x
less). The estimate is "accuracy of the chosen configuration", not "of the
procedure" — a far weaker leak than the never-reported winning inner score. The
full run dropped from a projected ~6 h to **21 m 27 s**. This also dissolved
the "lightgbm is slow" puzzle: 25,100 calls of fixed per-call overhead at
n<=222 swamps any library's tree-building advantage.

**Tuner: mlr3mbo at 30 evals** (from random search at 50). Paired on identical
outer splits across 112 tuned fits: mean delta **-0.0001**, 54/112 improved —
same quality at 60% of the budget. Pleasingly this makes §2.5's "Bayesian
optimisation" claim TRUE; finding 1.4 records that the original never ran it.
Gap hit on the way: mlr3mbo's GP surrogate needs `DiceKriging` (+`rgenoud`) for
all-numeric spaces; a smoke test that only exercised the mixed-type svm space
missed it. Both installed and locked.

### 7.26 First complete drone-arm results (full profile, spatial CV)

168 fits, 7 sites x 4 stacks x 6 learners, MBO-tuned, snapshotted in
`data-out/results/`. Headlines against the manuscript:

- **Figure 4B's stack ordering reproduces exactly**: 5_CHM_NDVI (0.878) >
  5_CHM_ALLVI (0.874) > 5_CHM (0.869) > spectral-only (0.856). The CHM's ~+2
  points are real under honest spatial CV.
- **"SVM and ensemble performed best" half-survives**: svm best in 18/28, mean
  0.897 vs ~0.86 for the rest, and the only learner where tuning clearly pays.
  The ensemble is mid-pack (best in 3).
- **"~90% overall accuracy" is ~2-3 points optimistic**: grand mean 0.869,
  per-site bests 0.81-0.96. "Mean ~87%, up to ~95% at the best sites" is the
  defensible restatement.
- **Tuned ranger vs untuned baseline: 0.862 vs 0.861.**
- **0 of 28 site-stack winners are clear** of the winner's own iteration sd
  (~0.09-0.13): report "svm consistently at or near the top", not a winners
  table. **[ANDY]**

### 7.27 Why SVM beat the trees: the boundaries are linear

Asked why svm dominated (7.26), the tuner's own choices answered: **17 of 28
winning SVM configurations chose a linear kernel** (8 radial, 3 polynomial).
Three mechanisms, in order of weight: the features are polygon-mean
reflectances, so noise is pre-averaged and classes separate along smooth
spectral/height gradients where a max-margin hyperplane is the right inductive
bias; trees approximate oblique hyperplanes with axis-aligned staircases whose
stairs each cost data that n = 82-222 does not have; and spatial CV holds out
whole clusters whose shifted feature distributions punish non-extrapolating
piecewise-constant models, while linear decision functions extend beyond the
training hull. Consistent with tuned ranger ≈ untuned baseline: no
hyperparameter fixes an inductive-bias mismatch.

### 7.28 Confirmed: a penalised multinomial matches everything

glmnet (lts default: alpha, s) added as the direct test of 7.27, and it **tops
the table**: mean 0.892 vs svm 0.889, best learner in 15 of 28 tasks, and wins
the paired-by-task comparison against svm 17/28 (mean +0.003). The simplest,
fastest model in the pool matches or beats every tree ensemble and the stacked
ensemble. **[ANDY]** This materially strengthens the paper's framing: the
predictive signal lives in the constructed features (polygon means, CHM,
indices), not in model complexity - and the workhorse claim should arguably be
"a regularised linear classifier suffices", which is a more interesting
ecological statement than a learner bake-off.

Operational notes from the same round: glmnet's multinomial refuses folds where
a class has 0-1 observations, which finding 1.7's n = 2 V. erioloba at
struizendam_4 guarantees under 5-fold CV - all learners now run encapsulated
with a featureless fallback so fold-level failures score rather than crash. And
ranger's crew-safe `num.threads = 1` silently carried into landscape prediction,
where the regime inverts (few heavy targets, idle machine): a 224M-pixel
single-threaded ranger predict ran ~2 h before being killed; prediction now
tiles across forked workers (`NELTUMA_PREDICT_CORES`).

### 7.29 First full-resolution landscape surfaces

Seven class + probability surfaces (5_CHM_ALLVI, winning learner per site,
masked to AOI). Neltuma cover: 0.4-3.9% at six sites, **16.8% at
struizendam_4** - ecologically plausible, and the per-site whole-surface mean
top-class probability (0.75-0.96) is a confidence figure the original could
never report (1.5). These feed the Figure 4/6 reproductions next.

### 7.30 Class palette defined, CVD-validated, and the first map figure

`classes.json` now carries a color per class - the palette never existed in any
repository. The nine classes that appear on the drone maps were validated
computationally, not by eye: **all 36 pairs reach OKLab dE >= 15 for normal
vision and >= 8 under Vienot protan/deutan simulation** (Python implementation
of the dataviz validator; no node on this host). Getting there took ten
iterations and two structural concessions worth recording:

- **Neltuma is magenta**, not red: red-vs-green collapses for deuteranopes, and
  Neltuma-vs-*V. erioloba* is the exact confusion pair the manuscript turns on
  (2.4), so the map must hold it for CVD readers. Magenta's blue content
  survives both protan and deutan simulation against every woody class.
- **Two rare classes left their semantic hue**: *V. erioloba* is orange and
  *Boscia* near-navy, because five greens/olives cannot pairwise-separate at
  this standard. Legend-anchored rare classes can afford it; the dominant
  cover classes (bare cream, grass yellow, *Rhigozum* olive) stay semantic.

`R/figures.R` renders the Figure 4A analogue: seven full-resolution surfaces,
winning learner and accuracy per panel, modal-aggregated for display, italic
binomial legend built from `class_labels()` (8.6). Two environment notes: the
installed `ragg` fails with "Graphics API version mismatch" and `ggsave`
auto-selects it, so the device is pinned to cairo png; and the figure's
dependency list is built from `SITES` so the fast profile resolves.

**A caveat the maps carry forward**: these are raw per-pixel classifications.
The original applied a 25-cell modal focal filter before area accounting
(finding 1.6 - which also records that the manuscript describes a sieve filter
that was never implemented). Our class areas will therefore differ from
smoothed ones; whether to add an explicit, honestly-described smoothing step is
an open design decision. **[HUGH]**
---

## 8. Class scheme

Standardised into [`inst/config/classes.json`](inst/config/classes.json), with
accessors in [`R/classes.R`](R/classes.R). This supersedes
`Neltuma_Mlr3_Pipeline/data_in/Veg_type_lookup_list.xlsx`, which must not be read
directly. Action item 2 closed.

**8.1 CONFIRMED. What Andy's correction changed.** Commit `567696e`
(2026-08-10) did two things to the lookup, both correct:

- Added a header row (`Class_Number`, `Class_Name`, `Class_Description`). The
  original file had none; its first row was class 1.
- Replaced colloquial names with accepted binomials: Prosopis to *Neltuma*,
  Bare Sand to Bare Ground, Gnidia to *Gnidia polycephala*, Camel Thorn to
  *Vachellia erioloba*, Rig Trig to *Rhigozum trichotomum*, Acacia Melifera to
  *Senegalia mellifera*, Blue Bush to *Diospyros lycioides*, Shepherds Tree to
  *Boscia albitrunca*, Candle Bush to *Vachellia hebeclada*. Class 13's
  description gained "Mostly *Stipagrostis amabilis*", which is what licenses
  Tables S3 and S4 calling it by the binomial.

All superseded names are preserved as `aliases` in `classes.json` so legacy
outputs can still be migrated.

**8.2 CONFIRMED. The correction silently breaks the current code.**
`build_ml_df()` calls
`read_xlsx(lookup_file, col_names = c("Type", "Class", "Description"))`.
Supplying `col_names` declares the file has **no header**. That was true of the
original file and false of the corrected one. Running the pipeline as it stands
against the lookup on `main` ingests the header as data row 1, coerces `Type` to
character, and breaks the join against the numeric `Type` in the field
shapefiles. `R/classes.R::assert_not_legacy_lookup()` guards this path.

**[ANDY] 8.3.** The corrected class names do not fully agree with Tables S3 and
S4, so the manuscript needs updating either way:

| Lookup (corrected, authoritative) | Table S3 / S4 | Action |
|---|---|---|
| `Short Grass` (3) | `Grass` | Pick one. Table S3 also names the assemblage |
| `Tall Dune Grass` (13) | `Stipagrostis amabilis` | The binomial is better and the corrected description now supports it |
| `Calcrete` (8) | absent; Table S3 folds calcrete into Bare Ground | Decide whether Bare Ground includes calcrete |
| `Diospyros lycioides` (9) | absent | Never classified. Drop from the lookup or note as unused |
| `Other` (11) | absent | Excluded by its own description |
| absent | `Mixed woody cover` | Satellite-only class with no code. **UNRESOLVED**, most likely code 6 under the simple scheme, must be confirmed against archived training sets |

**8.4 CONFIRMED.** Class code `53` appears as `frac_53` in the Sentinel-2 purity
filters and balanced-sample counts, with no entry in any lookup. Origin unknown.
Recorded under `unresolved_codes` in `classes.json`.

**8.5 CONFIRMED.** Under the `simple` four-class scheme, code 6 no longer means
*Rhigozum trichotomum*; it means any woody vegetation that is not *Neltuma*
(codes 5 and 7 are recoded into it). Any output that mixes the field and simple
schemes without relabelling is wrong. `class_labels(simple = TRUE)` handles this.

**8.6.** Using `label_md` everywhere prevents recurrence of the `Rhigosum`
misspelling Reviewer 1 flagged across Figures 3, 8, S1 to S8, S10 and S11.

---

## 9. Environment

Step 2 of the server handover. Full detail in
[`docs/environment.md`](docs/environment.md); this section records only what a
future reader would otherwise have to rediscover the hard way.

**9.1 CONFIRMED. Pre-built binaries cannot be used for the spatial stack on this
host, and the failure is silent until load time.** `uvr` defaults to Posit
Package Manager binaries and correctly identifies the host as Ubuntu 24.04
("noble"). But P3M's noble builds link the GDAL from noble's own archive
(3.8.x, `libgdal.so.34`) while the machine carries ubuntugis GDAL 3.11.4
(`libgdal.so.37`). Sonames are the ABI contract, so the binary cannot resolve
its own dependency:

```
$ ldd .uvr/library/terra/libs/terra.so
    libproj.so.25    => /lib/x86_64-linux-gnu/libproj.so.25       (ok)
    libgdal.so.34    => not found
    libgeos_c.so.1   => /lib/x86_64-linux-gnu/libgeos_c.so.1      (ok)
```

GEOS and PROJ happen to match, which is why this presents as a partial breakage
rather than an obvious one. Not a `uvr` defect and not fixable by a `uvr`
upgrade — it is a host/repository mismatch. It affects `terra`, `sf` and
`exactextractr` today and every future GDAL/GEOS/PROJ-linked package.

**9.2 The fix is `UVR_NO_BINARY=1`, set globally via `tools/uvr-env.sh`.**
Source builds run each package's own `configure`, which shells out to
`gdal-config` on `PATH`, so the link target is the installed GDAL by
construction and cannot drift. `uvr` 0.4.6 offers no `uvr.toml` key for this —
flag or environment variable only — so the setting cannot live in the manifest
alongside the dependency list. It is deliberately blunt: every package builds
from source, not just the spatial ones. A per-package allowlist would be the
clever option and would rot at the first dependency change. Cost is modest;
terra is 1m20s wall at `-j16`, and `MAKEFLAGS` propagates through
`R CMD INSTALL`.

**9.3 `uvr sync --ignore-cache` does not replace an installed package.**
`--ignore-cache` skips the *download* cache only; the "already present in
`.uvr/library/`" check is separate, so sync reports `Everything is up to date`
and no-ops straight over a broken binary. A bad install must be evicted with
`rm -rf .uvr/library/<pkg>` first. Worth knowing before debugging a rebuild that
never happened.

**9.4 WITHDRAWN — nothing was missing, and the check was wrong.** This entry
originally claimed `libcurl4-openssl-dev` and `libtiff-dev` were absent and
needed root. Both were installed the whole time:

```
ii  libcurl4-openssl-dev:amd64   8.5.0-2ubuntu10.11
ii  libtiff-dev:amd64            4.5.1+git230720-4ubuntu2.5
```

The check tested for `/usr/include/curl/curl.h` and `/usr/include/tiff.h`.
Ubuntu installs both under the multiarch prefix `/usr/include/x86_64-linux-gnu/`,
so the probe missed them. **No system-level installation was needed for any part
of this environment.**

The lesson generalises, and is why 9.1 held up while this did not: 9.1 was
established by `ldd` against a real load failure, whereas this was inferred from
guessed paths. Interrogate the build system, never the filesystem —
`pkg-config --modversion`, `pkg-config --cflags`, `curl-config`, `gdal-config`,
`dpkg -l` — because those are what a package's own `configure` consults. An
unversioned `libfoo.so` in `ldconfig -p` is a further tell, since that symlink
ships only in the `-dev` package. Procedure recorded in `docs/environment.md`.

**9.4a CONFIRMED, and it is the real rendering problem.** The genuine issue was
never a missing library but an unreachable one. `rmarkdown` locates pandoc via
`RSTUDIO_PANDOC`, which Positron sets to its own bundled copy under
`~/.positron-server/bin/<build-hash>/quarto/bin/tools/x86_64`. That works in an
interactive session, breaks whenever the IDE updates its build hash, and is
**never set in a batch `Rscript` or `targets` run** — which is the only case that
matters for the pipeline. Measured directly:

| Context | `rmarkdown::pandoc_available()` |
|---|---|
| batch R, IDE variables stripped | **FALSE** |
| after `source tools/uvr-env.sh` | **TRUE**, pandoc 3.10 |

Resolved by installing Quarto 1.10.18 system-wide from the upstream `.deb` — it
bundles pandoc 3.10, so no separate `pandoc` package is needed — and pointing
`RSTUDIO_PANDOC` at `/opt/quarto/bin/tools/x86_64` in `tools/uvr-env.sh`.

Note also that `rstudio-server` ships Quarto 1.8.25 at
`/usr/lib/rstudio-server/bin/quarto/bin`, which precedes `/usr/local/bin` in
`PATH`, so the `.deb`'s symlink does not win and a bare `quarto --version` still
reports the old build. `tools/uvr-env.sh` prepends `/opt/quarto/bin`.

**9.5 This constrains the Docker work.** The base image must either carry a GDAL
whose soname matches the binaries it installs, or adopt the same source-build
policy. Pinning a `rocker/geospatial` tag satisfies the first only for as long
as that tag's GDAL and P3M's stay in step, which is precisely the assumption
that broke here. The second is what `tools/uvr-env.sh` already encodes and is
the safer default.

**9.6 R is not pinned.** No `.r-version`, so the project is bound to system R
4.6.0. Pinning would make the environment reproducible across machines but
forces a full rebuild against a uvr-managed R. Deferred, and recorded here so
the omission is a decision rather than an oversight.

**9.7 A pinned git dependency makes every later `uvr add` contingent on GitHub.**
`uvr add` re-resolves the *whole* dependency set, so adding an unrelated CRAN
package re-fetches the pinned `mlr3extralearners` sha from the GitHub API. If
that call fails the entire transaction rolls back and nothing is added. During
the GitHub incident of 2026-08-17 this blocked `uvr add tarchetypes` for over
half an hour: 12 attempts, mostly `504 Gateway Timeout`.

**A first diagnosis of this was wrong and is corrected here.** The initial
failure returned `429 Too Many Requests` and was recorded as rate limiting caused
by this session's own API calls. It was not. `GET /rate_limit` showed **57 of 60**
requests remaining, and `githubstatus.com` reported a `major` **Partial System
Outage**; subsequent failures were `504`, and a direct API call returned `504`
too. The status code was read as a cause without checking either the quota or
the service status. Same error of method as 9.4: inferring from one signal
instead of interrogating the authoritative source.

Mitigations, in order of preference: set a `GITHUB_PAT` (raises the quota, does
nothing for an outage); prefer CRAN over git pins where a CRAN release will do;
and note that `uvr` offers no offline resolution path even though `uvr.lock`
already carries the pinned URL and checksum, which is arguably a gap in the tool.

**9.8 CLOSED (2026-08-17). `uvr.lock` was knowingly incomplete.** To get past 9.7,
`tarchetypes` was declared with `uvr add tarchetypes --no-lock`, which writes
`uvr.toml` without resolving, and then installed straight from CRAN into
`.uvr/library/`. So the manifest and the library both have it and **the lockfile
does not**. This is deliberate and temporary, not an oversight.

**Reconcile as soon as GitHub is healthy:**

```sh
source tools/uvr-env.sh
uvr lock && uvr sync          # then confirm tarchetypes appears in uvr.lock
```

Until that is done the environment is not fully reproducible from `uvr.lock`
alone, which is the one property the whole `uvr` arrangement exists to provide.

**Resolved.** GitHub recovered and the lockfile was reconciled: `uvr.lock` now
carries `tarchetypes` 0.14.1 among 180 packages, and `uvr doctor` reports
manifest, lockfile and library in agreement.

---

## Changelog

- **2026-08-13** Phase 0.1 to 0.3. Cross-repo audit; recovered and reconstructed
  the missing pipeline functions; imported 23 orphaned scripts into
  `legacy_imported/`. Findings 1.1 to 1.8, 2.1 to 2.8, 3.1 to 3.2, 4.1 to 4.13
  recorded.
- **2026-08-14** Phase 1.3. Data manifest (66 entries), discovery script and
  server handover guide. Section 7 added. Phase 0.4 folded into
  `audit/source-recovery-map.md` and `legacy_imported/README.md` rather than
  written as a third overlapping document.
- **2026-08-14** Work handed off to the analysis server. Entry point
  `docs/server-handover.md`, covering data discovery, the uvr environment and the
  targets scaffold. Manifest switched to CSV with a `type` column: 25 of 66
  entries are true inputs, 41 are derived, 6 are derived with no producer and so
  behave as inputs. Sections 7.2 and 7.3 added.
- **2026-08-14** Phase 1.4. Class scheme standardised to
  `inst/config/classes.json` with accessors in `R/classes.R`. Section 8 added.
  **Finding 4.1 corrected**: the lookup-reading fault is a regression introduced
  by commit `567696e` adding a header row, not a pre-existing bug.
- **2026-08-16** Step 1 of the server handover completed. The data was located on
  `TESS` in `/raid/home/gs558`: 23 of 66 manifest entries found, 9 partial, 34
  missing. Sections 7.4 to 7.13 added; 27 entries flipped to `unknown_lost`; a
  `resolved_path` column added to the manifest, with 34 entries resolved. Two
  manifest rows added for data that shipped but was never catalogued
  (`wv2_raw_pan`, `veg_type_lookup`). Inputs mirrored into a gitignored
  `data-in/` by the new `tools/mirror-inputs.sh`; raw WorldView-2 supplied
  separately from SharePoint and verified against expected tile counts, pixel
  size, band count and CRS.
  **Open question 6.3 advanced**: the Landsat arm was run (7.8). **Open question
  on `wv2_raw_order3` closed**: not a real delivery (7.6). **Open question 6.4
  not settled** — the console histories carry no training-rung evidence (7.10).
  New findings 3.3 (`build_ml_df` recovered from `.Rhistory` and disagreeing with
  the repo copy) and 4.14 (filename casing). **Finding 7.3 partially superseded**:
  `drone_chm`'s DSM-minus-DTM rebuild path has no inputs, because the CHM is a
  band inside the stack and both Pix4D elevation products are gone (7.5).
- **2026-08-16** Step 3 begun. `inst/config/sites.csv`, `stacks.csv` and
  `resampling.yml` added, each derived from an observable source rather than from
  the manuscript: `sites.csv` read straight off the mirrored rasters and vectors,
  `stacks.csv` from `DRONE_STACK_BANDS`, `resampling.yml` from the scripts.
  **Action item 4 closed**; action item 3 (`predict_type = "prob"`) set globally.
  Findings 4.15, 7.14 and 7.15 added. `sensors.csv` deliberately left unwritten:
  its purity thresholds and per-class sizes are exactly what finding 2.7 shows
  Table S7 and the code disagreeing about, so choosing values would bury the
  contradiction rather than resolve it. **[ANDY]**
- **2026-08-17** Step 2. The R environment stands up. Section 9 added.
  `uvr` is in use, but its default P3M binaries are unusable on this host — they
  link GDAL 3.8 while the machine carries 3.11.4, so every GDAL-linked package
  fails at load (9.1). Fixed by forcing source builds through the new
  `tools/uvr-env.sh`, which must be sourced before any `uvr` command (9.2).
  `terra`, `sf` and `exactextractr` installed and verified against the mirrored
  data: GDAL 3.11.4, GEOS 3.12.2, PROJ 9.4.1, and `refl_stack_chm.tif` reads back
  the same 6 bands, EPSG:32734, 0.05679 m and CHM range that `sites.csv` and
  `stacks.csv` record. Environment procedure written up in
  [`docs/environment.md`](docs/environment.md), which also carries the
  consequences for the deferred Docker work (9.5).
- **2026-08-17** Drone data restructured and the satellite training question
  cracked open. `tools/split-chm.sh` separates the CHM from the spectral bands so
  every drone product is one independent raster; proven lossless twice over
  (7.17). Stack assembly stays a pipeline target, not a stored product.
  `tools/mirror-results.sh` mirrors 152 surviving benchmark and confusion
  workbooks into `data-in/results/`, which **answers open question 6.4 for
  WorldView-2** and largely resolves 2.7 (7.18): reading `equal_class_size` off
  the confusion column sums shows WV2 ran at **400 @ 95%** and Planet at 400@85
  or 30@90, so Table S7's 280 and 200 were never run, while its Sentinel-2 row
  (100 @ 65%) is corroborated by a real run. New finding **2.9**: the WV2
  per-learner accuracies in §3 (75.9% and 75.1%) appear in no surviving workbook
  and invert the true ranking, though the headline 75.8% reproduces exactly.
  Sections 5.3 and 5.4 record the CHM and file-format decisions; finding 7.5 is
  corrected in place and the earlier proposal to derive the CHM from DSM minus
  DTM is withdrawn.
- **2026-08-17** **Open question 6.4 closed and finding 2.7 resolved.**
  `inst/config/sensors.csv` written from the archive rather than from either the
  table or the code, closing **action item 5** — all four config files now exist
  and validate. The discriminator is the learner graph recorded in each benchmark
  workbook: the archive splits into a `scale_branch/pca` family and a `nop`
  family, and the WV2 run that demonstrably produced the reported 75.8% belongs
  to the former, so the reported satellite results are that family's (7.19).
  That decides Sentinel-2 outright — family A holds exactly one S2 run, 60@65,
  while the 100@65 run that would have corroborated Table S7 belongs to the other
  architecture — and PlanetScope by timestamp, 400@85 being the later of two runs
  written the same afternoon. **Table S7's purity thresholds (95/85/65) are all
  correct and its class sizes (280/200/100) are all wrong**, the true values being
  400/400/60: one clean correction rather than three. Planet is flagged as the
  weakest of the three, since its confusion column sums are not balanced and its
  class size rests on convention and timestamp rather than arithmetic. New finding
  **2.10**: the manuscript gives PlanetScope as 3 m in the Abstract, §2 and Table
  S7 but 4 m in the Figure 7 caption.
- **2026-08-17** The pipeline exists and runs. `_targets.R` with `R/config.R` and
  `R/manifest.R`: config is the single source of truth, the manifest is a
  contract, and a missing input stops the run with an acquisition note rather
  than being substituted or skipped. Static branching via `tarchetypes::tar_map`
  over sites, so every check is an individually named target
  (`refl_check_bokspits_1`) that names the offending site on failure. Input files
  and shapefile **sidecar sets** are tracked, so a changed `.dbf` or a vanished
  `.prj` registers (7.13). 27 targets under the fast profile, 55 under the full,
  0 errors; the full run reproduces finding 2.6's **1,056** field records as a
  computed target rather than a documented assertion.
  Two environment findings: **9.7**, a pinned git dependency makes every later
  `uvr add` contingent on the GitHub API — and the first diagnosis of that
  failure, as rate limiting, was **wrong** and is corrected in place: the quota
  was untouched and GitHub was in a major outage. **9.8** records that `uvr.lock`
  is knowingly incomplete as a result, and must be reconciled with
  `uvr lock && uvr sync` once GitHub is healthy.
- **2026-08-17** Predictor cubes. `R/cubes.R` assembles every stack in
  `stacks.csv` as a GDAL VRT from the independent rasters, closing the design in
  5.3: 28 cubes for 7 sites x 4 tags, 144 kB in total, nothing stored
  pre-combined. Bands are named in the VRT so a cube is self-describing and the
  `dsm`/`chm` confusion (7.14) cannot recur. New finding **7.20**: the VI rasters
  sit on the uncropped footprint, larger than the reflectance grid though
  pixel-aligned, and `gdalbuildvrt` unions by default — so an unpinned cube would
  silently shift every pixel index. Cubes are pinned with `-te` and verified
  against the reflectance grid; values checked band-by-band against source at
  3,000 points, `max|diff| = 0`. **Finding 3.3 resolved** without needing further
  data: all 29 surviving drone confusion workbooks carry numeric dimnames (codes
  1-7, 10, 13) rather than class-name strings, so the response variable was the
  field `Type` code, matching the `.Rhistory` copy of `build_ml_df` rather than
  the repo copy. **Finding 9.8 closed**: `uvr.lock` now carries `tarchetypes`.
  Pipeline at 132 targets, 0 errors.
- **2026-08-17** Training tables. `R/training.R` ports `build_ml_df` on the
  settled reading of 3.3. New finding **7.21**, the strongest reproduction
  evidence so far: the reconstructed per-site class sets match the archived
  confusion matrices at **all seven sites**, code for code, with row counts equal
  to `n_field_features` and **zero** rows lost to incompleteness (4,224 rows over
  7 sites x 4 stacks). The training data entering the models is demonstrably the
  data that entered the originals. **Finding 1.7 recomputed and strengthened**:
  8 of 41 site-class combinations fall below the "minimum of 20 observations per
  class" claimed in §2.2, at every site, the worst being *V. erioloba* at
  Struizendam 4 with **n = 2** — a class the central confusion claim in 2.4
  depends on. **[ANDY]** Pipeline at 172 targets, 0 errors.
- **2026-08-18** The model layer redesigned around measurement, and the first
  complete results. Per-learner fit targets with select-best; tune-once with the
  fixed config under the unchanged 10x10 outer CV (7.25); tuner switched to
  mlr3mbo at 30 evals after a paired comparison showed parity with random search
  at 50 (7.25); lightgbm added, xgboost's space capped, fit targets decoupled
  from the config so single-learner edits rerun only that learner. Full run:
  **21 minutes**. Findings 7.24-7.26; first drone-arm results summarised in 7.26
  and snapshotted. `R/predict.R` added: per-site landscape class + probability
  surfaces from the winning learner, masked to the AOI against 7.20, with class
  areas and a whole-surface confidence figure as pipeline targets.
- **2026-08-18** glmnet round and the linearity verdict. Findings 7.27-7.29:
  the winning SVMs were mostly linear-kernel, and a penalised multinomial added
  as the direct test **tops the 7-learner table** (0.892), best in 15/28 -
  model complexity is not where the signal is. Universal featureless fallback
  after glmnet died on 1.7's n=2 class; prediction parallelised across raster
  tiles after ranger's crew-safe single-threading crawled on a 224M-pixel
  surface. Seven full-resolution class+probability surfaces rendered; Neltuma
  0.4-3.9% cover at six sites, 16.8% at struizendam_4. Results snapshotted in
  data-out/results/final_7learner.rds.
- **2026-08-17** Models. `R/models.R` builds the spatial tasks, the learner graph
  and the benchmark, all driven from `resampling.yml`. New finding **7.22**: the
  graph reconstructed from the archived learner ids regenerates those ids exactly,
  bar the `.tuned` suffix `auto_tuner()` adds, so the preprocessing structure is
  recovered rather than guessed. With 7.21 the training data and the model
  structure are both now evidenced against the originals; the numbers are not,
  and need a full-budget run. `predict_type = "prob"` throughout closes **action
  item 3** in code, and the `ens_rf`/`ens_svm` mislabelling of 4.9 is corrected.
  Coordinates are excluded from the feature set (`coords_as_features = FALSE`) so
  no model can memorise location. The full modelling chain runs on the fast
  profile: 4 tasks x 5 learners in 1m42s, accuracies 0.73-0.84. Recorded but not
  claimed: the untuned baseline matches or beats every tuned learner at the fast
  budget, which is uninformative at 5 evaluations but would matter at 50.
