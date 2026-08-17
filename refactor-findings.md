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

Which one produced the reported numbers is **OPEN**. It is settleable without
guesswork: the surviving `*ML_in_point_level.rds` files carry the column names,
so reading one shows whether `Type` or `Class` was the modelled response.

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
  drone/{site}/       aoi.* field_points.* refl_stack.tif
                      refl_stack_chm.tif ndvi/savi/msavi/msavi2/mtvi.tif
  wv2/raw/{order}_01/ {order}_01_P001_MUL/  {order}_01_P001_PAN/  GIS_FILES/
  wv2|s2|planet/grids/{site}.*
  shared/veg_type_lookup.xlsx
  provenance.csv
```

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

**9.4 Two dev headers are missing and need root.** `libcurl4-openssl-dev` and
`libtiff-dev`. Neither blocks the spatial stack, but the first blocks `curl` →
`httr`/`gh` and anything fetching over the network. Everything else the stack
needs is present, including `udunits2`, `sqlite3`, `zstd`, `lz4`, `openssl`,
`libxml2`, `netcdf` and the font and image libraries.

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
