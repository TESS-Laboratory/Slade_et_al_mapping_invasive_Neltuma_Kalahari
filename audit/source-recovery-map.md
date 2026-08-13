# Source recovery map

Where the code behind each manuscript output actually lives.

**Purpose.** The published archive (Zenodo `10.5281/zenodo.16681147`, this
repository) cannot reproduce the paper. Parts of the analysis live in three
sibling repositories, and parts were never written in R at all. This document
records what is recoverable, from where, and what must be reconstructed.

**Refs audited** (fetched 2026-08-13, read-only; nothing was pulled or merged):

| Repository | Ref | SHA |
|---|---|---|
| `Slade_et_al_mapping_invasive_Neltuma_Kalahari` (published) | `HEAD` | `a0dfb1f` |
| `TESS-Laboratory/slade-prosopis` | `origin/main` | `671e56f` |
| `GlennSlade/MLR3_pipeline` | `origin/main` | `fa61810` |
| `TESS-Laboratory/Glenn-Prosopis-ML` | `origin/master` | `6d9def5` |

Script counts (`.R` + `.Rmd`): published 108, `slade-prosopis` 166,
`MLR3_pipeline` 31, `Glenn-Prosopis-ML` 17. **94 script basenames present in
`slade-prosopis` are absent from the published archive.**

> Local working copies of `MLR3_pipeline` and `Glenn-Prosopis-ML` carry
> uncommitted modifications. These were inspected and are whitespace and
> reformatting noise only (trailing spaces, blank-line changes) with no
> functional content. They were deliberately left in place; all reading was done
> against the fetched `origin` refs.

---

## 1. Missing functions

Called by the published pipeline, defined nowhere in it.

| Function | Call sites | Recoverable from |
|---|---|---|
| `predict_terra_tile()` | 28 | `MLR3_pipeline@fa61810:R/predict_terra_tile.R` |
| `build_cube_5()`, `build_cube_5_CHM()`, `build_cube_5_CHM_NDVI()` | 3 | **Nowhere.** Reconstruct as band subsets of the canonical 11-band cube from `build_cube()`. |
| `build_cube_S2()`, `build_cube_LS8_T()`, `build_cube_WV2()`, `build_cube_Planet()` | 4 | **Nowhere.** Reconstruct per sensor. |
| `build_ml_df_WV2()`, `build_ml_df_WV2e()`, `build_ml_df_S2()`, `build_ml_df_Planet_e()`, `build_ml_df_LS8()` | 5 | **Nowhere.** Reconstruct as sensor-parameterised variants of `build_ml_df()`. |

Consequence: of the published run scripts, only `run.R` and the
`Development_test/run_choices*.R` family can execute at all, and those call
`predict_terra_tile` whenever `tile_split == "YES"`.

---

## 2. Manuscript outputs → source

Status key: **PUB** in this repo · **SIB** recoverable from a sibling repo ·
**RECON** must be reconstructed, no code exists anywhere.

| Output | Status | Location / note |
|---|---|---|
| Fig 4A/4B/4C (drone accuracy panels) | PUB | `Analysis/Benchmark_Analysis/benchmark_drone_full_combined.R`. Alternative renderings in `slade-prosopis`: `benchmark_drone_full_combined_figure.R`, `..._violin.R` |
| Fig 5 (sub-pixel cover by grain) | PUB | `Analysis/Comparison_Analysis/Satellite_Polygon_prosopis cover.R`. Landsat panel needs `slade-prosopis:Landsat/LST_Classification_Grid_Extract_Drone.R` |
| Fig 6A (satellite algorithm comparison) | SIB | `slade-prosopis:benchmark_plots_WV2.R` |
| Fig 6B (with vs without drone calibration) | RECON | Data exist (`benchmark_analysis_MLR.R` tags `points` vs `30_99`/`400_95`/`30_99_simple`); no script plots the contrast |
| Fig 6C, 7A–D, S1–S7 (map layouts) | RECON | Rasters are pipeline outputs; layouts were made in GIS. Pipeline will export styled rasters + shared legend |
| Fig 7E (accuracy by sensor) | RECON | `Analysis/Benchmark_Analysis/Full_accuracy_result_analysis.R` is the nearest thing and is broken past line 100, Planet only |
| Fig 8A (100 m cover), 8B (250 m phase) | RECON | See §3 |
| Fig S8 (drone PCA) | RECON | `prcomp`/`ggbiplot` exist only for WV2 (`WV2_training_points_extract.R`) and Planet (`Planet_Boravast_PCA.R`), both with magic column indices. No drone version |
| Fig S9 (band boxplots) | SIB | `slade-prosopis:Reflectance_Boxplots.R`. Reads `Vegpoly_DFB1.csv` from a **fourth** project root, `C:/Workspace/R_Scripts/Kgalagadi` — but that file is regenerable by `build_ml_df(df_type = "point")`, so no external input is needed |
| Fig S10/S11 (settlement / road profiles) | PUB | `Analysis/Eco_Analysis/WV2_Classification_buffer_extract.R`; variants in `slade-prosopis:Profile_Plots{,_2,_3,_4}.R` |
| Fig S12 (occurrence map) | RECON | Literature/field compilation, GIS |
| Table 1, Table S8 (invasion phase) | RECON | See §3 |
| Table S9 (plant-scale validation) | RECON | Inputs exist (`Analysis/Accuracy/Prosopis height points_extraction_accuracy.R`, n=184 confirmed against the two in-repo shapefiles). The 5×5 cross-tab and its percentages are computed nowhere |
| Table S10 (WV2 vs drone) | RECON | Paired columns produced by `WV2_Classification_Grid_Extract_Drone_Accuracy.R`; cross-tab and the 24.8% / 34.6% figures computed nowhere |
| Methods §2.6 (*V. erioloba* co-occurrence) | RECON | **Searched all four repos for `sieve`, `as.polygons(dissolve=TRUE)`, `st_nearest`: zero hits.** The described method was never implemented. The 63% and 27% are hardcoded `geom_text()` annotations in `Analysis/Eco_Analysis/Camel_thorn.R`, plotted from a hand-edited `temp.xlsx` |

---

## 3. Invasion phase: not implemented anywhere

Every tracked file in all four repositories was searched for `dominance`,
`incursion`, `invasion phase` and `pre-incursion`. **Zero hits.**

Table 1, Table S8 and Figure 8B were produced outside R, presumably in QGIS or
Excel. What does exist in `slade-prosopis:Aggregate/` is the hexagonal extraction
that feeds them:

- `Hex_Polygon_Grid_Creation_surveys.R` — grid construction
- `Hex_Grid_Extract_WV2.R`, `WV2_and_Drone_Classification_hex_Extract_Version_1.R`,
  and six `Hex_Grid_Extract_WV2_roll_time_series*.R` variants
- Grids are read as pre-made shapefiles from `E:/Glenn/Botswana/GIS_aggregate`:
  `hex_100m_wv2_wide_clip_minus_corner`, `hex_250m_wv2_wide_clip`,
  `hex_500m_wv2_wide_clip`

Two consequences:

1. **Table S8's "250 m hexagon" is correct and Figure 8B's caption ("250 m grid
   cells") is imprecise.** Hexagons are confirmed.
2. **The hex extraction runs against a Random Forest classification**
   (`RF_WV2_all_train_val_combined_b30_additional_WV2_merged_mosaic.tif`), not
   the mlr3 SVM/ensemble output that Figure 6C reports. If Figure 8 and Table 1
   derive from the RF product while Figure 6 reports the mlr3 product, the paper
   mixes two classifications without saying so. **Flag for Andy — this needs
   confirming before Table 1 is regenerated.**

---

## 4. The SPCV divergence has an explanation

`Glenn-Prosopis-ML@6d9def5:spatial-autocorrelation.Rmd` is the only place in any
repository where `spcv_block` appears. It is a teaching notebook that:

- compares `rsmp("cv")`, `rsmp("spcv_block", folds = 6, range = 100)`,
  `rsmp("spcv_coords", folds = 6)` and `rsmp("spcv_disc", folds = 6, radius = 50,
  buffer = 50)`;
- calls `autoplot(block_cv, task = task, fold_id = 1:4, show_blocks = TRUE)` —
  **exactly the visualisation Reviewer 1 asked for at L239-242**;
- runs `ml_resample(..., .folds = 6, .repeats = 3)` per regime and plots the
  accuracy distributions as violins, demonstrating that random CV is the most
  optimistic.

This is very likely where the manuscript's "spatial block cross-validation"
language and its random-vs-spatial comparison came from. But the notebook runs on
**Bokspits_1 only**, on a cube aggregated by a factor of 5, using an untuned
`classif.randomForest`, at 6 folds × 3 repeats. The reported results come from a
different configuration entirely (`spcv_coords`, 20 folds for tuning,
`repeated_spcv_coords` 10×10 for final accuracy).

So the abstract's claim is *substantiated in kind but not in configuration*: a
random-vs-spatial comparison was done, but not on the models whose accuracies
are reported, and not with block CV in the production pipeline.

**Action:** promote this notebook into the pipeline as a first-class target. It
answers R1's blocking question, it substantiates the abstract claim properly if
re-run across all seven sites with the reported learners, and it resolves whether
the methods text should say "block" or "coordinate-based k-means clustering".

---

## 5. Other material worth recovering from `slade-prosopis`

| Script | Why |
|---|---|
| `theme_fancy.R` | Canonical copy of the theme duplicated verbatim into 15+ scripts |
| `Planet_Struizendam_Merge.R` | Produces `Struizendam_2022_09_07.tif`, an input with no producer in the published repo |
| `Landsat/LST_Classification_Grid_Extract_Drone.R`, `Landsat_Polygon_Grid_Creation.R`, `LST_Boravast_VI.R` | The Landsat arm, needed for the 4-panel Fig 5 variant |
| `Prosopis_height.R`, `Prosopis_height_analysis.R` | Plant-scale validation, feeds Table S9 |
| `Majority_filter.R`, `WV2/WV_2_Majority_filter.R` | Nearest existing analogue to the §2.6 filter. Both are stubs: they read a raster and stop. Not a recovery path |
| `Variogram*.R` (6 files) | Empirical spatial-autocorrelation range, relevant to justifying block size if we adopt `spcv_block` |
| `Field_vs_Drone_Data_Compare*.R`, `Reflectance_extraction_base.R` | Reflectance extraction feeding Figs S8/S9 |

Out of scope for this paper but present: Rhododendron scripts, spectroscopy
resampling, questionnaire analysis, NDVI trend analysis, tree detection.

---

## 6. Project roots referenced across the codebase

Five distinct working-directory roots are assumed, none of which exist on this
machine:

```
E:/Glenn/Botswana/                            external Windows drive, all data
E:/Glenn/Botswana/R_Scripts/Glenn-Prosopis-ML
E:/Glenn/Botswana/R_Scripts/slade-prosopis
E:/Glenn/Botswana/R_Scripts/slade-Neltuma     (Eco_Analysis outputs only)
C:/Workspace/R_Scripts/slade-prosopis         (setwd() in 4 scripts)
C:/Workspace/R_Scripts/Kgalagadi              (Reflectance_Boxplots.R)
```

All are replaced by a single `DATA_ROOT` plus `data_manifest.yml`.
