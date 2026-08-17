# Server handover

Single entry point for continuing this work on the analysis server. Covers
locating the input data and building the pipeline on top of it.

**Branch:** `refactor-v2.0`. `main` is untouched and stays that way until this is
dialled in.

**Repo:** `git@github.com:TESS-Laboratory/Slade_et_al_mapping_invasive_Neltuma_Kalahari.git`
(private). The server is `TESS` / `10.164.88.16`, user `hg499`.

---

## Why we are doing this

Two reviewers returned major revisions on the Neltuma manuscript. Glenn cannot
take it forward, so Hugh and Andy are. The response-to-reviewers document already
carries an agreed action list, items 1 to 10 assigned to Hugh.

The governing constraint, established during the audit: **the published code
archive (Zenodo `10.5281/zenodo.16681147`) cannot reproduce the paper.** Thirteen
functions were called but never defined; several reported outputs have no
generating code anywhere; and substantial analysis lives only in
`TESS-Laboratory/slade-prosopis`, which was never archived.

The end state is a `targets` pipeline that rebuilds every reported number and
every data-driven figure from a declared manifest, then a shift from hard
land-cover classification to probabilistic *Neltuma* prediction, then an
end-to-end reproducible Quarto manuscript.

---

## What is already done

Six commits on `refactor-v2.0`:

| Commit | What |
|---|---|
| `e6a1d49` | Recovered `predict_terra_tile` verbatim from `MLR3_pipeline`; reconstructed the four `build_cube_*` stack variants and the satellite cube builders; added loud placeholders for the five `build_ml_df_*` variants that cannot be responsibly reconstructed |
| `c0cada3` | Imported 23 orphaned scripts into `legacy_imported/` with per-file provenance headers; started `refactor-findings.md` |
| `5974ee6` `ffe1ac5` | 66-entry data manifest, dependency-free discovery script, collation guide |
| `06e06e0` | Class scheme standardised to `inst/config/classes.json` with accessors in `R/classes.R`, superseding the xlsx lookup |
| `5555562` | Manifest to CSV, `type` column splitting inputs from derived products |

### Read these first

| File | What it is |
|---|---|
| [`refactor-findings.md`](../refactor-findings.md) | **Start here.** Living record: 8 manuscript-vs-code divergences, 8 numbers that do not reconcile, missing implementations, 13 code defects, 6 open questions for Andy |
| [`audit/source-recovery-map.md`](../audit/source-recovery-map.md) | Where the code behind each manuscript output actually lives, across four repositories |
| [`docs/data-collation.md`](data-collation.md) | The data hunt in detail. Step 2 below is the short version |
| [`legacy_imported/README.md`](../legacy_imported/README.md) | The imported reference code and what it produces |

Do not rewrite `refactor-findings.md`. Append, and add a changelog entry.

---

## Step 0. Clone and verify

```sh
git clone -b refactor-v2.0 git@github.com:TESS-Laboratory/Slade_et_al_mapping_invasive_Neltuma_Kalahari.git
cd Slade_et_al_mapping_invasive_Neltuma_Kalahari
git log --oneline -6          # should end at 5555562
```

If SSH is awkward, `gh auth login` with the device flow works well headless.

---

## Step 1. Find the data

The single highest-value command, before any full scan. These directory names are
distinctive; if the original `E:/Glenn/Botswana/` tree survived anywhere, this
finds it:

```sh
find / -maxdepth 4 -type d \
  \( -iname 'Botswana' -o -iname 'ReflStacks' -o -iname 'Satellite_Data' \
     -o -iname 'GIS_aggregate' -o -iname 'Pix4d' -o -iname 'Glenn*' \) 2>/dev/null
```

Then scan. Needs only `bash`, `find` and `awk`:

```sh
# targeted, if the probe hit
tools/discover-inputs.sh -o discovery /path/that/hit /home/glenn

# or broad
find / -type f -print 2>/dev/null > /tmp/file-index.txt
tools/discover-inputs.sh --reuse-index /tmp/file-index.txt -o discovery
```

Outputs land in `discovery/`: a readable `discovery-report.md`, a machine-readable
`discovery-report.tsv`, and `found-paths.tsv` mapping manifest id to every match.
`discovery/` is gitignored, so commit a summary rather than the raw index.

### What matters

Of the 66 manifest entries, **25 are true inputs and 41 are derived**. A missing
derived file is usually fine because we rebuild it, and rebuilding is preferable
anyway since it puts the product under the pipeline's control.

Priority order:

| # | Manifest id | Why |
|---|---|---|
| 1 | `bench_workbooks`, `confusion_workbooks` | **Every reported accuracy figure was read from these 64 workbooks.** Without them no headline number can be checked against its own source, only recomputed |
| 2 | `wv2_corrected_16m` | Co-registration reference for the whole satellite arm. No producer |
| 3 | `drone_chm` | Every Drone+CHM stack. No producer, but rebuildable as DSM minus DTM if the Pix4D projects survive |
| 4 | `wv2_train_points_combined` | The field-only baseline behind the headline 6.1% improvement |
| 5 | `train_wv2`, `train_planet`, `train_s2` | Settles which rung of the training ladder each satellite run used |
| 6 | `drone_aoi_clip`, `wv2_aoi` | Small, but every mask, grid and area figure depends on them. Also settles 445 vs 450 km² |
| 7 | `hex_grids` | Fig 8 and Table 1 |
| 8 | `wv2_classification_rf` vs `_mlr3` | Open question 6.1, below |

Six entries are marked `derived` but read `NO PRODUCER FOUND`. Treat them as
inputs: `drone_chm`, `wv2_corrected_16m`, `wv2_train_points_combined`,
`planet_grid_95`, `cover_all`, `cover_resolution_simple`.

### Also hunt for these

Not in the manifest because we do not know their filenames, but each could settle
an open question outright:

```sh
find / -type f \( -name '*.qgz' -o -name '*.qgs' \) 2>/dev/null   # invasion phase was made outside R
find / -type f -name '.Rhistory' 2>/dev/null                       # would reveal which training set each run used
find / -type d -name 'Manuscript figures' 2>/dev/null
find / -type f -name '*T34JDR*' 2>/dev/null                        # Sentinel-2 source
```

### After the scan

1. Add a `resolved_path` column to `inst/manifest/data_manifest.csv` from
   `found-paths.tsv`, and set `DATA_ROOT`.
2. Flip `availability` to `unknown_lost` for anything genuinely gone, so the
   pipeline declares it explicitly rather than failing obscurely.
3. Per lost entry, decide: rebuild, substitute, or drop the dependent analysis.
4. Record the outcome in `refactor-findings.md` section 7.

---

## Step 2. Environment

> **Done, with one mandatory deviation. See [`docs/environment.md`](environment.md).**
>
> The warning below about P3M binaries was right, and stronger than expected:
> binaries do not merely "compile from source sometimes", they are *unusable* on
> this host. P3M's noble builds link GDAL 3.8 (`libgdal.so.34`); the server has
> 3.11.4 (`libgdal.so.37`). Every GDAL-linked package installs cleanly and then
> fails to load. **Source builds must be forced** — `source tools/uvr-env.sh`
> before any `uvr` command. Findings 9.1 to 9.3.
>
> **Every system library was already present and no admin was needed** for the R
> environment — including `libcurl4-openssl-dev` and `libtiff-dev`, which an
> earlier revision of this note wrongly reported as missing (finding 9.4).
>
> One root install was made, and for a different reason: **Quarto 1.10.18**, so
> that `knitr`/`rmarkdown` can find pandoc in batch runs rather than only inside
> an IDE session (finding 9.4a). It bundles pandoc 3.10; no separate `pandoc`
> package is needed.
>
> Two departures from the recipe below, both deliberate: R is **not** pinned —
> the server has 4.6.0, not the 4.6.1 named here, and pinning forces a full
> rebuild against a uvr-managed R (finding 9.6). And `qs2` has not been added
> yet; `tar_option_set(format = "qs")` in Step 3 needs whichever of `qs`/`qs2`
> the installed `targets` expects, so it is settled there rather than guessed
> here.

Use **`uvr`**, not `renv`. It pins the R version itself, which is the whole point
when moving between the workstation and this server.

```sh
curl -fsSL https://raw.githubusercontent.com/nbafrank/uvr/main/install.sh | sh

uvr init
uvr r pin 4.6.1                    # match the workstation
uvr add targets crew qs2 jsonlite
uvr add terra sf exactextractr
uvr add mlr3 mlr3spatiotempcv mlr3learners mlr3pipelines mlr3tuning \
        mlr3tuningspaces mlr3filters mlr3viz mlr3mbo
uvr add mlr3-org/mlr3extralearners@<sha>     # GitHub-only, pin the sha
uvr add ranger xgboost e1071                 # learner backends
uvr add dplyr tidyr purrr ggplot2 patchwork readxl writexl
uvr sync
```

Commit `uvr.toml` and `uvr.lock`.

**Before that, check the system libraries.** P3M binaries are weakest on Linux, so
`terra`, `sf` and `exactextractr` may compile from source:

```sh
gdal-config --version && geos-config --version && proj --version
```

If missing, they need installing at system level (`libgdal-dev libgeos-dev
libproj-dev libudunits2-dev`), which may need an admin. Document whatever is
required in `docs/`.

Also export a tool-independent snapshot into `audit/` as an escape hatch, since
`uvr` is pre-1.0:

```r
write.csv(as.data.frame(installed.packages()[, c("Package", "Version")]),
          "audit/package-snapshot.csv", row.names = FALSE)
```

### Do not skip

- **Retired dependencies** across the legacy scripts: `rgeos`, `rgdal`, `xlsx`
  (Java), `ggbiplot`, `bbplot`, and `library(read_xl)` which is not a package.
  None of it should enter the new pipeline.
- `windowsFonts()` / `windowsFont()` appears in 15+ scripts and hard-fails on
  Linux. Strip it.

---

## Step 3. Build the pipeline scaffold

Target layout:

```
_targets.R                    graph definition only
R/                            functions only, no top-level side effects
  classes.R                   DONE
  config.R  manifest.R  cubes.R  training.R  models.R  predict.R
  extract.R  landscape.R  figures.R  tables.R  claims.R  theme.R
inst/config/                  classes.json DONE; sites.csv sensors.csv
                              stacks.csv resampling.yml TO DO
inst/manifest/                data_manifest.csv DONE
```

Order of work:

1. **Config as targets.** `sites.csv` (7 AOIs), `sensors.csv` (grain, band map,
   grid path, purity threshold, class size per sensor), `stacks.csv` (the four
   predictor stacks, already encoded as `DRONE_STACK_BANDS` in
   `Neltuma_Mlr3_Pipeline/R/build_cube_variants.R`), and `resampling.yml` as the
   single source of truth for folds, repeats, tuner and budget. That last one
   closes action item 4.
2. **Manifest validation targets.** Every entry gets existence, checksum, CRS and
   band-count checks that fail loudly with the acquisition note from the manifest.
3. **`crew` controller** sized from config, plus a `profile: fast|full` switch so
   the whole graph runs end to end on reduced budgets before committing real
   compute.
4. **`tar_option_set(format = "qs")`**, `"file"` for rasters.

Then Phase 3 (drone arm) per the plan. Two things to carry in from the start:

- **`predict_type = "prob"` everywhere.** Action item 3, and the foundation for
  the conformal work later. Retain both hard and probability outputs.
- **Reuse rather than rewrite.** `Neltuma_Mlr3_Pipeline/R/{build_cube, build_ml_df,
  build_task, tune_lrnr, ml_resample, benchmark_lrnrs, predict_terra,
  terra_read_rows}.R` are sound. Delete `auto_ml_learner.R`, `auto_ml_tune.R`,
  `model_params.R`, `Temp_build_cube_wv2.R`, `Analysis/MLR_analysis/`,
  `scripts/ML-pipeline.R`, `tune_n_bench.R`.

### One trap to avoid on day one

`build_ml_df()` reads the class lookup with
`read_xlsx(lookup_file, col_names = c("Type", "Class", "Description"))`. Supplying
`col_names` declares the file has no header. That was true of the original file
and **false** since Andy's correction (`567696e`) added one. Running it as-is now
ingests the header as data, coerces `Type` to character, and silently breaks every
downstream join.

Use `R/classes.R::class_lookup()` instead. `assert_not_legacy_lookup()` guards the
old path.

---

## Open questions

Four need Andy. Two need whatever the scan turns up.

| # | Question | For |
|---|---|---|
| 6.1 | Do Fig 8 and Table 1 derive from the **Random Forest** product (what the hex extraction reads) while Fig 6C reports the **mlr3** product? If so the paper mixes two classifications silently, and Reviewer 2's objection lands on a number that does not describe the map it defends | Andy |
| 6.2 | Were the `frac_*` sub-pixel cover columns **predictors or filters** in the satellite models? If predictors, the models consumed drone-derived information directly and "cross-scale calibration" means something stronger than §2.5 says | Andy |
| 6.3 | Is the Landsat arm in scope? Not reported in the manuscript | Andy |
| 6.6 | Should §2.6 be rewritten to describe the modal focal filter that was actually run, or should the sieve-and-nearest-neighbour analysis be built as described? | Andy |
| 6.4 | Which training-set rung did each satellite run consume? Determines whether Table S7 or the code is right | Scan |
| 6.5 | 1,056 shapefile records vs 1,024 in Table S4 | Scan |

### The SPCV fix, worth doing early

`legacy_imported/spatial_cv/spatial-autocorrelation.Rmd` is the only file in any
repository using `spcv_block`, and it calls
`autoplot(block_cv, task = task, fold_id = 1:4, show_blocks = TRUE)` — exactly
the figure Reviewer 1 asked for at L239-242. It almost certainly seeded the
manuscript's "spatial block cross-validation" wording, but it runs on Bokspits_1
only, on a 5x aggregated cube, with an untuned `randomForest` at 6 folds x 3
repeats. It does not describe the reported models, which use `spcv_coords` at 20
folds and `repeated_spcv_coords` 10x10.

Promoting it to a pipeline target and re-running across all seven sites with the
reported learners answers R1's question, substantiates the abstract's
random-vs-spatial claim properly, and settles whether §2.5 should say "block" or
"coordinate-based k-means clustering". Pair with `legacy_imported/variogram/` to
justify a defensible block `range`.

---

## Bootstrapping a session on the server

Paste this into a fresh Claude Code session in the repo root:

> I am continuing the Neltuma Kalahari refactor on the analysis server, branch
> `refactor-v2.0`. Read `docs/server-handover.md` first, then
> `refactor-findings.md`. We are at step 1 (locating input data) and step 3
> (building the targets scaffold). Do not modify `main`. Append to
> `refactor-findings.md` rather than rewriting it, and add a changelog entry for
> anything you find.

---

## Ground rules

- Work on `refactor-v2.0`. Leave `main` alone.
- Nothing under `legacy_imported/` or `Neltuma_Mlr3_Pipeline/` runs as-is. It is
  reference, not runnable, until ported.
- No Excel in the new pipeline. `classes.json` supersedes the xlsx lookup.
- Reproduction targets **statistical equivalence, not bit-identity**.
  `set.seed(5446)` covers tuning and final resampling, but the balanced class
  sampling in the extract scripts calls `sample()` unseeded, so the exact training
  sets are unrecoverable. Report the delta for every headline number rather than
  claiming a match.
- Treat every manuscript number as unverified until recomputed. Eight already do
  not reconcile; see `refactor-findings.md` section 2.
