# Slade et al. — mapping invasive *Neltuma* in the Kalahari

Reproducible analysis for *"Detecting sparse biological invasions through
multi-scale remote sensing: a drone–satellite framework for mapping Neltuma
cover, with calibrated uncertainty, in dryland ecosystems"* (SW Kalahari,
Botswana). Part of the Oppenheimer Programme in African Landscape Systems
(OPALS, https://opals-exeter.org/).

The manuscript (`paper/manuscript.qmd`) and its Supplement
(`paper/supplement.qmd`) are pipeline products: every number is an inline
expression reading the `targets` store, and every figure and table is a target.

## What the pipeline does

1. **Drone arm** — per-site multispectral + CHM predictor stacks, six learners
   (five tuned by Bayesian optimisation plus an untuned random forest) evaluated
   under prediction-domain-matched spatial cross-validation (kNNDM, CAST), and
   mapped as the equal-weight average of the tuned learners' class probabilities.
2. **Discrete satellite classification** (WorldView-2, PlanetScope, Sentinel-2)
   — the comparison arm: field, drone-derived purity and archived training
   sources, whole-site hold-out evaluation with the Wasserstein mismatch *W*,
   conformal prediction sets, pixel-level agreement with the drone maps.
3. **Sub-pixel *Neltuma* cover** — the primary satellite product: Platt-calibrated
   drone probabilities area-averaged onto each satellite grid as the regression
   target; glmnet/ranger/LightGBM ensemble on leave-one-site-out folds;
   dissimilarity index and a coverage-driven area of applicability; DI-stratified
   conformal intervals with nested (honest) coverage; prediction-powered area with
   a site cluster bootstrap; invasion phases with interval ranges.

Design decisions and findings are logged in `refactor-findings.md` and
`docs/refactor-3.0-plan.md`; the reviewer orientation is `docs/reviewer-handoff.md`;
the review of the 3.0 rewrite is `docs/review-3.0.md`; claims changed from the
submitted manuscript are listed in `paper/changes-from-submitted.md`.

## Run it

```bash
source tools/uvr-env.sh                 # REQUIRED: source builds against the host GDAL, wires quarto

# fast smoke profile (two sites, tiny budgets, its OWN store; ~15-30 min)
NELTUMA_PROFILE=fast NELTUMA_STORE=_targets_fast \
  R -e 'targets::tar_make(store = "_targets_fast")'

# full profile (~day of compute on 64 cores; predictions dominate)
NELTUMA_PROFILE=full R -e 'targets::tar_make()'

# render only the manuscript + supplement from the full store
NELTUMA_PROFILE=full NELTUMA_STORE=_targets R -e 'targets::tar_make(names = c("paper", "supplement"))'

# unit tests (pure functions, no store)
bash tools/run-tests.sh
```

`_targets.R` refuses to run the fast profile against the full store
(`NELTUMA_PROFILE` unset defaults to `fast`). Compute knobs:
`NELTUMA_WORKERS`, `NELTUMA_ML_WORKERS`, `NELTUMA_FUTURE`,
`NELTUMA_PREDICT_WORKERS`, `NELTUMA_PREDICT_CORES`, `NELTUMA_TILE_CELLS`
(see the header of `_targets.R` and `docs/server-handover.md`).

## Layout

| Path | Content |
|---|---|
| `_targets.R`, `R/graph.R` | the graph, generated from `inst/config/sensors.yml` |
| `R/` | pure functions: cubes, training, models, resampling (kNNDM), predict, conformal, cover, figures, paper values, checks |
| `inst/config/` | sites, stacks, sensors, resampling/tuning, prediction, class scheme (`classes.json`) |
| `paper/` | `manuscript.qmd`, `supplement.qmd`, `changes-from-submitted.md`, `pipeline.css` |
| `tests/testthat/` | unit tests (`tools/run-tests.sh`) |
| `tools/` | environment (`uvr-env.sh`), data mirroring, vector conversion, OSM fetch, lock-file export |
| `data-in/`, `data-out/` | mirrored inputs and pipeline outputs (not tracked) |
| `legacy_imported/`, `Analysis/`, `Neltuma_Mlr3_Pipeline/`, `Image_Processing/` | the original scripts, kept for the audit trail |

## Environment

Managed with `uvr` (`uvr.toml`, `uvr.lock`); an equivalent `renv.lock` is
exported for reviewers (`tools/export-renv-lock.R`). See `docs/environment.md`.

## Data and archive

Ground-observed training and validation data: `Analysis/input_data`. Processed
drone products and classification outputs: https://doi.org/10.5281/zenodo.18506271.
Code archive: https://doi.org/10.5281/zenodo.16681147.

Contact: a.cunliffe@exeter.ac.uk, gs558@exeter.ac.uk
