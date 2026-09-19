# refactor-3.0: from reproduction to the paper we want

Branch `refactor-3.0`, cut from `refactor-v2.0` at `d067afe` (2026-09-16).
Audience: Hugh and Andy. Decisions needed are marked **[HUGH]** / **[ANDY]**
and collected in section 7. Findings referenced as 7.x live in
`refactor-findings.md`.

## 0. Where v2.0 leaves us

The reproduction draft is complete: 1,624 targets rebuild the manuscript's
numbers, figures and tables from the raw inputs, in ~38 CPU-hours. What it
established, in one line each:

| | Finding |
|---|---|
| Drone accuracy | ~87% grand mean, "~90%" 2-3 points optimistic; learner choice is within noise; boundaries are linear (glmnet top) (7.28) |
| WV2 accuracy | 64% under 10x10 spatial CV; the reported 75.8% is reproduced to the decimal by *non-spatial* CV, and is a function of fold count (0.49 at 5 folds -> 0.71 at 40) (7.34) |
| Cross-scale calibration | the +6.1% reverses: field points alone 66.8% > drone-purity training 64.3% (7.37) |
| Smoothing | the modal filter erases sparse Neltuma; at WV2 it halves recall for no precision; S10's +24.8% is two filters roughly cancelling (7.31, 7.35) |
| Phases | Dominance reproduces; 40% of the landscape flips Pre-/Initial-Incursion with the surface choice (7.35) |
| Grain | Neltuma precision against drone maps 0.40 -> 0.24 -> 0.15 (WV2 -> Planet -> S2); overall accuracy is not monotone (7.37) |
| Sub-pixel cover | 1.1% / 0.9% / 0.3% of pixels reach the purity threshold per sensor (Fig 5) |

Compute profile of the store: landscape predictions 11 h and 16 GB (the
per-class probability rasters), fits 7.5 h, tuning 4 h, everything else
under 1 h each.

## 1. What is actually being asked of the rewrite

### 1.1 The original plan (reviewer-response document, "To pick up with Hugh")

| # | Action | Status after v2.0 |
|---|---|---|
| 1 | targets across the workflow, provenance | done |
| 2 | standardise class names, xlsx -> json | done (classes.json) |
| 3 | class probabilities as the foundation for conformal | done (`predict_type = "prob"` everywhere; prob rasters written) |
| 4 | resolve the SPCV iteration discrepancy; audit blocking | done: 100 not 200; spcv_coords not block; fold-count sensitivity measured (7.34) |
| 5 | Quarto manuscript + SI, Word output | HTML done; **docx and SI pending** |
| 6 | audit what "uncertainty" the outputs support | done: none, beyond CV variance (1.5) |
| 7 | check every reported statistic | done: sections 1-2 and 7.x |
| 8 | capture the environment | done with uvr, not renv (9.x) **[HUGH: renv.lock alongside?]** |
| 9 | README / workflow docs | partial; README still the original stub |
| 10 | reproducibility checks against manuscript statistics | partial (validators on inputs; no statistic assertions) |
| 11 | probabilistic / conformal extension | **this refactor** |
| 12-14 | reconcile manuscript, SI, responses | Andy, after this refactor |

### 1.2 What the reviewers need to see

Reviewer 1: show the spatial blocking (which areas held out, L239-242); an
uncertainty layer (L253); ranges of km² and % invaded, not absolutes (L272);
a defensible inference method for the settlement/road relationships, citing
prediction-powered inference (L278); UA and PA for Table S9; a treatment
table instead of "the multispectral + CHM + NDVI stack"; the nested design
made visible (Fig 4).

Reviewer 2: satellite performance is overestimated and the 24.8% / 34.6%
discrepancy insufficiently disclosed; it must propagate to the abstract, the
conclusions and the area claims ("16 km² established, 356 km² expanding");
cites Angelopoulos & Bates (2021) on conformal prediction directly.

Every one of these maps onto a pipeline product below.

## 2. Design principles for 3.0

1. **Honest spatial evaluation is the result, not the obstacle.** Report the
   accuracy *curve* against holdout distance / fold geometry, and pick a
   primary design that matches the question ("a new area of this landscape").
2. **Probabilities are the product; hard classes are a view of them.** Every
   landscape output carries calibrated uncertainty, and every area statistic
   is an interval.
3. **One code path for every sensor.** The drone is the finest sensor, not a
   special case: sites x sensors x training-source x learner, one graph.
4. **Raw surfaces only, no smoothing at all.** The modal filter is dropped
   entirely (2026-09-19 [HUGH], superseding the D5 "sensitivity only"): with the
   C2 cover regression there is no hard per-pixel map to smooth, and the filter
   was a net recall penalty (7.35). Removal is explained to reviewers, not
   reproduced (rationale in refactor-findings.md 2026-09-19).
5. **Measured, not argued.** Fold-count curves, coverage curves, calibration
   plots and timing tables are figures, not paragraphs.

## 3. The probabilistic and conformal design

### 3.1 What conformal prediction gives us here

Split (inductive) conformal prediction turns any classifier's probabilities
into **prediction sets** with a finite-sample coverage guarantee: at level
alpha = 0.1, the true class is in the set for >= 90% of exchangeable test
points. For a sparse target the interesting outputs are:

- **set size per pixel** - the uncertainty layer R1 asked for; 1 = confident,
  2+ = ambiguous, 0 = "none of the classes fit" (novel cover);
- **"Neltuma cannot be ruled out"** - the pixels whose set contains Neltuma,
  which is the management map: where to look, with a stated error rate;
- **area bounds** - lower bound = pixels whose set is exactly {Neltuma}, upper
  bound = pixels with Neltuma in the set. That is the "range of km² and %"
  R1 wants, with the reviewer-requested over-prediction folded in.

### 3.2 Method choices **[HUGH]**

| Choice | Recommendation | Why |
|---|---|---|
| Score | LAC (1 - p_true) as primary, APS/RAPS reported | LAC gives the smallest sets at nominal coverage; APS gives better conditional coverage. Both are ten lines. |
| Class-conditional | **Mondrian (per-class) calibration** | Marginal coverage can be met by covering the common classes and failing Neltuma; per-class thresholds guarantee Neltuma's own coverage. |
| Calibration data | the out-of-fold probabilities we already produce (100 spatial-CV iterations per task) - cross-conformal / CV+ thresholds | Free: `run_resample` already stores them; spatially held-out by construction, so exchangeability is "a new spatial fold of this landscape", which is the deployment claim. |
| Levels | alpha in {0.05, 0.10, 0.20}; report coverage curves | The paper should show the trade-off, not one number. |
| Validation | empirical coverage per class and per site on spatial holdout; calibration reliability diagrams for p(Neltuma) | Coverage-by-site is the honest statement of spatial transfer. |
| Implementation | `R/conformal.R`, ours | Nothing installed does Mondrian split conformal for mlr3 classification; the maths is small and we want it auditable. |
| Not `learner_pi_cvplus` | regression-only (wraps LearnerRegr) - used for the fractional-cover arm (3.8), not here | For classification the recipe is: score s_i = 1 - p_i[y_i] on the out-of-fold probabilities, per-class threshold q_c = the ceil((n_c+1)(1-alpha))/n_c quantile, set = {c : 1 - p[c] <= q_c} on the landscape rasters. Cross-conformal (fold thresholds, full-data model) by default; exact CV+ (each fold model predicts the landscape, 10x cost) as a one-sensor check. |

### 3.3 Inference on the maps: prediction-powered inference **[HUGH]**

R1's L278 point is right: regressions on model output are inference on
predictions. Prediction-powered inference (Angelopoulos et al. 2023) is the
fix and fits our data exactly: the drone-overlap pixels are a *labelled* set
(drone class as label, satellite class as prediction), the whole scene is
the *unlabelled* set. PPI gives a confidence interval for **Neltuma area**
and for **cover-by-distance-to-settlement** that accounts for the
satellite's error rate measured on the drone overlap. It costs one function
and answers R1 and R2 in the same object.

### 3.4 Spatial cross-validation: kNNDM (decided 2026-09-16 [HUGH])

Prediction-domain adaptive evaluation (Linnenbrink, Nowosad & Meyer 2026,
arXiv:2605.13689; kNNDM, Linnenbrink et al. 2024, GMD 17:5897) replaces
block CV. Folds are built so the nearest-neighbour-distance distribution
between test and training points matches that between the *prediction
domain* and the training points; the mismatch is a Wasserstein statistic W
that we report. This is the honest evaluation for the question each map
answers, and it resolves 7.34 by construction rather than by choosing a
fold count.

- Implementation: **CAST::knndm via mlr3spatiotempcv's
  `repeated_spcv_knndm`** (already in our 2.3.5; CAST added to the
  environment). blockCV's port was considered and not adopted: no mlr3
  integration, a heavier GDAL-linked dependency tree, and comparing two
  implementations of one algorithm measures the port, not the method.
- Prediction domains, per question: drone arm -> the site's own AOI (the
  map is that site); satellite arms -> the 445 km2 study-area boundary. The
  two accuracies differ legitimately and are reported as such.
- k: 10 for the drone arm; for the satellite arms k chosen by W (the paper
  recommends 4-6 for severely clustered samples), with W and the NND ECDF
  figure shown - that is R1's "show the blocking".
- 10 repeats over different prediction-point samples; tuning inner folds
  kNNDM too (5-fold), MBO 30, futures on (4.2).
- Secondary figure: the fold-count / design curve (7.34) so the reader sees
  what the choice of evaluation does to the number.
- Area of applicability (Meyer & Pebesma 2021) computed alongside, and the
  conformal set-size layer reported over it.
- Measured on our points (finding 7.38): satellites, domain = study area ->
  every kNNDM fold sits inside one site, W = 4,890 m at k = 10 and 3,719 m
  at k = 5 (use k = 5); drone bokspits_1, domain = its AOI -> balanced
  near-random folds, W = 1.8 m. The two questions, two designs.

### 3.5 Learners and training sources **[HUGH]**

- Learners: glmnet, lightgbm, ranger, svm (linear/radial), untuned ranger
  baseline. Drop xgboost (lightgbm covers it, 7.37 shows no gap) and decide
  on the stacked ensemble: it never won by a clear margin anywhere and costs
  45 minutes a task. Keep it only if the paper wants to keep saying
  "ensemble".
- Training sources per satellite: **field points** (0.8 m buffers) and
  **drone-purity from OUR raw surfaces** - both reported; Glen's archived
  extraction becomes a reproduction-only check. The cross-scale claim is
  then the honest comparison of the two.
- The `dr_smooth` arms retire with the filter.

### 3.5b Why a plain probability average and not the stacked ensemble (D3, D16)

Measured on the 38 tasks of the Phase A run: the stacked ensemble ranked
first in 3, sat on average **2.8 points below the best single learner**
(range -10.3 to +0.2), had the larger fold-to-fold spread (sd 0.122 vs
0.109) and cost 28% of all fit time (3.9 of 14 CPU-hours). The reasons are
structural, and they are the methods-text argument:

1. **Stacking needs base learners that err differently.** On polygon-mean
   features with near-linear class boundaries (7.27, 7.28) the penalised
   multinomial, the SVM and the trees converge on almost the same decision
   surface; there is little complementary signal for a meta-learner to find.
2. **The meta-learner is one more model fitted on little data.** Its weights
   are estimated from a few hundred out-of-fold rows per task, inside each
   of 100 outer iterations; at n = 82-2,400 that adds variance rather than
   removing it, which is what the spread shows.
3. **Stacked weights are chosen by the same noisy criterion that 7.39
   showed cannot rank our learners.** Winner margins are <= 0.012 in every
   flipped case; a weight vector optimised on that signal inherits its
   instability, and the instability lands in the sparse-class areas.
4. **An equal-weight average has no free parameters.** It cannot overfit the
   selection, it needs no inner CV, it is reproducible from the per-learner
   probability rasters, and averaging probabilities is variance reduction
   exactly where we need it - the ~1% class whose mapped area moved by
   +54% / -26% under retuning of a single model.
5. **It composes with the uncertainty design.** Conformal calibration and
   PPI act on the averaged probabilities as they would on any model's; the
   per-learner surfaces stay available as a sensitivity table rather than
   as competing "best" maps.

Engineering note: the average is computed in ONE pass over each cube (read a
block once, predict with every tuned learner, average, write), so the cost
is one raster read plus five model predictions per block, not five full
prediction runs.

### 3.6 The sensor comparison

Report per sensor: spatial-CV accuracy with intervals, pixel-level
precision/recall against the drone maps (the grain story that actually
holds), conformal set-size distributions, and Neltuma area as PPI intervals.
Fig 7E becomes precision/recall + coverage, not "overall accuracy".

### 3.7 Phases with uncertainty

Per hexagon: P(cover > threshold) from the pixel probabilities (or the
conformal bounds), giving a phase map with a confidence class ("Expansion,
>90% sure" vs "Expansion or Incursion") and area ranges per phase. That is
Table 1 with the intervals R1 asked for.

### 3.8 Sub-pixel fractional cover ("unmixing") - a separate product, not a by-product **[HUGH]** **[ANDY]**

Class probabilities and fractional cover are different quantities. The
classifier's p(Neltuma) is the calibrated probability that a pixel's
*dominant* cover is Neltuma, learned from pixels that were >= 95% / 85% /
65% pure. It says nothing about a 10 m pixel that is 20% Neltuma - and Fig 5
shows that is almost every pixel with any Neltuma at all: only 1.1% / 0.9%
/ 0.3% of pixels reach the purity threshold, so the classification design
discards >99% of the cross-scale information at every sensor and then
struggles at exactly the sparse end the paper is about (7.37: Neltuma
recall 0.65 -> 0.39 -> 0.57).

The purity extraction already computes the answer for every satellite pixel
over the drone sites: `frac_1` = Neltuma fraction from the raw drone
surface, for 690k WV2, 179k Planet and 17k S2 pixels. That is a regression
training set, and the same drone maps that were "cross-scale calibration"
for a handful of pure pixels become labels for all of them.

**Design**

| Element | Choice | Note |
|---|---|---|
| Response | Neltuma fraction per satellite pixel (0-1); woody-vs-other fraction as a second target if the paper wants it | Compositional all-class unmixing (Dirichlet / multi-output) is a later option; Neltuma is the claim |
| Labels | raw drone surfaces only (7.31); label noise from drone error (~10%) carried into PPI | the filtered surfaces would erase the sparse end of the target |
| Learners | regression twins of the classification set: glmnet, lightgbm, ranger, svm | same 9-band cubes; same kNNDM folds |
| Zero inflation | evaluate direct regression against a hurdle (presence classifier x cover regression); trees may not need it, glmnet will | 86% of WV2 pixels are zero |
| Evaluation | kNNDM folds; RMSE/MAE and calibration of cover, plus detection metrics at cover thresholds (>0, >0.1, >0.5) | the sparse-detection question is a threshold on cover |
| Uncertainty | **CV+ / jackknife+ intervals via mlr3pipelines `learner_pi_cvplus`** - the regression conformal tool already in our stack and the one Hugh has used | coverage checked per site as for the sets |
| Baseline | linear spectral unmixing with endmembers from pure pixels | the classical method reviewers will expect to see beaten |
| Products | Neltuma fractional-cover raster + lower/upper interval rasters per sensor; area = sum of fractions x pixel area with PPI intervals; phases per hexagon from mean cover directly | no majority vote, no purity threshold anywhere in the chain |

**What it changes in the paper (D11, decided).** The paper is about
Neltuma: the satellite arms' primary product becomes Neltuma fractional
cover with intervals. The existing multi-class land-cover classification
moves mostly to the SI (it still supplies the conformal sets and the drone
labels); no fractional cover is produced for the other classes here. Table 1 / Fig 8 derive from cover, not from thresholded
classes, so the 40-point Pre-/Initial-Incursion flip (7.35) disappears by
construction. The sensor-grain story becomes "how well can each grain
resolve cover" - a cleaner claim than accuracy of a dominant class.

**Phase.** C2, in parallel with the conformal-set work: the extraction is
done, the folds are shared, the learners are the regression twins, and the
interval machinery is off the shelf.

**Decisions.** D11 Neltuma-only fraction vs compositional all-class
unmixing (recommend Neltuma-only first) [ANDY]; D12 hurdle vs direct
regression, decided by measurement [HUGH]; D13 include the endmember
baseline (recommend yes, it costs a day) [HUGH].

### 3.9 Epistemic x aleatoric: conformal prediction conditioned on the area of applicability **[HUGH]** **[ANDY]** - the novel piece

Proposed by Hugh, 2026-09-16. Two uncertainty sources, two tools that have
not been combined:

- **Epistemic** - how far a pixel is from anything the model has seen, in
  predictor space: the dissimilarity index DI and the area of applicability
  (Meyer & Pebesma 2021; CAST::aoa, with local point density LPD since
  2025). The AOA threshold is the outlier-removed maximum DI seen *during
  cross-validation* - so with kNNDM folds the threshold is itself tied to the
  prediction situation. Weakness: no validity guarantee.
- **Aleatoric (plus model error)** - conformal sets / CV+ intervals with a
  coverage guarantee. Weakness: the guarantee assumes exchangeability with
  the calibration data, which extrapolation violates.

The complement is exact: kNNDM makes the calibration folds match the
prediction situation in *geographic* space; DI measures the prediction
situation in *feature* space. Quick literature check (2026-09-16): conformal
prediction in Earth observation exists (Valle et al. 2024 Sci Rep; LULC
conformal, RSE 2023; GeoConformal 2025 with geographic weighting) and
weighted conformal under covariate shift exists (Tibshirani et al. 2019;
CPS under covariate shift 2024), but nothing conditions conformal
calibration on DI/AOA, and nothing does so with prediction-domain-matched
folds. A proper literature review is the first task of the phase.

**Design, in increasing ambition**

| Level | Method | What it buys |
|---|---|---|
| 1 | **DI-stratified Mondrian conformal**: calibration groups = class x DI-bin (bins from the CV DI quantiles); per-group thresholds | Coverage guaranteed *within each DI stratum* (DI is a function of the features, so grouping on it is legitimate). Set size / interval width grow with DI automatically. Outside the AOA there are no calibration points, so no set is issued: the AOA becomes the *domain of validity* of the conformal guarantee, which is the sentence that has not been written. |
| 2 | **DI-normalised scores**: s = (1 - p_y) / g(DI) with g from CAST's DI-to-error calibration (`DItoErrormetric`) | One global guarantee with continuously adaptive sets; compare against level 1 on efficiency (mean set size at equal coverage). |
| 3 | **Weighted conformal beyond the AOA**: likelihood-ratio weights from feature-space density (kNN / LPD) to extend partial guarantees outside the AOA | The extrapolation zone gets a stated, weaker guarantee instead of nothing. Research-grade; optional. |

Applies to both arms: classification (sets, cross-conformal) and
fractional cover (CV+ intervals via `learner_pi_cvplus`, stratified by DI
by fitting per stratum or by post-hoc Mondrian quantiles on its residuals).

**Products**
- DI and LPD rasters per sensor (epistemic); set-size / interval-width
  rasters (aleatoric); a **2x2 typology map** (inside/outside AOA x
  confident/ambiguous) for management;
- **coverage-vs-DI curve** and inside/outside-AOA coverage per class - the
  empirical core: does the guarantee hold up to the AOA edge and fail
  beyond it, as the theory predicts?
- efficiency curves (set size vs DI) for levels 1 vs 2.

**Where the jackknife/CV+ sits**: fractional cover (regression). Sets for
classification are cross-conformal. Both take the same DI stratification.

**Publication shape [ANDY]**: Hugh has discussed the concept with Jakub
Nowosad (co-author of kNNDM and the prediction-domain adaptive evaluation
paper), who was interested - a natural collaborator and the right reviewer
of the kNNDM/AOA half. This is a short methods paper in its own
right ("prediction-domain adaptive calibration: conformal guarantees
within the area of applicability"), with the Neltuma maps as the case
study - separate from the Neltuma paper, which uses the products.

**Phase C3** (2 wk, after C/C2 have their calibration data): literature
review; DI/AOA targets with kNNDM folds; level 1; coverage-vs-DI figure;
level 2 comparison; level 3 if time. Decision **D14**: proceed with levels
1-2 in this refactor, level 3 as a stretch [HUGH]; **D15** methods-paper
split [ANDY].

## 4. Workflow improvements (the engineering half)

### 4.1 Graph shape
- One `tar_map` over `sensor x site x source x learner` with the drone as a
  sensor; shared training-table, cube, fit, predict and compare code.
  Removes the three near-duplicate WV2 / sat / drone blocks in `_targets.R`.
- `pred_*` depend on the **winner's** configuration, not the list of all
  tuned configs (a one-learner change re-predicted seven sites, 7.37).
- Split `resampling.yml` into `evaluation.yml` (folds, budget, learners) and
  `prediction.yml` so an evaluation edit cannot invalidate predictions.

### 4.2 Compute
- `tune_config()` gets a future plan (`multisession`, never `multicore`
  after lightgbm - 7.35); measured 4.4x at 8 workers.
- Predictions tiled with `terra` windows and `cores`; probability rasters
  written as **INT16 scaled** COGs (16 GB -> ~4 GB) and the class raster
  derived from them, not stored twice.
- Conformal outputs are thresholding passes over the prob rasters: cheap.

### 4.3 Data and formats
- Vectors: `.fgb` everywhere. One conversion pass over the mirrored
  shapefiles with provenance (`tools/convert-vectors.sh`), manifest updated,
  and the CRS-less layers (WV2_clip, All_points_buffered_additional) fixed
  at conversion. (Decision 5.4, deferred from v2.0.)
- Tables: CSV/JSON in `inst/`, `qs` in the store, Parquet for anything
  shared out.
- Inputs manifest becomes the only place a path is spelled.

### 4.4 Tests and checks
- `testthat` for the pure functions: `balance_classes`, `vi_formulas`
  against the shipped rasters, phase thresholds, conformal coverage on
  synthetic data, PPI on a known population.
- A `checks` target family asserting invariants after every run: class
  balance, CRS, band counts, coverage >= 1 - alpha on holdout, and the
  handful of headline statistics the paper quotes (original plan item 10).
- The fast profile stays the gate: nothing full-budget runs before it is
  green.

### 4.5 The paper
- `paper/manuscript.qmd` stops being verbatim: it becomes *the* manuscript,
  with the treatment table R1 asked for, every number an inline value,
  `docx` and HTML outputs, and `paper/supplement.qmd` for the SI.
- Responses to reviewers as a third qmd reading the same values.
- The colour-coded reproduction draft is kept as a record on
  `refactor-v2.0`, not carried forward.

### 4.6 Repository
- README rewritten: what it is, how to run it (fast / full), where outputs
  land, the decision log.
- `docs/decisions.md`: the [HUGH]/[ANDY] decisions from the findings, as
  short ADRs, so the reasoning survives the findings file being retired.
- Environment: uvr stays; export a `renv.lock`-equivalent for reviewers.
  **[HUGH]**

## 5. Sequence

| Phase | Content | Gate |
|---|---|---|
| A. Foundations (1 wk) | 4.1-4.4: unified graph, winner-only deps, tuning futures, INT16 probs, fgb conversion, tests. No science changes. **DONE 2026-09-17: gate passed - scores reproduce to mean diff <= 0.006, 31.5 vs 38.4 CPU-h, fast profile 8 min; and finding 7.39: sparse-class areas are unstable under retuning (+54% / -26% with the same learner).** | full run reproduces v2.0 numbers (data-out/results/v2_baseline/); wall time down |
| B. Evaluation (1-2 wk) - **in progress 2026-09-17: kNNDM designs, learner trim, D16 averaged prediction, D4/D5 sources, D9 lock file done; fast gate green; full run launched** | 3.4: variogram range, spcv_block primary, fold-count and LOSO figures, learner trim | fold/CV figures rendered; per-site holdout table |
| C. Uncertainty (2 wk) | 3.1-3.3, 3.7: conformal calibration from resample predictions, set-size / Neltuma-possible / area-bound rasters, coverage validation, PPI areas, probabilistic phases | coverage >= nominal on holdout per class; area intervals in Table 1 |
| C3. AOA x conformal (2 wk, after C/C2) | 3.9: DI/LPD/AOA on kNNDM folds, DI-stratified conformal, coverage-vs-DI, typology map | coverage holds inside AOA per stratum; the curve figure |
| C2. Fractional cover (1-2 wk, parallel) | 3.8: cover regression on all purity-extraction pixels, CV+ intervals, endmember baseline, cover-derived areas and phases | interval coverage >= nominal per site; cover RMSE per sensor |
| D. Sensors (1 wk) | 3.5-3.6: field vs purity sources, precision/recall + coverage per sensor, Fig 7 redesign | sensor table with intervals |
| E. Paper (1-2 wk) | 4.5: manuscript, SI, responses; docx | Andy's pass |

Phases A and B can run while C is designed; C is the critical path.

## 6. Risks

- **Exchangeability under spatial shift.** Calibrating on spatial folds
  buys coverage for "another fold of this landscape"; a genuinely different
  region can still break it. Per-site coverage is reported, not assumed.
- **Set sizes at coarse grain.** Sentinel-2 at alpha = 0.1 may produce
  mostly 2-3 class sets; that is the honest result and Fig 7 should show it.
- **Class scarcity.** Mondrian thresholds for S.mellifera / Boscia rest on
  few calibration points; report n per class alongside coverage.
- **Scope creep in the paper.** The rewrite should change what the paper
  *claims*, not what it is about.

## 6b. Follow-up data recovery (not blocking)

- **Settlement / road distance layers** (D6 gradient PPI, R1 L278):
  `buffers_settlement` and `buffers_road` are `unknown_lost` in the manifest.
  Locate in Glen's tree or rebuild from the village points and a road vector,
  then run the cover-vs-distance PPI (the code generalises to it). Agreed
  follow-up [HUGH], after the main refactor-3.0 pass.

## 7. Decisions needed

| # | Decision | Recommendation | Owner |
|---|---|---|---|
| D1 | Conformal score and Mondrian calibration | **decided 2026-09-17 [HUGH]: LAC + per-class (Mondrian), APS reported** | done |
| D2 | Primary CV design | **decided: kNNDM via CAST**, domains per question | done |
| D3 | Learner set; keep the stacked ensemble? | **decided 2026-09-17 [HUGH]: drop the stacked ensemble.** Five tuned learners (glmnet, svm, ranger, lightgbm, xgboost) + the untuned ranger as a control. The manuscript's "SVM and ensemble performed best" goes [ANDY]. | done |
| D4 | Training sources reported per satellite | **decided 2026-09-17 [HUGH for ANDY]:** field points + purity from OUR raw drone surfaces; Glen's archived extraction kept as a reproduction check only | done |
| D5 | Retire the modal filter; smoothing as sensitivity only | **decided 2026-09-17 [HUGH for ANDY]: yes** - raw surfaces are the products; the filter is reported once as a sensitivity analysis (7.31, 7.35). **SUPERSEDED 2026-09-19 [HUGH]: dropped entirely** - the C2 cover regression leaves no hard map to smooth; removal explained to reviewers, not reproduced. All smoothed targets (pred_smooth_*, confusion_*_smooth, smooth_areas/smooth_index, area_comparison, raw-vs-smooth phases, fig_wv2_map smoothed panel, paper_values *_smooth) retire with the C2 build. | superseded -> drop |
| D6 | PPI for areas and settlement gradients | **decided [HUGH]; area PPI implemented 2026-09-18** (ppi_neltuma_area, corrects each scene's Neltuma area by the drone-overlap bias with a CI). The settlement/road GRADIENT PPI is BLOCKED: buffers_settlement / buffers_road are unknown_lost in the manifest - needs the distance layers to be located or rebuilt [ANDY/HUGH] | area done; gradient data-blocked |
| D7 | Phase map as probabilistic membership with area ranges | **decided 2026-09-17 [HUGH for ANDY]: yes** | done |
| D8 | Paper outputs: docx + HTML, SI as qmd | **decided 2026-09-17 [HUGH for ANDY]: yes** | done |
| D9 | Environment export for reviewers (renv.lock from uvr) | **decided 2026-09-17 [HUGH]: yes** - export the lock file on its own alongside uvr; uvr stays the working tool | done |
| D10 | Landsat arm in scope? (6.3) | **decided 2026-09-17 [HUGH for ANDY]: out** | done |
| D11 | Fractional cover: Neltuma-only vs compositional | **decided 2026-09-17 [HUGH]: Neltuma only in the paper.** The existing multi-class land-cover classification moves mostly to the SI; no fractional cover for the other classes in this paper | done |
| D12 | Hurdle vs direct cover regression | **decided 2026-09-17 [HUGH]: measure both**, pick by kNNDM RMSE + threshold detection | done |
| D13 | Endmember linear-unmixing baseline | **decided 2026-09-17 [HUGH]: yes** | done |
| D14 | AOA x conformal: levels 1-2 now, 3 as stretch | **decided 2026-09-17 [HUGH]: yes** | done |
| D15 | Split the AOA x conformal method into its own short paper | **decided 2026-09-17 [HUGH]: yes, but AFTER this paper is complete** - the method is used here; the methods paper is not started until the Neltuma paper is done | done |
| D16 | Model selection for the landscape products (7.39) | **decided 2026-09-17 [HUGH]: equal-weight average of class probabilities over ALL tuned learners.** No winner-takes-all, no fallback learner, and no dropping of "laggards" - defining a laggard is a qualitative call we decline to make. Per-learner surfaces reported as a sensitivity table. | done |
