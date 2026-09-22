# Changes from the submitted manuscript (for the co-authors)

*Branch `refactor-3.0`, 2026-09-22. This file replaces the colour-coded
"PIPELINE CONTRADICTS" boxes of the reproduction draft. Every claim of the
submitted manuscript that the refactored analysis changed is listed here with
the evidence behind it (finding numbers refer to `refactor-findings.md`).
Decisions still open for the authors are marked **[DECISION]**.*

## A. Claims that changed

| # | Submitted claim | Now | Why |
|---|---|---|---|
| A1 | Abstract, §3.1: drone ~90% overall, ~87% Neltuma; SVM and ensemble best (91.3 / 90.3%) | Per-site winners average ~91% on the best stack; learner means 86.5–89.9%; no learner wins any task by more than its own fold-to-fold SD; the equal-weight probability average (89.6%) is what is mapped | Findings 7.28, 7.39, 7.41, D3, D16. Learner ranking is inside the noise; a nominal winner's map of a 1% class is one draw from a wide distribution |
| A2 | §2.5–2.6: "spatial block cross-validation", "200 iterations", Bayesian optimisation | Prediction-domain-matched kNNDM CV (Linnenbrink et al. 2024): 10 folds × 10 repeats within site for the drone, 5 × 10 whole-site holdouts for the satellites, with W reported per task; Bayesian optimisation (mlr3mbo, 50 evaluations in the full profile) is now true | Findings 1.1, 1.2, 7.25, 7.38, 7.40, D2 |
| A3 | Abstract, §3.2, Fig 6: WV2 75.8%; drone calibration +6.1% (69.7 → 75.8%) | Under whole-site holdouts the best WV2 learner reaches ~62% on drone-derived labels and ~65% on field points alone; 75.8% reproduces only under random CV; the same task moves from ~49% to ~71% as the number of coordinate-clustered folds rises from 5 to 40 | Findings 7.34, 7.37, 7.41. The +6.1% reverses; the accuracy is a function of fold geometry |
| A4 | §3.2: WV2 over-predicts Neltuma extent by 24.8%; 34.6% of drone Neltuma plants liable to be classed as other woody | Against the drone maps the averaged WV2 classification recovers ~61% of Neltuma pixels at ~56% precision and matches drone extent to within ~10%, with ~32% of drone-Neltuma pixels labelled woody; Planet/S2 over-predict extent 2.2× / 5.7× at 28% / 15% precision | Finding 7.35 (the 24.8% arose from a modal filter applied to both surfaces; the filter is retired), current confusion tables. The 34.6% is not recoverable from the S10 matrix as reported |
| A5 | §3.3, Fig 7E: overall accuracy 71.1 → 70.3%, Neltuma-specific 50.4% at S2 | Fig 6E now shows pixel-level Neltuma recall/precision against the drone maps and conformal set size per sensor; cross-validated accuracies (all arms, all learners) are Table S11 | Plan 3.6; class rosters differ by sensor so overall accuracy is not comparable |
| A6 | §2.7, §3.4, Fig 8, Table 1: hard-class prevalence map; 16 km² (3.6%) dominance, 356 km² (79.9%) expanding | Continuous sub-pixel cover with conformal intervals, an area of applicability and a bias-corrected area (WV2 ~1,110 ha, 95% CI ~320–2,170 ha); phases from cover with an interval range (Table 3); the hard-class map is Figure S14 | D11, C2/C3 design (findings 2026-09-19 to 09-22). The hard-class phase split flipped by 40 points with the surface choice (7.35) and its areas varied by tens of percent between learners |
| A7 | §2.2: "minimum of 20 observations per class" | Target of 20; 8 of 41 site–class combinations fall below it (min n = 2) | Findings 1.7, 7.21; Table S4 regenerated |
| A8 | §3.1: 84.7% plant-scale detection (n = 184) | Retained as the submitted number, flagged as not regenerated | The two-plot survey is not among the archived inputs; the archived point layer is the training polygons (a resubstitution check, Table S9 note) |
| A9 | §2.6 "sieve filter"; post-classification modal filter | No smoothing anywhere | Findings 1.6, 7.31, 7.35; D5 superseded 2026-09-19 |
| A10 | Fig 7 caption "PlanetScope (4 m)" | 3 m throughout | Finding 2.10 |
| A11 | Table S7 class sizes 280/200/100 | 400/400/60 (thresholds 95/85/65% unchanged) | Findings 7.18, 7.19 |
| A12 | §3.4: density increases towards settlements and roads (Figs S10–S11, lost layers) | Regenerated from OSM roads/villages on the cover hexagons (Figures S10–S11), descriptive only | R1 L278; the authors' response (exploratory, no inference) is honoured |

## B. New content

- §1: a paragraph on evaluation geometry, per-pixel uncertainty, conformal prediction, area of applicability and prediction-powered inference; research questions 2–3 reworded.
- §2.5: learner set, tuning, and the equal-weight probability average; Table 1 (predictor stacks = R1's treatment table).
- §2.6: kNNDM with W; conformal prediction sets; Table S12 (fold designs, R1's "show the blocking").
- §2.7: the sub-pixel cover method in four steps.
- §3.4: cover areas per sensor, honest per-site coverage, the dense-site under-prediction, grain → applicability; Fig 7 (12 panels), Fig 8, Table 2.
- §3.5: phases with interval ranges (Table 3) and the settlement/road gradient.
- §4.2 rewritten around what drone data contribute (cover labels, not purer class labels) and the honest limitations; §4.5 transferability and recommendations (R1); management cost of false positives/negatives (R2) in §4.2 and §4.4; abstract and conclusion carry the discrepancy metrics (R2).
- Supplement as `paper/supplement.qmd`: Tables S4, S9–S14; Figures S10–S16.

## C. Decisions for the authors **[DECISION]**

1. **Area point estimate.** The bias-corrected (prediction-powered) area is reported as the headline with the uncorrected within-AOA sum beside it. The dense-stratum correction is measured with the only dense site held out, so its sign rests on Struizendam 4 (about a third of the bootstrap falls below the uncorrected sum). Alternative: report the uncorrected sum as the point and the corrected value as the sensitivity.
2. **Phase thresholds.** Table S8's thresholds were defined for the share of Neltuma-classified pixels; applied to mean sub-pixel cover they never produce pre-incursion (a regression never predicts zero) and the 1.5% boundary sits at the model's detection floor (~2% cover). Table 3 reports the interval range and the below-floor share; the text says the incursion/expansion split is not resolved by satellite. Alternative: redefine the bands for cover (e.g. below floor / sparse / moderate / dense).
3. **Calibrator prior.** Platt scaling is fit on the field sample (20% Neltuma) and applied to pixels (~3%); a prior-shift correction could lower every cover target. Not implemented; flagged.
4. **Plant-scale validation.** Restore the two-plot survey layer to the inputs so 84.7% is regenerated, or keep it as an authored number.
5. **Figure 2 and Figure 3** are authoring items (workflow diagram; orthomosaic details and photographs); proposed content is in the qmd.
6. **Reference list**: add Angelopoulos et al. 2023, Meyer & Pebesma 2021, Meyer et al. 2024 (CAST), Linnenbrink et al. 2024, Frazier & Wang 2011 (drafted in the qmd); drop citations no longer used.
