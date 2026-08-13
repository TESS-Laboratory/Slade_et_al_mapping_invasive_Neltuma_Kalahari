# legacy_imported

Reference code recovered from repositories that were never archived alongside
the paper. **Nothing here runs as-is.** It is kept so the `targets` rewrite has a
source of truth to port from, and so the provenance of every manuscript output is
traceable.

Every file carries a header recording its source repository, path, ref, commit
and sha256, plus why it was imported. Content below each header is verbatim,
including pre-existing bugs.

Merges into `legacy/` alongside the archived scripts at plan phase 2.

## Why these files are here

The published archive (Zenodo `10.5281/zenodo.16681147`) is incomplete. 94
script basenames in `TESS-Laboratory/slade-prosopis` are absent from it,
including the canonical Figure 4 script, which is **newer** than the archived
copy. See [`../refactor-findings.md`](../refactor-findings.md) and
[`../audit/source-recovery-map.md`](../audit/source-recovery-map.md).

## Contents

| Directory | What it holds | Manuscript output |
|---|---|---|
| `figures/` | Fig 4 script (v4, supersedes the archived v2), its violin variant, the satellite benchmark plots, the per-survey cover extract, and the canonical `theme_fancy()` | Figs 4, 6A |
| `landscape/` | Hexagonal grid construction and extraction; settlement and road buffer profiles | Figs 8, S10, S11; Table 1 |
| `validation/` | Plant-scale height analysis; per-band reflectance boxplots | Table S9, Fig S9 |
| `spatial_cv/` | The only file in any repository using `spcv_block`, with `show_blocks = TRUE` | Answers R1 L239-242 |
| `preprocessing/` | Planet Struizendam merge; the modal focal filters | §2.6 |
| `landsat/` | Landsat arm, not reported in the manuscript | Optional Fig 5 fourth panel |
| `variogram/` | Empirical variogram, to justify a block `range` if block CV is adopted | Supporting §5.1 |

## Known non-running state

- Windows absolute paths under `E:/Glenn/Botswana/`, plus four other project
  roots (`C:/Workspace/R_Scripts/slade-prosopis`,
  `C:/Workspace/R_Scripts/Kgalagadi`, and two `R_Scripts` variants).
- Archived packages: `rgeos`, `rgdal`. Java-dependent `xlsx`. GitHub-only
  `ggbiplot`, `bbplot`.
- `windowsFonts()` / `windowsFont()` calls fail on Linux.
- Two files do not parse, both pre-existing faults preserved on import:
  - `preprocessing/Majority_filter.R` line 58, missing comma in a `focal()` call.
  - `figures/theme_fancy.R` is an Rmd chunk saved with a `.R` extension. The
    copies embedded in the analysis scripts are the working ones.

## Not imported

Seven near-identical `Hex_Grid_Extract_WV2_roll_time_series*` variants, the
Rhododendron scripts, spectroscopy resampling, questionnaire analysis, NDVI trend
analysis and tree detection. All out of scope for this paper. Recoverable from
`TESS-Laboratory/slade-prosopis@671e56f` if needed.
