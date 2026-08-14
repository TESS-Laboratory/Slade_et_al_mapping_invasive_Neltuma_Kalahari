# Data collation

How to find the analysis inputs on the machine where the original work was run.

> Detail document. The entry point is
> [`docs/server-handover.md`](server-handover.md), which sequences this alongside
> the environment setup and pipeline work.

**Goal.** Establish, for each of the 66 manifest entries, whether the data still
exists, where, and how much of it. The output is a report you bring back here; it
drives what the `targets` pipeline can reproduce and what has to be rebuilt.

**Time.** 10 minutes of commands, plus however long the filesystem scan takes.

---

## Why this is needed

Every path in the original codebase points at a Windows external drive
(`E:/Glenn/Botswana/`) that we do not have. Five different project roots are
assumed across the scripts:

```
E:/Glenn/Botswana/                              all data
E:/Glenn/Botswana/R_Scripts/Glenn-Prosopis-ML   pipeline in/out
E:/Glenn/Botswana/R_Scripts/slade-prosopis      analysis in/out
E:/Glenn/Botswana/R_Scripts/slade-Neltuma       Eco_Analysis outputs only
C:/Workspace/R_Scripts/slade-prosopis           setwd() in 4 scripts
C:/Workspace/R_Scripts/Kgalagadi                Reflectance_Boxplots.R
```

Some inputs are on Zenodo (`10.5281/zenodo.18506271`, 33.7 GB of drone data).
Most are not: the satellite scenes, every vector layer, the co-registered
mosaics, the hexagonal grids, and all the intermediate result workbooks that the
reported accuracy figures were read from.

---

## Steps

### 1. Clone the branch

```sh
git clone -b refactor-v2.0 \
  https://github.com/TESS-Laboratory/Slade_et_al_mapping_invasive_Neltuma_Kalahari.git
cd Slade_et_al_mapping_invasive_Neltuma_Kalahari
```

### 2. Get the lie of the land

Before scanning everything, find the plausible roots.

```sh
# where are the big filesystems?
df -h

# any obvious project or data directories?
ls -la /mnt /media /srv /data 2>/dev/null
ls -la ~ 

# is there an old Windows drive image or a copy of the E: tree?
find / -maxdepth 4 -type d \
  \( -iname 'Botswana' -o -iname 'Glenn*' -o -iname 'Pix4d' \
     -o -iname 'ReflStacks' -o -iname 'Satellite_Data' -o -iname 'GIS_aggregate' \) \
  2>/dev/null
```

That last command is the highest-value one. `ReflStacks`, `Satellite_Data` and
`GIS_aggregate` are distinctive directory names; if the tree survived anywhere,
they will show up.

### 3. Run the discovery scan

Dependencies: `bash`, `find`, `awk`. Nothing else. No R, no network.

```sh
# targeted, if step 2 found the tree
tools/discover-inputs.sh -o discovery /mnt/data/Botswana /home/glenn

# or broad, if it did not. Excludes keep the scan sane.
tools/discover-inputs.sh -o discovery \
  -x '*/.git/*' -x '/proc/*' -x '/sys/*' -x '*/node_modules/*' \
  / 
```

A whole-root scan on a large server can take a long time. If you want to scan
once and re-match later without re-walking the filesystem:

```sh
find / -type f -print 2>/dev/null > /tmp/file-index.txt
tools/discover-inputs.sh --reuse-index /tmp/file-index.txt -o discovery
```

### 4. Bring back the results

```sh
tar czf discovery-$(hostname).tar.gz discovery/
```

Four files are produced in `discovery/`:

| File | What it is |
|---|---|
| `discovery-report.md` | Human-readable. Missing / partial / found, with example paths |
| `discovery-report.tsv` | Machine-readable, one row per manifest entry |
| `found-paths.tsv` | Manifest id to every matching path. The important one |
| `file-index.txt` | The raw index. Large; drop it if the archive is unwieldy |

`file-index.txt` can be big. Everything else is small.

---

## What matters most

Matching is on **filename only**, not directory, so a file that was moved or
reorganised still matches. That also means false positives are possible; check
example paths before concluding something was found.

Priority order, highest first. These are the entries that block the most.

| Priority | Manifest id | Why |
|---|---|---|
| 1 | `bench_workbooks`, `confusion_workbooks` | **Every reported accuracy figure was read from these 64 workbooks.** Without them no headline number can be verified against its source, only recomputed from scratch |
| 2 | `wv2_corrected_16m` | The co-registration reference for the whole satellite arm. No script produces it |
| 3 | `drone_chm` | Required by every Drone+CHM stack. No script produces it. Rebuildable from DSM and DTM if those survive |
| 4 | `wv2_train_points_combined` | The field-only baseline behind the headline "6.1% improvement, 69.7% to 75.8%" |
| 5 | `train_wv2`, `train_planet`, `train_s2` | Determines which rung of the training ladder each satellite run used, resolving open question 6.4 |
| 6 | `drone_aoi_clip`, `wv2_aoi` | Small vector files, but every mask, grid and area calculation depends on them. Also settles the 445 vs 450 km² question |
| 7 | `hex_grids` | Fig 8 and Table 1. Confirms the 250 m hexagon geometry |
| 8 | `wv2_classification_rf` vs `wv2_classification_mlr3` | **Open question 6.1.** If Table 1 came from the RF product while Fig 6C reports the mlr3 one, the paper mixes two classifications |
| 9 | `camel_thorn_counts` (`temp.xlsx`) | The hand-edited file behind the 63% co-occurrence claim |
| 10 | `wv2_raw_order3` | A third WorldView-2 product ID appears only as band names. Either a further delivery or a stale reference |

Anything marked `zenodo` in the manifest can be re-downloaded, so treat those as
low priority even if missing.

---

## Also worth looking for while you are there

Things not in the manifest because we do not know what they are called.

- **Any `.Rhistory`, `.RData` or `.Rproj.user` under the project roots.** Command
  history would settle several open questions outright, in particular which
  training set each satellite run consumed.
- **Anything under a `Manuscript figures` directory.** That name is used in
  `slade-prosopis` and is where the final figure files were written.
- **Pix4D project directories** (`*_MS`, `*_MS_RGB`). If the full projects
  survive, the DSM and DTM are there and the CHM is rebuildable.
- **Any QGIS project files** (`.qgz`, `.qgs`). Invasion phase, Table 1 and Fig 8
  were produced outside R, most likely in QGIS. A project file would recover the
  thresholds and the geometry.
- **Sentinel-2 SAFE directories** or anything matching `*T34JDR*`.
- **Any file whose name contains `hex_`**, beyond the three in the manifest.

For the QGIS point specifically:

```sh
find / -type f \( -name '*.qgz' -o -name '*.qgs' \) 2>/dev/null
find / -type f -name '.Rhistory' 2>/dev/null
```

---

## After the scan

Send back `discovery-$(hostname).tar.gz`. From `found-paths.tsv` we will:

1. Fill the `resolved_path` column of the manifest and set `DATA_ROOT`.
2. Mark entries that are genuinely lost, so the pipeline can declare them
   explicitly instead of failing obscurely.
3. Decide, per lost entry, whether to rebuild it, substitute it, or drop the
   analysis that depends on it.

Manifest fields, for reference (`inst/manifest/data_manifest.csv`):

| Field | Meaning |
|---|---|
| `id` | Stable key, used by the pipeline |
| `type` | `input` or `derived`. See below |
| `group` | `drone_raw`, `drone_derived`, `satellite_raw`, `satellite_derived`, `vector`, `ground`, `classification`, `training_set`, `results`, `context` |
| `sensor` | `drone`, `wv2`, `planet`, `s2`, `landsat`, `multi`, `na` |
| `kind` | `raster`, `vector`, `table` |
| `filename_glob` | Basename pattern. `{a,b}` alternation supported |
| `expected_count` | How many files should exist. `0` means unknown |
| `original_location_hint` | Where it lived on the E: drive |
| `availability` | `in_repo`, `zenodo`, `external`, `not_located`, `unknown`, `unknown_lost` |
| `produced_by` | For derived entries, what makes them |
| `required_for` | What breaks without it |
| `notes` | Caveats, inconsistencies, open questions |

## input vs derived

**41 of the 66 entries are `derived`**: the pipeline produces them, or could, from
other manifest entries. Only **25 are true `input`s** that must be obtained
externally.

This matters for the scan. A missing `input` has to be found or re-acquired. A
missing `derived` file is usually fine, because we can rebuild it once its own
inputs are located, and rebuilding is preferable anyway since it puts the product
under the pipeline's control. Finding derived files is still valuable: they let us
check the rebuild against what the paper actually used.

**Six entries are marked `derived` but their `produced_by` reads
`NO PRODUCER FOUND`. Treat those as inputs.** Nothing in the codebase can
regenerate them:

| id | needed for |
|---|---|
| `drone_chm` | every Drone+CHM predictor stack |
| `wv2_corrected_16m` | co-registration reference for the whole satellite arm |
| `wv2_train_points_combined` | the field-only baseline behind the 6.1% claim |
| `planet_grid_95` | a drone-vs-Planet comparison script |
| `cover_all` | Fig 5 combined panel |
| `cover_resolution_simple` | Fig 5 density and line panels |

`drone_chm` is the one exception with a plausible rebuild path: DSM minus DTM, if
`drone_pix4d_dsm` and `drone_pix4d_dtm` are found. That is why both Pix4D
elevation products are on the list even though no current script reads the DSM.
