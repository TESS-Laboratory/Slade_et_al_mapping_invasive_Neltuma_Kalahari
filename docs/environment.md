# Environment

Step 2 of [`server-handover.md`](server-handover.md). This is the record of how
the R environment is built on the analysis server, and — more importantly — why
one non-obvious setting is mandatory rather than a preference.

## TL;DR

```bash
source tools/uvr-env.sh   # forces source builds; do this before any uvr command
uvr sync
```

Never run a bare `uvr add` or `uvr sync` on this machine. It will appear to
succeed and leave you with packages that fail to load.

## The problem: P3M binaries cannot work here

`uvr` defaults to pre-built binaries from Posit Package Manager, and it is not
wrong to do so — `uvr doctor` correctly reports:

```
v P3M binary packages          available (Linux noble)
```

The host *is* Ubuntu 24.04 (noble). The trouble is that P3M's noble builds link
the GDAL that ships in noble's own archive, and this machine carries a much
newer GDAL from ubuntugis:

| | GDAL | soname |
|---|---|---|
| P3M "noble" binaries built against | 3.8.x | `libgdal.so.34` |
| Installed on this host | 3.11.4 | `libgdal.so.37` |

Sonames are the ABI contract. `.34` and `.37` are not interchangeable, so the
binary cannot resolve its own dependency:

```
$ ldd .uvr/library/terra/libs/terra.so
    libproj.so.25    => /lib/x86_64-linux-gnu/libproj.so.25       (ok)
    libgdal.so.34    => not found                                 <-- the failure
    libgeos_c.so.1   => /lib/x86_64-linux-gnu/libgeos_c.so.1      (ok)
```

Note that GEOS and PROJ resolve cleanly. Only GDAL is out of step, which is why
the failure presents as a confusing partial breakage rather than an obvious
"nothing works".

This is not a `uvr` bug and there is no version of `uvr` that fixes it. It is a
genuine mismatch between the host's system libraries and the binary repository's
build environment.

## The fix: force source builds

Building from source makes `R CMD INSTALL` run each package's own `configure`
script, which shells out to `gdal-config` (and `geos-config`, `pkg-config proj`)
on `PATH`. The package therefore links against whatever GDAL is *actually*
installed, by construction, and cannot drift.

`uvr` exposes this two ways, which are equivalent:

```bash
uvr add <pkg> --no-binary          # per command
export UVR_NO_BINARY=1             # for the session
```

`tools/uvr-env.sh` sets the environment variable, because per-command flags get
forgotten and the failure is silent until you try to load the package.

### Why it is set globally rather than per package

`uvr` 0.4.6 has no `uvr.toml` key for this — it is flag-or-environment only.
`UVR_NO_BINARY` is all-or-nothing, so packages that do not link GDAL get built
from source too. That costs seconds each, and the alternative is a
hand-maintained list of which packages happen to link GDAL, which would rot the
first time a dependency changed. Blunt and correct beats clever and stale.

Packages with no compiled code at all are unaffected — `uvr` still reports them
as `binary` in its summary (`1 binary - 13 from source` for the `sf` install,
which is `DBI`), because there is nothing to compile either way.

Cost, for calibration: **terra 1m20s** wall (6m11s CPU at `-j16`), and **9m6s**
for `sf` + `exactextractr` and their 12 dependencies. `MAKEFLAGS` propagates
through `R CMD INSTALL`, so parallel builds work.

### The trap that wastes an afternoon

This command looks right and does nothing:

```bash
UVR_NO_BINARY=1 uvr sync --ignore-cache
> Everything is up to date
```

`--ignore-cache` skips uvr's *download* cache. It has no bearing on the
"already present in `.uvr/library/`" check, so `sync` no-ops straight over a
broken binary. To replace one, evict it first:

```bash
rm -rf .uvr/library/<pkg>
source tools/uvr-env.sh
uvr sync --ignore-cache
```

## Verifying

The check that matters is not `library(terra)` — it is what the linker resolved
and what GDAL reports at runtime:

```bash
ldd .uvr/library/terra/libs/terra.so | grep gdal    # expect libgdal.so.37

# `uvr run` takes a script path, not an -e expression; `-` reads stdin
uvr run - <<'EOF'
library(terra)
cat("GDAL:", gdal(), " GEOS:", gdal(lib = "geos"), " PROJ:", gdal(lib = "proj"), "\n")
EOF
```

Expected: GDAL 3.11.4, GEOS 3.12.2, PROJ 9.4.1.

Then read something real, which also re-checks the `inst/config/` values:

```r
r <- terra::rast("data-in/drone/bokspits_1/refl_stack_chm.tif")
# 6 bands, EPSG:32734, 0.05679 m, band 6 range -0.524 to 14.178 m
```

## System libraries

Present and sufficient (no admin needed):

| Library | Version | Header |
|---|---|---|
| GDAL | 3.11.4 | `/usr/include/gdal/gdal.h` |
| GEOS | 3.12.2 | `/usr/include/geos_c.h` |
| PROJ | 9.4.1 | `/usr/include/proj.h` |
| R | 4.6.0 | `/usr/bin/R` (system, unpinned) |

Also verified present, since source builds need the headers rather than just the
runtimes: `udunits2`, `sqlite3`, `zstd`, `lz4`, `openssl`, `libxml2`, `netcdf`,
`fontconfig`, `freetype2`, `png`, `jpeg`.

### Missing, needs root

Not required by the spatial stack, but both will block packages we are likely to
reach for later:

```bash
sudo apt-get install libcurl4-openssl-dev libtiff-dev
```

`libcurl` is needed by `curl` → `httr`/`gh` and anything fetching over the
network; `libtiff` by several imaging packages. Worth doing in one go rather
than discovering them one failed build at a time.

### One warning that can be ignored

Installing `sf` raises:

```
! WARN  Missing system dependencies for 1 package(s)
  s2 needs: libabsl-dev
```

This comes from Posit's sysreqs index, which assumes a system Abseil. `s2`
vendors its own copy, so it compiled and loads correctly — `sf_use_s2()` returns
`TRUE`. **Do not install `libabsl-dev` to silence it**; a system Abseil that
disagrees with the vendored headers is a new version of the same ABI problem
this whole document is about.

## Implications for Docker

When the project is containerised, the base image must either

1. carry a GDAL whose soname matches whatever binaries it installs, or
2. adopt the same policy as here and build the spatial stack from source.

Option 2 is the safer default and is what `tools/uvr-env.sh` already encodes.
Pinning a `rocker/geospatial` tag would satisfy option 1, but only for as long
as that tag's GDAL and P3M's stay in step — which is exactly the assumption that
broke here.

## Notes

- **R is not pinned.** `uvr doctor` warns that there is no `.r-version`, so the
  project is bound to system R 4.6.0. Pinning (`uvr r install` / `uvr r pin`)
  would make the environment reproducible across machines, but it triggers a
  full source rebuild against a uvr-managed R. Deferred, not forgotten.
- **Source-built `.so` files are large.** terra is 103 MB source-built versus
  6.7 MB for the binary, because R's default `CXXFLAGS` carry `-g`. Harmless;
  `.uvr/library/` is gitignored.
- **`exactextractr` pulls in `raster` and `sp`.** Both are superseded and neither
  should appear in new pipeline code — they are transitive dependencies only.
