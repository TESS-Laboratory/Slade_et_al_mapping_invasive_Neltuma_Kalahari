#!/usr/bin/env bash
#
# Separate the canopy height model from the spectral bands in the mirrored drone
# data, so every drone product is one independent raster.
#
#   data-in/drone/<site>/refl_stack_chm.tif   6 bands: 5 spectral + CHM
#                        refl_stack.tif       5 bands: spectral only  (already present)
#                        chm.tif              1 band:  CHM            (written here)
#
# WHY
#
# The CHM is a raw input, not something this project derives: the original
# producer script reads a finished ReflStacks/<site>_CHM.tif and concatenates it
# onto the reflectance stack (refactor-findings.md 5.3). That standalone file did
# not survive (7.16), so the only way back to it is to decompose the stack.
#
# This is lossless. Bands 1-5 of refl_stack_chm.tif are bit-identical to
# refl_stack.tif, so refl_stack.tif + chm.tif carries exactly the same
# information as the 6-band stack, and the two can be recombined by VRT.
#
# BUILDING THE VRT IS NOT THIS SCRIPT'S JOB. Stack assembly is a pipeline target,
# so that predictor combinations are declared in the graph rather than baked into
# the mirror. This script only separates.
#
# THE BAND LABEL
#
# Band 6 is described in the source as "<site>_MS_RGB_dsm", and DRONE_STACK_BANDS
# calls it "dsm", but the values are height above ground, not elevation above
# datum (finding 7.14). The output band is therefore described as "chm", with the
# original string preserved in metadata so nothing is lost. This is deliberate:
# the failure mode being prevented is someone later "correcting" the pipeline to
# treat that band as a DSM.
#
# Idempotent: existing outputs are skipped unless FORCE=1.
# Does not modify or delete any source file.

set -euo pipefail

SITES=(bokspits_1 bokspits_2 bokspits_3
       struizendam_1 struizendam_2 struizendam_3 struizendam_4)

ROOT="data-in/drone"
SRC_NAME="refl_stack_chm.tif"
OUT_NAME="chm.tif"
CHM_BAND=6
FORCE="${FORCE:-0}"
DRY_RUN="${DRY_RUN:-0}"

command -v gdal_translate >/dev/null || { echo "gdal_translate not found" >&2; exit 1; }
[ -d "$ROOT" ] || { echo "no $ROOT - run from the project root" >&2; exit 1; }

# --verify: prove chm.tif reproduces band 6 of the source exactly.
# Exact arithmetic, not a checksum - GDAL's checksum is a narrow 16-bit value
# and collisions are cheap, so it is fine for a first pass but must not be the
# evidence a "lossless" claim rests on. Also compares the NaN masks, since a
# difference raster cannot reveal a nodata cell that moved.
# --verify-spectral: prove bands 1-5 of refl_stack_chm.tif are identical to
# refl_stack.tif, which is what makes refl_stack.tif + chm.tif a complete
# replacement for the 6-band stack. Exact, for the same reason as --verify:
# GDAL band checksums collide readily (three bands of bokspits_2 share 41043),
# so they are a smoke test, not proof. Reads ~64 GB; expect it to be slow.
if [ "${1:-}" = "--verify-spectral" ]; then
  fail=0
  for site in "${SITES[@]}"; do
    chm="$ROOT/$site/$SRC_NAME"; spec="$ROOT/$site/refl_stack.tif"
    if [ ! -f "$chm" ] || [ ! -f "$spec" ]; then
      printf '%-16s SKIP (missing)\n' "$site"; continue
    fi
    python3 - "$spec" "$chm" "$site" <<'PY' || fail=1
import sys, numpy as np
from osgeo import gdal
gdal.UseExceptions()
spec, chm, site = sys.argv[1], sys.argv[2], sys.argv[3]
A, B = gdal.Open(spec), gdal.Open(chm)
if A.RasterCount != 5 or B.RasterCount != 6:
    print(f"{site:<16} FAIL  band counts {A.RasterCount}/{B.RasterCount}"); sys.exit(1)
if (A.RasterXSize, A.RasterYSize) != (B.RasterXSize, B.RasterYSize):
    print(f"{site:<16} FAIL  dimensions differ"); sys.exit(1)
if A.GetGeoTransform() != B.GetGeoTransform():
    print(f"{site:<16} FAIL  geotransforms differ"); sys.exit(1)
worst, nanmis, rows = 0.0, 0, 2048
for i in range(1, 6):
    a, b = A.GetRasterBand(i), B.GetRasterBand(i)
    for y in range(0, A.RasterYSize, rows):
        n = min(rows, A.RasterYSize - y)
        x = a.ReadAsArray(0, y, A.RasterXSize, n).astype("float64")
        z = b.ReadAsArray(0, y, B.RasterXSize, n).astype("float64")
        nx, nz = np.isnan(x), np.isnan(z)
        nanmis += int(np.count_nonzero(nx != nz))
        both = ~nx & ~nz
        if both.any():
            worst = max(worst, float(np.abs(x[both] - z[both]).max()))
ok = (worst == 0.0) and (nanmis == 0)
print(f"{site:<16} {'PASS' if ok else 'FAIL'}  max|diff|={worst:g}  nodata_mismatch={nanmis}")
sys.exit(0 if ok else 1)
PY
  done
  echo
  [ "$fail" = "0" ] && echo "bands 1-5 identical at every site; refl_stack.tif + chm.tif is complete" \
                    || echo "SPECTRAL VERIFICATION FAILED - do not drop the 6-band stacks" >&2
  exit "$fail"
fi

if [ "${1:-}" = "--verify" ]; then
  fail=0
  for site in "${SITES[@]}"; do
    src="$ROOT/$site/$SRC_NAME"; out="$ROOT/$site/$OUT_NAME"
    if [ ! -f "$out" ] || [ ! -f "$src" ]; then
      printf '%-16s SKIP (missing)\n' "$site"; continue
    fi
    python3 - "$src" "$out" "$site" "$CHM_BAND" <<'PY' || fail=1
import sys, numpy as np
from osgeo import gdal
gdal.UseExceptions()
src, out, site, bidx = sys.argv[1], sys.argv[2], sys.argv[3], int(sys.argv[4])
# Keep the Dataset references alive: a band borrowed from a temporary dataset is
# invalidated as soon as that dataset is collected.
dsa, dsb = gdal.Open(src), gdal.Open(out)
a, b = dsa.GetRasterBand(bidx), dsb.GetRasterBand(1)
if (a.XSize, a.YSize) != (b.XSize, b.YSize):
    print(f"{site:<16} FAIL  dimensions differ"); sys.exit(1)
maxdiff, nan_mismatch = 0.0, 0
rows = 2048
for y in range(0, a.YSize, rows):
    n = min(rows, a.YSize - y)
    x = a.ReadAsArray(0, y, a.XSize, n).astype("float64")
    z = b.ReadAsArray(0, y, b.XSize, n).astype("float64")
    nx, nz = np.isnan(x), np.isnan(z)
    nan_mismatch += int(np.count_nonzero(nx != nz))
    both = ~nx & ~nz
    if both.any():
        maxdiff = max(maxdiff, float(np.abs(x[both] - z[both]).max()))
ok = (maxdiff == 0.0) and (nan_mismatch == 0)
print(f"{site:<16} {'PASS' if ok else 'FAIL'}  max|diff|={maxdiff:g}  "
      f"nodata_mismatch={nan_mismatch}  band='{b.GetDescription()}'")
sys.exit(0 if ok else 1)
PY
  done
  echo
  [ "$fail" = "0" ] && echo "all sites reproduce band $CHM_BAND exactly" \
                    || echo "VERIFICATION FAILED" >&2
  exit "$fail"
fi

echo "gdal_translate: $(gdal_translate --version)"
[ "$DRY_RUN" = "1" ] && echo "DRY RUN - nothing will be written"
echo

written=0; skipped=0; missing=0

for site in "${SITES[@]}"; do
  src="$ROOT/$site/$SRC_NAME"
  out="$ROOT/$site/$OUT_NAME"

  if [ ! -f "$src" ]; then
    echo "MISSING  $src"; missing=$((missing + 1)); continue
  fi

  nb=$(gdalinfo "$src" | grep -c '^Band ')
  if [ "$nb" -ne 6 ]; then
    echo "SKIP     $site - expected 6 bands, found $nb"; skipped=$((skipped + 1)); continue
  fi

  if [ -f "$out" ] && [ "$FORCE" != "1" ]; then
    echo "EXISTS   $out (FORCE=1 to rebuild)"; skipped=$((skipped + 1)); continue
  fi

  # The label carried by band 6 in the source, kept for provenance.
  orig=$(gdalinfo "$src" \
         | awk '/^Band 6 /{f=1} f && /Description = /{sub(/.*Description = /,""); print; exit}')

  echo "WRITE    $out   (band $CHM_BAND of $SRC_NAME, was \"$orig\")"
  [ "$DRY_RUN" = "1" ] && continue

  # -b 6            take only the CHM band
  # LZW + PREDICTOR=3   same codec as the source; predictor 3 is the float
  #                     predictor, which the source does not set but which is
  #                     correct for Float32 and costs nothing to read
  # BIGTIFF=IF_SAFER    some sites are >4 GB uncompressed
  gdal_translate -q -b "$CHM_BAND" \
    -co COMPRESS=LZW -co PREDICTOR=3 -co TILED=YES -co BIGTIFF=IF_SAFER \
    -mo "SOURCE_FILE=$SRC_NAME" \
    -mo "SOURCE_BAND=$CHM_BAND" \
    -mo "ORIGINAL_BAND_DESCRIPTION=$orig" \
    -mo "NOTE=Canopy height model. The source labels this band 'dsm' but the values are height above ground, not elevation above datum. Already cropped, masked and bilinear-resampled onto the reflectance grid upstream in Pix4D. See refactor-findings.md 7.14 and 5.3." \
    "$src" "$out"

  python3 - "$out" <<'PY' 2>/dev/null || true
import sys
from osgeo import gdal
ds = gdal.Open(sys.argv[1], gdal.GA_Update)
ds.GetRasterBand(1).SetDescription("chm")
ds = None
PY

  written=$((written + 1))
done

echo
echo "written $written, skipped $skipped, missing $missing"
[ "$DRY_RUN" = "1" ] && exit 0

echo
echo "Verify with:  tools/split-chm.sh --verify"
