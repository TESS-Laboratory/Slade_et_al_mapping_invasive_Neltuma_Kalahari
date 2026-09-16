#!/usr/bin/env bash
#
# convert-vectors.sh - one-time conversion of every mirrored shapefile under
# data-in/ to FlatGeobuf (.fgb), next to the original, same stem.
#
# Decision 5.4 / refactor-3.0 4.3: no shapefiles in the pipeline. The .fgb
# carries its CRS in-file, so the two layers that shipped without a .prj
# (WV2_clip, All_points_buffered_additional - finding 7.13) get EPSG:32734
# DECLARED here, once, with the decision recorded in the provenance log. Their
# coordinates were verified to be UTM 34S (they sit inside the WV2 mosaic).
#
# Originals are left untouched (they are the mirror); the pipeline reads .fgb.
# Vendor GIS_FILES under the WV2 tile orders are skipped: nothing reads them.
#
# Usage: tools/convert-vectors.sh [--dry-run]
set -euo pipefail
DRY=0; [ "${1:-}" = "--dry-run" ] && DRY=1
LOG="data-in/vector_conversion.csv"
[ "$DRY" = 0 ] && printf '"fgb","shp","declared_crs","features","converted_at"\n' > "$LOG"
n=0; skipped=0
while IFS= read -r shp; do
  case "$shp" in */GIS_FILES/*) skipped=$((skipped+1)); continue;; esac
  fgb="${shp%.shp}.fgb"
  declared=""
  if [ ! -f "${shp%.shp}.prj" ]; then declared="EPSG:32734"; fi
  n=$((n+1))
  if [ "$DRY" = 1 ]; then echo "$shp -> $fgb ${declared:+(declare $declared)}"; continue; fi
  if [ -n "$declared" ]; then
    ogr2ogr -q -f FlatGeobuf -a_srs "$declared" -overwrite "$fgb" "$shp"
  else
    ogr2ogr -q -f FlatGeobuf -overwrite "$fgb" "$shp"
  fi
  feats=$(ogrinfo -ro -so "$fgb" "$(basename "${fgb%.fgb}")" 2>/dev/null | awk -F': ' '/Feature Count/ {print $2}')
  printf '"%s","%s","%s","%s","%s"\n' "$fgb" "$shp" "$declared" "$feats" "$(date -Is)" >> "$LOG"
done < <(find data-in -name '*.shp' | sort)
echo "converted $n shapefile(s), skipped $skipped vendor file(s)$([ "$DRY" = 1 ] && echo ' (dry run)')"
[ "$DRY" = 0 ] && echo "log: $LOG"
