#!/usr/bin/env bash
#
# mirror-inputs.sh - mirror the original input data into data-in/ under a
# consistent, sensor-then-location naming scheme.
#
# Reads from the original analysis tree (Glen's Glenn-Prosopis-ML/data_in),
# which needs sudo. Writes into this repo's data-in/, which is gitignored.
#
# Every copy is recorded in data-in/provenance.csv so the mirror stays
# auditable: destination, source, size and mtime.
#
# Dependencies: bash, find, cp, stat, du. Nothing else.
#
# Usage:
#   sudo tools/mirror-inputs.sh --dry-run     # list what would be copied, with sizes
#   sudo tools/mirror-inputs.sh               # do it
#   sudo tools/mirror-inputs.sh -s SRC        # override the source tree
#
set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

SRC="/raid/home/gs558/Glenn-Prosopis-ML/data_in"
DEST="$REPO_ROOT/data-in"
DRY_RUN=0

while [ $# -gt 0 ]; do
  case "$1" in
    -n|--dry-run) DRY_RUN=1; shift ;;
    -s|--source)  SRC="$2"; shift 2 ;;
    -d|--dest)    DEST="$2"; shift 2 ;;
    -h|--help)    sed -n '2,18p' "$0"; exit 0 ;;
    *) echo "unknown option: $1" >&2; exit 2 ;;
  esac
done

[ -d "$SRC" ] || { echo "source tree not readable: $SRC" >&2
                   echo "(this needs sudo - it reads another user's directory)" >&2; exit 1; }

PROV="$DEST/provenance.csv"
TOTAL_BYTES=0
COPIED=0
MISSING=0

# The seven drone AOIs.
SITES=(Bokspits_1 Bokspits_2 Bokspits_3
       Struizendam_1 Struizendam_2 Struizendam_3 Struizendam_4)

# Per-site product map: "<source suffix>|<destination basename>".
# Shapefiles are named without extension and carried as a sidecar set.
DRONE_RASTERS=(
  "_Refl_StackCrop.tif|refl_stack.tif"
  "_Refl_StackCrop_CHM.tif|refl_stack_chm.tif"
  "_NDVI.tif|ndvi.tif"
  "_SAVI.tif|savi.tif"
  "_MSAVI.tif|msavi.tif"
  "_MSAVI2.tif|msavi2.tif"
  "_MTVI.tif|mtvi.tif"
)
DRONE_VECTORS=(
  "_clip|aoi"
  "_Field_data_points_All_b30|field_points"
)

# Shapefile sidecars. Not all are always present; absence of .prj is a finding,
# not an error, so we record what we see rather than requiring the full set.
SIDECARS=(shp shx dbf prj cpg qpj sbn sbx)

log_prov () {  # dest_rel  src_abs
  [ "$DRY_RUN" -eq 1 ] && return 0
  local sz mt
  sz=$(stat -c %s "$2" 2>/dev/null || echo 0)
  mt=$(stat -c %y "$2" 2>/dev/null | cut -d. -f1)
  printf '"%s","%s","%s","%s"\n' "$1" "$2" "$sz" "$mt" >> "$PROV"
}

copy_one () {  # src_abs  dest_abs
  local src="$1" dst="$2" sz
  if [ ! -f "$src" ]; then
    printf '  MISSING  %s\n' "${src#$SRC/}"
    MISSING=$((MISSING + 1))
    return 1
  fi
  sz=$(stat -c %s "$src" 2>/dev/null || echo 0)
  TOTAL_BYTES=$((TOTAL_BYTES + sz))
  COPIED=$((COPIED + 1))
  printf '  %8s  %s -> %s\n' "$(numfmt --to=iec "$sz" 2>/dev/null || echo "$sz")" \
         "${src#$SRC/}" "${dst#$DEST/}"
  if [ "$DRY_RUN" -eq 0 ]; then
    mkdir -p "$(dirname "$dst")"
    cp -p "$src" "$dst" || return 1
    log_prov "${dst#$DEST/}" "$src"
  fi
  return 0
}

copy_shapefile () {  # src_stem  dest_stem
  local found=0 ext
  for ext in "${SIDECARS[@]}"; do
    if [ -f "$1.$ext" ]; then
      copy_one "$1.$ext" "$2.$ext" && found=1
    fi
  done
  if [ "$found" -eq 0 ]; then
    printf '  MISSING  %s.* (no sidecars found)\n' "${1#$SRC/}"
    MISSING=$((MISSING + 1))
  fi
}

# ------------------------------------------------------------------ set up
if [ "$DRY_RUN" -eq 0 ]; then
  mkdir -p "$DEST"
  printf '"destination","source","bytes","source_mtime"\n' > "$PROV"
fi

[ "$DRY_RUN" -eq 1 ] && echo "DRY RUN - nothing will be written" && echo
echo "source: $SRC"
echo "dest:   $DEST"
echo

# ------------------------------------------------------------- drone sites
for site in "${SITES[@]}"; do
  slug=$(echo "$site" | tr '[:upper:]' '[:lower:]')
  echo "drone/$slug"
  for map in "${DRONE_RASTERS[@]}"; do
    copy_one "$SRC/$site/${site}${map%%|*}" "$DEST/drone/$slug/${map##*|}"
  done
  for map in "${DRONE_VECTORS[@]}"; do
    copy_shapefile "$SRC/$site/${site}${map%%|*}" "$DEST/drone/$slug/${map##*|}"
  done
  echo
done

# ---------------------------------------------------------- sensor grids
# Per-site polygon grids, one directory per sensor.
for pair in "WV2_Grids|wv2|_WV2_grid" "S2_Grids|s2|_S2_grid" "Planet_Grids|planet|_Planet_grid"; do
  IFS='|' read -r srcdir sensor suffix <<< "$pair"
  echo "$sensor/grids"
  for site in "${SITES[@]}"; do
    slug=$(echo "$site" | tr '[:upper:]' '[:lower:]')
    [ -f "$SRC/$srcdir/${site}${suffix}.shp" ] || continue
    copy_shapefile "$SRC/$srcdir/${site}${suffix}" "$DEST/$sensor/grids/$slug"
  done
  echo
done

# ---------------------------------------------------------------- shared
echo "shared"
copy_one "$SRC/Veg_type_lookup_list.xlsx" "$DEST/shared/veg_type_lookup.xlsx"
echo

# ---------------------------------------------------------------- summary
echo "----------------------------------------------------------------"
printf 'files:   %d\n' "$COPIED"
printf 'missing: %d\n' "$MISSING"
printf 'size:    %s\n' "$(numfmt --to=iec "$TOTAL_BYTES" 2>/dev/null || echo "$TOTAL_BYTES bytes")"
if [ "$DRY_RUN" -eq 0 ]; then
  echo "provenance: ${PROV#$REPO_ROOT/}"
  echo
  echo "NOTE: files are owned by root after a sudo run. Fix with:"
  echo "  sudo chown -R \$(id -un):\$(id -gn) $DEST"
fi
