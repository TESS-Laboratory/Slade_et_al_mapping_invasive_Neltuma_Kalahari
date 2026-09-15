#!/usr/bin/env bash
#
# mirror-results.sh - copy the surviving benchmark and confusion workbooks into
# data-in/results/ so they can be read without further access to the source tree.
#
# WHY
#
# Open question 6.4 asks which training-set rung each satellite run consumed,
# which decides whether Table S7 or the code is right (finding 2.7). The console
# histories did not settle it (7.10) and the .RData files are deliberately not
# being opened. The archived outputs are the remaining evidence: their filenames
# already show configurations that contradict Table S7, and their contents should
# show the per-class counts directly.
#
# These are small spreadsheets, not rasters - the whole set is a few tens of MB.
#
# Reads from Glen's tree, so needs sudo. Writes only into this repo's
# data-in/results/, which is gitignored, and chowns the result back to the
# invoking user so the files are not left root-owned.
#
# Usage:
#   sudo tools/mirror-results.sh --dry-run
#   sudo tools/mirror-results.sh
#   sudo tools/mirror-results.sh --with-training --dry-run
#   sudo tools/mirror-results.sh --with-training

set -euo pipefail

SRC_ROOT="${SRC_ROOT:-/raid/home/gs558}"
DEST_ROOT="data-in/results"
# Scan every argument, not just $1: "--with-training --dry-run" must still be a
# dry run, and getting that wrong writes when the user asked to preview.
DRY_RUN=0
for a in "$@"; do [ "$a" = "--dry-run" ] && DRY_RUN=1; done

# "<source directory>|<destination subdirectory>"
RESULT_SETS=(
  "$SRC_ROOT/Glenn-Prosopis-ML/data_out/Bench|glenn_prosopis_ml/bench"
  "$SRC_ROOT/Glenn-Prosopis-ML/data_out/Confusion|glenn_prosopis_ml/confusion"
  "$SRC_ROOT/Glenn-Prosopis-ML/data_out/Confusion_back_up/back_up|glenn_prosopis_ml/confusion_backup"
  "$SRC_ROOT/MLR3_pipeline/data_out/S2|mlr3_pipeline/s2"
  "$SRC_ROOT/MLR3_pipeline/data_out/Planet|mlr3_pipeline/planet"
  "$SRC_ROOT/MLR3_pipeline/data_out/Dinaka|mlr3_pipeline/dinaka"
  "$SRC_ROOT/MLR3_pipeline/data_out/Dinaka/Bench|mlr3_pipeline/dinaka_bench"
  "$SRC_ROOT/MLR3_pipeline/data_out/Dinaka/Confusion|mlr3_pipeline/dinaka_confusion"
)

# Serialised training tables. NOTE these are .rds, not .RData: a single
# serialised object with no session state, environments or attached packages -
# safe to read, unlike the session snapshots which are deliberately untouched.
#
# Two things they can settle that nothing else can:
#   *ML_in_point_level.rds        the response column name, Type vs Class,
#                                 which the two copies of build_ml_df disagree
#                                 about (finding 3.3)
#   *_pixel_extract_full_train_N  the satellite training rungs, with the purity
#                                 threshold in the filename - an independent
#                                 check on sensors.csv (7.19)
#
# Enabled with --with-training because they may be large; run --dry-run first.
TRAINING_SETS=(
  "$SRC_ROOT/Glenn-Prosopis-ML/data_out|glenn_prosopis_ml/ml_in"
  "$SRC_ROOT/MLR3_pipeline/data_out|mlr3_pipeline/ml_in"
  "$SRC_ROOT/Glenn-Prosopis-ML/data_in|glenn_prosopis_ml/pixel_extract"
)
WITH_TRAINING=0
for a in "$@"; do [ "$a" = "--with-training" ] && WITH_TRAINING=1; done

# Satellite imagery for the WV2/Planet/S2 arms (sections 3.2-3.4). These
# survive in Glen's tree (availability "external" described origin, not
# location). Includes WV2_clip.shp - the AOI with no .prj (finding 7.13).
#
# Same-destination pairs merge FLAT, later source wins on a basename clash.
# The Planet pair is safe: every clash checked byte-identical (MLR3_pipeline
# holds 2024 copies of the 2023 originals). The S2 sources are NOT: both
# MLR3_pipeline/data_in/S2 and Glenn data_out/S2 ship an S2_stack.tif with
# different contents (101 MB / 9-band vs 152 MB), so data_out gets its own
# subdirectory. Finding 7.32.
SAT_SETS=(
  "$SRC_ROOT/Glenn-Prosopis-ML/data_in/Planet_2022|../planet/raw"
  "$SRC_ROOT/MLR3_pipeline/data_in/Planet|../planet/raw"
  "$SRC_ROOT/Glenn-Prosopis-ML/data_in/S2|../s2/raw"
  "$SRC_ROOT/MLR3_pipeline/data_in/S2|../s2/raw"
  "$SRC_ROOT/Glenn-Prosopis-ML/data_out/S2|../s2/raw/glenn_out"
  "$SRC_ROOT/Glenn-Prosopis-ML/data_in/WV2|../wv2/glenn"
)
WITH_SAT=0
for a in "$@"; do [ "$a" = "--with-satellite" ] && WITH_SAT=1; done

if [ "$(id -u)" -ne 0 ]; then
  if [ "$DRY_RUN" = "0" ]; then
    echo "needs sudo to read $SRC_ROOT - re-run as: sudo $0" >&2
    exit 1
  fi
  echo "NOTE: not running as root, so $SRC_ROOT cannot be stat'ed and every"
  echo "      directory below will report MISSING. Use 'sudo $0 --dry-run' for"
  echo "      a dry run that reflects what is actually there."
  echo
fi

PROV="$DEST_ROOT/provenance.csv"
[ "$DRY_RUN" = "0" ] && mkdir -p "$DEST_ROOT"
[ "$DRY_RUN" = "0" ] && printf '"destination","source","bytes","source_mtime"\n' > "$PROV"

total=0; copied=0; missing=0

for entry in "${RESULT_SETS[@]}"; do
  src="${entry%%|*}"
  sub="${entry##*|}"
  dst="$DEST_ROOT/$sub"

  if [ ! -d "$src" ]; then
    echo "MISSING DIR  $src"; missing=$((missing + 1)); continue
  fi

  n=$(find "$src" -maxdepth 1 -type f -iname '*.xlsx' | wc -l)
  echo "$sub  <-  $src  ($n workbooks)"
  total=$((total + n))
  [ "$DRY_RUN" = "1" ] && continue

  mkdir -p "$dst"
  while IFS= read -r f; do
    base=$(basename "$f")
    cp -p "$f" "$dst/$base"
    printf '"%s","%s","%s","%s"\n' \
      "$sub/$base" "$f" "$(stat -c%s "$f")" "$(stat -c '%y' "$f" | cut -d. -f1)" >> "$PROV"
    copied=$((copied + 1))
  done < <(find "$src" -maxdepth 1 -type f -iname '*.xlsx' | sort)
done

# --- training tables (.rds), opt-in ----------------------------------------
if [ "$WITH_TRAINING" = "1" ]; then
  echo
  for entry in "${TRAINING_SETS[@]}"; do
    src="${entry%%|*}"; sub="${entry##*|}"; dst="$DEST_ROOT/$sub"
    if [ ! -d "$src" ]; then
      echo "MISSING DIR  $src"; missing=$((missing + 1)); continue
    fi
    # Recursive here: these sit one level down, in per-site subdirectories.
    n=$(find "$src" -type f \( -iname '*ML_in_point_level.rds' -o \
                                -iname '*pixel_extract_full_train_*.rds' \) | wc -l)
    sz=$(find "$src" -type f \( -iname '*ML_in_point_level.rds' -o \
                                 -iname '*pixel_extract_full_train_*.rds' \) \
         -printf '%s\n' 2>/dev/null | awk '{t+=$1} END {printf "%.1f MB", t/1048576}')
    echo "$sub  <-  $src  ($n tables, $sz)"
    total=$((total + n))
    [ "$DRY_RUN" = "1" ] && continue

    mkdir -p "$dst"
    while IFS= read -r f; do
      base=$(basename "$f")
      cp -p "$f" "$dst/$base"
      printf '"%s","%s","%s","%s"\n' \
        "$sub/$base" "$f" "$(stat -c%s "$f")" "$(stat -c '%y' "$f" | cut -d. -f1)" >> "$PROV"
      copied=$((copied + 1))
    done < <(find "$src" -type f \( -iname '*ML_in_point_level.rds' -o \
                                     -iname '*pixel_extract_full_train_*.rds' \) | sort)
  done
fi

# --- satellite rasters, opt-in ----------------------------------------------
if [ "$WITH_SAT" = "1" ]; then
  echo
  for entry in "${SAT_SETS[@]}"; do
    src="${entry%%|*}"; sub="${entry##*|}"; dst="$DEST_ROOT/$sub"
    if [ ! -d "$src" ]; then
      echo "MISSING DIR  $src"; missing=$((missing + 1)); continue
    fi
    n=$(find "$src" -maxdepth 1 -type f \( -iname '*.tif' -o -iname '*.shp' -o \
        -iname '*.shx' -o -iname '*.dbf' -o -iname '*.prj' -o -iname '*.cpg' \) | wc -l)
    sz=$(find "$src" -maxdepth 1 -type f \( -iname '*.tif' -o -iname '*.shp' -o \
         -iname '*.shx' -o -iname '*.dbf' -o -iname '*.prj' -o -iname '*.cpg' \) \
         -printf '%s\n' | awk '{t+=$1} END {printf "%.1f GB", t/1073741824}')
    echo "$sub  <-  $src  ($n files, $sz)"
    total=$((total + n))
    [ "$DRY_RUN" = "1" ] && continue
    mkdir -p "$dst"
    while IFS= read -r f; do
      base=$(basename "$f")
      cp -p "$f" "$dst/$base"
      printf '"%s","%s","%s","%s"\n' \
        "$sub/$base" "$f" "$(stat -c%s "$f")" "$(stat -c '%y' "$f" | cut -d. -f1)" >> "$PROV"
      copied=$((copied + 1))
    done < <(find "$src" -maxdepth 1 -type f \( -iname '*.tif' -o -iname '*.shp' -o \
             -iname '*.shx' -o -iname '*.dbf' -o -iname '*.prj' -o -iname '*.cpg' \) | sort)
  done
fi

echo
if [ "$DRY_RUN" = "1" ]; then
  echo "DRY RUN - $total file(s) would be copied, $missing directories missing"
  exit 0
fi

# Do not leave root-owned files behind.
owner="${SUDO_UID:-0}:${SUDO_GID:-0}"
if [ "$owner" != "0:0" ]; then
  # data-in, not just results/: the satellite set writes into data-in/planet,
  # data-in/s2 and data-in/wv2/glenn, which sit outside DEST_ROOT.
  chown -R "$owner" "$(dirname "$DEST_ROOT")"
  echo "chowned $(dirname "$DEST_ROOT") to $owner"
fi

echo "copied $copied file(s), $missing directories missing"
echo "provenance: $PROV"
du -sh "$DEST_ROOT"
