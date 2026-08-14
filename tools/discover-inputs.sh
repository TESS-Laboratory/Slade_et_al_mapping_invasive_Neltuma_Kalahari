#!/usr/bin/env bash
#
# discover-inputs.sh - locate the analysis inputs on a machine.
#
# Reads inst/manifest/data_manifest.csv, indexes one or more search roots, and
# reports which manifest entries were found, how many, and where.
#
# Dependencies: bash, find, awk. Nothing else. No R, no network.
#
# Usage:
#   tools/discover-inputs.sh [-o OUTDIR] [-x EXCLUDE_GLOB] ROOT [ROOT...]
#   tools/discover-inputs.sh --reuse-index INDEX [-o OUTDIR]
#
# Examples:
#   tools/discover-inputs.sh /mnt/data /home/glenn
#   tools/discover-inputs.sh -o /tmp/scan -x '*/.git/*' /
#
# Outputs, in OUTDIR (default ./discovery):
#   file-index.txt        every regular file found under the roots
#   discovery-report.tsv  machine-readable, one row per manifest entry
#   discovery-report.md   human-readable summary to bring back
#   found-paths.tsv       manifest id -> every matching path
#
set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
MANIFEST="$REPO_ROOT/inst/manifest/data_manifest.csv"

OUTDIR="./discovery"
REUSE_INDEX=""
EXCLUDES=()
ROOTS=()

while [ $# -gt 0 ]; do
  case "$1" in
    -o) OUTDIR="$2"; shift 2 ;;
    -x) EXCLUDES+=("$2"); shift 2 ;;
    --reuse-index) REUSE_INDEX="$2"; shift 2 ;;
    -m|--manifest) MANIFEST="$2"; shift 2 ;;
    -h|--help) sed -n '2,26p' "$0"; exit 0 ;;
    -*) echo "unknown option: $1" >&2; exit 2 ;;
    *) ROOTS+=("$1"); shift ;;
  esac
done

[ -f "$MANIFEST" ] || { echo "manifest not found: $MANIFEST" >&2; exit 1; }
if [ -z "$REUSE_INDEX" ] && [ ${#ROOTS[@]} -eq 0 ]; then
  echo "give at least one search ROOT, or --reuse-index. See -h." >&2; exit 2
fi

mkdir -p "$OUTDIR"
INDEX="$OUTDIR/file-index.txt"

# ---------------------------------------------------------------- build index
if [ -n "$REUSE_INDEX" ]; then
  INDEX="$REUSE_INDEX"
  echo "reusing index: $INDEX ($(wc -l < "$INDEX") files)"
else
  echo "indexing ${#ROOTS[@]} root(s). This can take a while on a large filesystem."
  : > "$INDEX"
  for r in "${ROOTS[@]}"; do
    if [ ! -d "$r" ]; then echo "  skip (not a directory): $r" >&2; continue; fi
    echo "  scanning $r"
    # shellcheck disable=SC2016
    find_args=( "$r" -type f )
    for ex in "${EXCLUDES[@]:-}"; do
      [ -n "$ex" ] && find_args+=( ! -path "$ex" )
    done
    find "${find_args[@]}" -print 2>/dev/null >> "$INDEX"
  done
  echo "indexed $(wc -l < "$INDEX") files -> $INDEX"
fi

# ------------------------------------------------- expand {a,b} in glob fields
# awk has no brace expansion; do it here and hand awk plain globs.
expand_braces() {
  local g="$1"
  if [[ "$g" == *"{"*"}"* ]]; then
    local pre rest opts post
    pre="${g%%\{*}"
    rest="${g#*\{}"
    opts="${rest%%\}*}"
    post="${rest#*\}}"
    local out=() o parts
    IFS=',' read -ra parts <<< "$opts"
    for o in "${parts[@]}"; do out+=("$(expand_braces "${pre}${o}${post}")"); done
    printf '%s\n' "${out[@]}"
  else
    printf '%s\n' "$g"
  fi
}

# --------------------------------------- normalise CSV to an internal TSV
# The manifest is RFC4180 CSV (quoted, may contain commas). Everything
# downstream is simpler on TSV, and no field contains a tab.
MTSV="$OUTDIR/.manifest.tsv"
awk '
function csvsplit(str, arr,   n, i, c, field, inq) {
  n = 0; field = ""; inq = 0
  for (i = 1; i <= length(str); i++) {
    c = substr(str, i, 1)
    if (inq) {
      if (c == "\"") {
        if (substr(str, i+1, 1) == "\"") { field = field "\""; i++ } else inq = 0
      } else field = field c
    } else {
      if (c == "\"") inq = 1
      else if (c == ",") { arr[++n] = field; field = "" }
      else field = field c
    }
  }
  arr[++n] = field
  return n
}
{ n = csvsplit($0, f)
  out = f[1]
  for (i = 2; i <= n; i++) out = out "\t" f[i]
  print out }
' "$MANIFEST" > "$MTSV"

PATTERNS="$OUTDIR/.patterns.tsv"
: > "$PATTERNS"
# skip header; emit one line per (id, expanded glob)
tail -n +2 "$MTSV" | while IFS=$'\t' read -r id type group sensor kind glob n hint avail prod req notes; do
  [ -z "${id:-}" ] && continue
  while IFS= read -r g; do
    printf '%s\t%s\n' "$id" "$g" >> "$PATTERNS"
  done < <(expand_braces "$glob")
done

# ------------------------------------------------------------------ match pass
awk -F'\t' -v patfile="$PATTERNS" -v outpaths="$OUTDIR/found-paths.tsv" '
function glob2re(g,   r) {
  r = g
  gsub(/[\\.^$+()|\[\]{}]/, "\\\\&", r)   # escape regex metachars
  gsub(/\*/, ".*", r)                      # glob * -> .*
  gsub(/\?/, ".", r)                       # glob ? -> .
  return "^" tolower(r) "$"
}
BEGIN {
  np = 0
  while ((getline line < patfile) > 0) {
    split(line, f, "\t")
    if (f[1] == "") continue
    np++; pid[np] = f[1]; pre[np] = glob2re(f[2])
  }
  close(patfile)
  printf "" > outpaths
}
{
  path = $0
  n = split(path, seg, "/")
  base = tolower(seg[n])
  for (i = 1; i <= np; i++) {
    if (base ~ pre[i]) {
      cnt[pid[i]]++
      if (cnt[pid[i]] <= 5) sample[pid[i]] = sample[pid[i]] (sample[pid[i]] == "" ? "" : "\n") path
      printf "%s\t%s\n", pid[i], path >> outpaths
    }
  }
}
END {
  for (k in cnt) printf "%s\t%d\t%s\n", k, cnt[k], sample[k] > "/dev/stderr"
}
' "$INDEX" 2>"$OUTDIR/.counts.raw" >/dev/null

# collapse the multi-line samples emitted above into a keyed count file
awk -F'\t' '/^[a-z0-9_]+\t[0-9]+\t/ {print $1"\t"$2}' "$OUTDIR/.counts.raw" > "$OUTDIR/.counts.tsv"

# ---------------------------------------------------------------- write report
REPORT_TSV="$OUTDIR/discovery-report.tsv"
REPORT_MD="$OUTDIR/discovery-report.md"

awk -F'\t' -v counts="$OUTDIR/.counts.tsv" -v paths="$OUTDIR/found-paths.tsv" \
    -v tsv="$REPORT_TSV" -v md="$REPORT_MD" -v host="$(hostname 2>/dev/null)" \
    -v when="$(date -u +%Y-%m-%dT%H:%M:%SZ)" '
BEGIN {
  while ((getline l < counts) > 0) { split(l, c, "\t"); found[c[1]] = c[2] }
  close(counts)
  while ((getline l < paths) > 0) {
    split(l, p, "\t")
    if (nsample[p[1]]++ < 3) example[p[1]] = example[p[1]] (example[p[1]]=="" ? "" : " ; ") p[2]
  }
  close(paths)
  print "id\ttype\tgroup\tavailability\texpected\tfound\tstatus\texample_paths" > tsv
  print "# Input discovery report" > md
  print "" > md
  print "- host: " host > md
  print "- generated: " when > md
  print "" > md
}
NR == 1 { next }
{
  id=$1; type=$2; group=$3; glob=$6; expn=$7; hint=$8; avail=$9; prod=$10; req=$11
  f = (id in found) ? found[id] : 0
  if (f == 0)            st = "MISSING"
  else if (expn+0 == 0)  st = "FOUND"
  else if (f >= expn+0)  st = "OK"
  else                   st = "PARTIAL"
  printf "%s\t%s\t%s\t%s\t%s\t%d\t%s\t%s\n", id, type, group, avail, expn, f, st, example[id] > tsv
  order[++n] = id
  R_type[id]=type; R_group[id]=group; R_glob[id]=glob; R_exp[id]=expn
  R_found[id]=f; R_st[id]=st; R_avail[id]=avail; R_hint[id]=hint; R_prod[id]=prod
  R_req[id]=req; R_ex[id]=example[id]
  tot[st]++; tot2[st "/" type]++
}
END {
  print "## Summary" > md
  print "" > md
  print "| status | n |" > md
  print "|---|---|" > md
  for (s in tot) print "| " s " | " tot[s] " |" > md
  print "" > md
  print "`OK` found at least the expected count. `PARTIAL` found some. `MISSING` found none." > md
  print "" > md
  print "## Missing INPUTS - must be sourced, cannot be rebuilt" > md
  print "" > md
  print "| id | group | availability | expected | original location hint | needed for |" > md
  print "|---|---|---|---|---|---|" > md
  for (i = 1; i <= n; i++) { id = order[i]
    if (R_st[id] == "MISSING" && R_type[id] == "input")
      print "| `" id "` | " R_group[id] " | " R_avail[id] " | " R_exp[id] " | `" R_hint[id] "` | " R_req[id] " |" > md }
  print "" > md
  print "## Missing DERIVED - rebuildable if their own inputs are found" > md
  print "" > md
  print "| id | expected | produced by | needed for |" > md
  print "|---|---|---|---|" > md
  for (i = 1; i <= n; i++) { id = order[i]
    if (R_st[id] == "MISSING" && R_type[id] == "derived")
      print "| `" id "` | " R_exp[id] " | " R_prod[id] " | " R_req[id] " |" > md }
  print "" > md
  print "Entries whose `produced by` reads NO PRODUCER FOUND cannot be rebuilt" > md
  print "from anything in the codebase. Treat those as inputs." > md
  print "" > md
  print "## Partially found" > md
  print "" > md
  print "| id | expected | found | example paths |" > md
  print "|---|---|---|---|" > md
  for (i = 1; i <= n; i++) { id = order[i]
    if (R_st[id] == "PARTIAL")
      print "| `" id "` | " R_exp[id] " | " R_found[id] " | " R_ex[id] " |" > md }
  print "" > md
  print "## Found" > md
  print "" > md
  print "| id | expected | found | example paths |" > md
  print "|---|---|---|---|" > md
  for (i = 1; i <= n; i++) { id = order[i]
    if (R_st[id] == "OK" || R_st[id] == "FOUND")
      print "| `" id "` | " R_exp[id] " | " R_found[id] " | " R_ex[id] " |" > md }
}
' "$MTSV"

rm -f "$MTSV" "$OUTDIR/.patterns.tsv" "$OUTDIR/.counts.raw" "$OUTDIR/.counts.tsv"

echo
echo "wrote:"
echo "  $REPORT_MD"
echo "  $REPORT_TSV"
echo "  $OUTDIR/found-paths.tsv"
echo
awk -F'\t' 'NR>1{c[$7]++; d[$7"/"$2]++} END{for (s in c) printf "  %-8s %d\n", s, c[s]; print ""; for (k in d) printf "  %-18s %d\n", k, d[k]}' "$REPORT_TSV"
