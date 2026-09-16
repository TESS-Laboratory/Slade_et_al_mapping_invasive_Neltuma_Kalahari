#!/usr/bin/env bash
# Pure-function tests. No store, no data-in. ~seconds.
set -euo pipefail
cd "$(dirname "$0")/.."
source tools/uvr-env.sh >/dev/null
Rscript tests/testthat.R
