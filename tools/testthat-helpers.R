# Load the pipeline's functions the way _targets.R does (tar_source), without
# building a graph. testthat runs tests with tests/testthat as the working
# directory, so the config paths are made absolute here.
suppressMessages({ for (f in list.files("R", full.names = TRUE)) source(f) })
CONFIG_DIR <- normalizePath(CONFIG_DIR)
if (exists("CLASSES_JSON")) CLASSES_JSON <- normalizePath(CLASSES_JSON)
if (exists("MANIFEST_CSV")) MANIFEST_CSV <- normalizePath(MANIFEST_CSV)
