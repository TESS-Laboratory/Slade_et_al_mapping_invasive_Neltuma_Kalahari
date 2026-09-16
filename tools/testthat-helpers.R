# Load the pipeline's functions the way _targets.R does (tar_source), without
# building a graph.
suppressMessages({ for (f in list.files("R", full.names = TRUE)) source(f) })
