# Source this before any uvr command in this project:
#
#     source tools/uvr-env.sh
#
# WHY THIS IS NOT OPTIONAL
#
# uvr defaults to pre-built P3M binaries and correctly identifies this host as
# "Linux noble" (Ubuntu 24.04). But the P3M noble builds link GDAL 3.8
# (libgdal.so.34), and this machine carries GDAL 3.11.4 (libgdal.so.37) from
# ubuntugis. Every GDAL-linked binary therefore fails to load:
#
#     $ ldd .uvr/library/terra/libs/terra.so
#     libgdal.so.34 => not found          <- GEOS and PROJ resolve fine
#
# Forcing a source build makes R CMD INSTALL run each package's own configure,
# which shells out to gdal-config on PATH and links against the GDAL that is
# actually installed. This affects terra, sf and exactextractr today, and any
# future GDAL/GEOS/PROJ-linked package.
#
# uvr 0.4.6 has no uvr.toml key for this - it is flag-or-environment only - so
# the setting lives here rather than in the manifest. It is deliberately blunt:
# UVR_NO_BINARY applies to every package, not just the spatial ones. Pure-R
# packages "build" in seconds, and a per-package allowlist would rot.
#
# See docs/environment.md and refactor-findings.md finding 7.16.

# Build every package from source. Equivalent to passing --no-binary.
export UVR_NO_BINARY=1

# Parallel compilation. R CMD INSTALL propagates MAKEFLAGS to package builds
# (terra: 1m20s wall for 6m11s CPU). This is a shared 64-core box, so the
# default is conservative. Override before sourcing if you need more.
export MAKEFLAGS="${MAKEFLAGS:--j16}"

# Rendering toolchain. Quarto 1.10.18 is installed system-wide and bundles its
# own pandoc, so no separate pandoc package is needed.
#
# Two problems this solves:
#
# 1. rstudio-server ships its own Quarto (1.8.25) at
#    /usr/lib/rstudio-server/bin/quarto/bin, which sits EARLIER in PATH than the
#    /usr/local/bin symlink the .deb creates. Without the prepend below, `quarto`
#    silently means 1.8.25.
#
# 2. rmarkdown/knitr find pandoc via RSTUDIO_PANDOC, which Positron sets to its
#    own bundled copy under ~/.positron-server/bin/<build-hash>/... That path
#    breaks on the next IDE update and is never set in a batch Rscript or
#    targets run - which is exactly the case that has to work.
if [ -x /opt/quarto/bin/quarto ]; then
  export PATH="/opt/quarto/bin:${PATH}"
  export RSTUDIO_PANDOC="/opt/quarto/bin/tools/x86_64"
  export QUARTO_PANDOC="${RSTUDIO_PANDOC}/pandoc"
fi

if [ -f .uvr/activate ]; then
  # shellcheck disable=SC1091
  . .uvr/activate
fi

echo "uvr: source builds forced (UVR_NO_BINARY=1), MAKEFLAGS=${MAKEFLAGS}"
if [ -x /opt/quarto/bin/quarto ]; then
  echo "uvr: quarto $(/opt/quarto/bin/quarto --version), pandoc via RSTUDIO_PANDOC"
fi
