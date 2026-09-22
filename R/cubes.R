#' Predictor cube assembly
#'
#' Stacks are assembled on demand as GDAL VRTs from independent single-purpose
#' rasters, never stored pre-combined (finding 5.3). A VRT is a few kB of XML, so
#' every predictor combination in stacks.csv is essentially free, and the band
#' provenance stays visible instead of being welded into a GeoTIFF.
#'
#' THE TRAP THIS CODE EXISTS TO AVOID
#'
#' The vegetation-index rasters are NOT on the same footprint as refl_stack.tif.
#' They were computed on the uncropped mosaic, so at Bokspits_1 they run
#' 12209 x 10309 against the reflectance stack's 9151 x 8221. The grids are
#' pixel-aligned - the offsets are whole pixel counts - but the extents differ.
#'
#' `gdalbuildvrt` defaults to the UNION of its inputs. Assembling a cube without
#' constraining the extent would therefore silently produce a larger grid with
#' the reflectance bands padded out in nodata, changing every extracted value
#' while looking entirely reasonable. Every cube is pinned to the reflectance
#' grid and then verified against it.

#' Which file supplies each band
#'
#' Band names come from stacks.csv, which derives from DRONE_STACK_BANDS.
BAND_SOURCES <- c(
  blue = "refl_stack.tif", green = "refl_stack.tif", red = "refl_stack.tif",
  red_edge = "refl_stack.tif", nir = "refl_stack.tif",
  chm = "chm.tif",
  ndvi = "ndvi.tif", savi = "savi.tif",
  msavi = "msavi.tif", msavi2 = "msavi2.tif", mtvi = "mtvi.tif"
)


#' Ordered source files for a predictor stack
#'
#' Returns one entry per distinct file, in band order. The five reflectance bands
#' live in one file and collapse to a single entry: `gdalbuildvrt -separate`
#' expands a multi-band input into consecutive bands, so listing refl_stack.tif
#' once yields bands 1 to 5.
#'
#' @param site site id
#' @param tag stack tag from stacks.csv
#' @param stacks the stacks table
#' @param root directory holding the per-site drone rasters
#' @return character vector of file paths, in band order
cube_sources <- function(site, tag, stacks = read_stacks(),
                         root = "data-in/drone") {
  s <- stacks[stacks$tag == tag, , drop = FALSE]
  if (nrow(s) != 1L) {
    stop("No stack called '", tag, "' in stacks.csv. Available: ",
         paste(stacks$tag, collapse = ", "), call. = FALSE)
  }
  bands <- s$band_list[[1]]

  unknown <- setdiff(bands, names(BAND_SOURCES))
  if (length(unknown)) {
    stop("stacks.csv names band(s) with no source file: ",
         paste(unknown, collapse = ", "), call. = FALSE)
  }

  files <- unname(BAND_SOURCES[bands])
  files <- files[!duplicated(files)]          # keeps first appearance, so order holds
  paths <- file.path(root, site, files)

  gone <- paths[!file.exists(paths)]
  if (length(gone)) {
    stop("Cannot assemble stack '", tag, "' for ", site, ": missing\n",
         paste0("    ", gone, collapse = "\n"), call. = FALSE)
  }
  paths
}


#' Write band descriptions into a VRT
#'
#' gdalbuildvrt does not carry band descriptions across, and an unnamed cube is
#' how a CHM ends up being read as a DSM (finding 7.14). The VRT is plain XML, so
#' the description is inserted directly.
#'
#' @param vrt path to the VRT
#' @param names character vector of band names, in order
#' @return the vrt path, invisibly
set_vrt_band_names <- function(vrt, names) {
  x <- readLines(vrt, warn = FALSE)
  hits <- grep("<VRTRasterBand ", x)
  if (length(hits) != length(names)) {
    stop("VRT has ", length(hits), " bands but ", length(names),
         " names were supplied for ", vrt, call. = FALSE)
  }
  # Walk backwards so earlier insertion points stay valid.
  for (i in rev(seq_along(hits))) {
    indent <- sub("^(\\s*).*", "\\1", x[hits[i]])
    x <- append(x, paste0(indent, "  <Description>", names[i], "</Description>"),
                after = hits[i])
  }
  writeLines(x, vrt)
  invisible(vrt)
}


#' Assemble a predictor cube as a VRT
#'
#' @param site site id
#' @param tag stack tag from stacks.csv
#' @param stacks the stacks table
#' @param sites the sites table, used to pin and verify the grid
#' @param root directory holding the per-site drone rasters
#' @param out_dir where to write the VRT
#' @return path to the written VRT
build_cube <- function(site, tag, stacks = read_stacks(), sites = read_sites(),
                       root = "data-in/drone", out_dir = out_path("cubes")) {
  s <- stacks[stacks$tag == tag, , drop = FALSE]
  bands <- s$band_list[[1]]
  srcs  <- cube_sources(site, tag, stacks = stacks, root = root)

  # The reflectance stack defines the grid. Everything else is pinned to it.
  ref <- terra::rast(file.path(root, site, "refl_stack.tif"))
  e   <- as.vector(terra::ext(ref))

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  vrt <- file.path(out_dir, paste0(site, "__", tag, ".vrt"))

  sf::gdal_utils(
    util = "buildvrt",
    source = srcs,
    destination = vrt,
    options = c(
      "-separate",
      # Pin to the reflectance extent. Without this gdalbuildvrt takes the union
      # of the inputs and the VI rasters silently enlarge the grid.
      # sprintf, not format(): format() right-pads to a common width and GDAL
      # rejects the leading spaces. Ten decimals is far more precision than
      # these metre-scale coordinates need.
      "-te", sprintf("%.10f", c(e["xmin"], e["ymin"], e["xmax"], e["ymax"])),
      "-resolution", "user",
      "-tr", sprintf("%.10f", terra::res(ref)),
      "-r", "nearest"
    ),
    quiet = TRUE
  )

  set_vrt_band_names(vrt, bands)
  assert_cube_grid(vrt, site, tag, expect_bands = length(bands), sites = sites, ref = ref)
  vrt
}


#' Verify an assembled cube against the reflectance grid
#'
#' Assembly is only safe because of the `-te` pin above, so the pin is checked
#' rather than trusted.
#'
#' @param vrt path to the VRT
#' @param site site id
#' @param tag stack tag
#' @param expect_bands expected band count
#' @param sites the sites table
#' @param ref optional pre-opened reference raster
#' @return one-row data.frame describing the cube, invisibly
assert_cube_grid <- function(vrt, site, tag, expect_bands, sites = read_sites(),
                             ref = NULL) {
  if (is.null(ref)) ref <- terra::rast(file.path("data-in/drone", site, "refl_stack.tif"))
  cube <- terra::rast(vrt)

  problems <- character(0)
  if (!identical(dim(cube)[1:2], dim(ref)[1:2])) {
    problems <- c(problems, paste0(
      "dimensions are ", paste(dim(cube)[1:2], collapse = "x"),
      ", reflectance grid is ", paste(dim(ref)[1:2], collapse = "x"),
      " - the extent pin did not hold"))
  }
  if (!isTRUE(all.equal(as.vector(terra::ext(cube)), as.vector(terra::ext(ref)),
                        tolerance = 1e-6))) {
    problems <- c(problems, "extent differs from the reflectance grid")
  }
  if (!identical(as.integer(terra::nlyr(cube)), as.integer(expect_bands))) {
    problems <- c(problems, paste0(
      "band count is ", terra::nlyr(cube), ", expected ", expect_bands))
  }
  if (length(problems)) {
    stop("Cube '", tag, "' for ", site, " failed verification:\n",
         paste0("    - ", problems, collapse = "\n"), call. = FALSE)
  }

  invisible(data.frame(
    site = site, tag = tag, vrt = vrt,
    n_bands = as.integer(terra::nlyr(cube)),
    ncol = terra::ncol(cube), nrow = terra::nrow(cube),
    bands = paste(names(cube), collapse = "|"),
    stringsAsFactors = FALSE
  ))
}
