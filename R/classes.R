#' Class scheme accessors
#'
#' Single entry point for the land-cover class scheme. Replaces
#' `Neltuma_Mlr3_Pipeline/data_in/Veg_type_lookup_list.xlsx`, which is superseded
#' and must not be read directly.
#'
#' Source of truth: inst/config/classes.json

CLASSES_JSON <- "inst/config/classes.json"


#' Read the class scheme
#'
#' @param path location of classes.json
#' @return the parsed list, with `$classes` as a list of class records
read_class_scheme <- function(path = CLASSES_JSON) {
  if (!file.exists(path)) {
    stop("Class scheme not found at '", path, "'. ",
         "Run from the project root.", call. = FALSE)
  }
  jsonlite::fromJSON(path, simplifyVector = FALSE)
}


#' Class lookup as a data frame
#'
#' The replacement for the old `read_xlsx(lookup_file, col_names = ...)` call in
#' `build_ml_df()`. Returns `Type` as integer, never character.
#'
#' @param scheme one of "field", "drone_training", "satellite_training".
#'   Filters to the codes participating in that scheme. "field" returns all.
#' @param path location of classes.json
#' @return data.frame with Type, Class, Label, Description
class_lookup <- function(scheme = c("field", "drone_training", "satellite_training"),
                         path = CLASSES_JSON) {
  scheme <- match.arg(scheme)
  j <- read_class_scheme(path)

  df <- do.call(rbind, lapply(j$classes, function(x) {
    data.frame(
      Type        = as.integer(x$code),
      Class       = x$name,
      Label       = x$label_md,
      Description = x$description,
      stringsAsFactors = FALSE
    )
  }))

  if (scheme != "field") {
    keep <- as.integer(unlist(j$schemes[[scheme]]$codes))
    df <- df[df$Type %in% keep, , drop = FALSE]
  }
  rownames(df) <- NULL
  df
}


#' Recode field classes to the simple four-class scheme
#'
#' Codes 5 and 7 collapse into 6, which then means "other woody", NOT
#' Rhigozum trichotomum. Anything outside the mapping becomes NA.
#'
#' @param x integer vector of class codes
#' @param path location of classes.json
#' @return integer vector of simple-scheme codes
recode_simple <- function(x, path = CLASSES_JSON) {
  m <- read_class_scheme(path)$schemes$simple$mapping
  out <- unname(unlist(m)[as.character(x)])
  as.integer(out)
}


#' Presentation labels for a set of codes
#'
#' Use these everywhere in figures and tables so binomials stay italicised and
#' the "Rhigosum" misspelling Reviewer 1 flagged cannot recur.
#'
#' @param x integer vector of class codes
#' @param simple if TRUE, label under the simple scheme instead
#' @param path location of classes.json
class_labels <- function(x, simple = FALSE, path = CLASSES_JSON) {
  j <- read_class_scheme(path)
  if (simple) {
    lab <- unlist(j$schemes$simple$labels)
    return(unname(lab[as.character(x)]))
  }
  lab <- vapply(j$classes, function(z) z$label_md, character(1))
  names(lab) <- vapply(j$classes, function(z) as.character(z$code), character(1))
  unname(lab[as.character(x)])
}


#' Guard against the superseded xlsx lookup
#'
#' The xlsx on main gained a header row in commit 567696e. The legacy call
#' `read_xlsx(lookup_file, col_names = c("Type", "Class", "Description"))`
#' declares that the file has NO header, so it now ingests the header as data
#' row 1 and coerces Type to character, silently breaking every downstream join.
#'
#' Call this anywhere the old path might still be read.
#'
#' @param path the xlsx path being read
assert_not_legacy_lookup <- function(path) {
  if (grepl("Veg_type_lookup_list\\.xlsx$", path)) {
    stop("Veg_type_lookup_list.xlsx is superseded by ", CLASSES_JSON, ".\n",
         "  Reading it with col_names = c('Type','Class','Description') now\n",
         "  ingests the header row as data and makes Type a character vector.\n",
         "  Use class_lookup() instead.", call. = FALSE)
  }
  invisible(TRUE)
}
