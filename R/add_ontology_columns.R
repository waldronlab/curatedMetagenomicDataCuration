#' Resolve the data dictionary CSV, local dev tree first
#'
#' Mirrors the local/installed fallback used by \code{load_validation_schema}
#' in \code{workflow_validation_helpers.R}, but returns the raw table instead
#' of a validation schema.
#' @noRd
.load_ontology_dictionary <- function(package = "curatedMetagenomicDataCuration") {
  dict_file <- ""
  local_dir <- file.path("inst", "extdata")
  if (dir.exists(local_dir)) {
    hits <- list.files(local_dir, pattern = "_data_dictionary\\.csv$", full.names = TRUE)
    if (length(hits) > 0) dict_file <- hits[1]
  }
  if (!nzchar(dict_file) || !file.exists(dict_file)) {
    pkg_dir <- system.file("extdata", package = package)
    hits <- list.files(pkg_dir, pattern = "_data_dictionary\\.csv$", full.names = TRUE)
    if (length(hits) == 0)
      stop("No *_data_dictionary.csv found in package '", package, "'")
    dict_file <- hits[1]
  }
  dict <- read.csv(dict_file, stringsAsFactors = FALSE)
  if (ncol(dict) < 2 || !"col.name" %in% names(dict)) {
    stop("Data dictionary '", dict_file, "' parsed into ", ncol(dict),
         " column(s) without a 'col.name' column. The file is likely not ",
         "comma-delimited.")
  }
  dict
}

#' Build the label -> ontology term ID map for one dictionary row
#'
#' Only \code{static_enum} fields with matching-length \code{allowedvalues}
#' and \code{static.enum} carry a reliable, offline label-to-ID mapping.
#' \code{dynamic_enum} fields (e.g. disease, target_condition) have no such
#' mapping available locally; resolving them requires a live ontology lookup,
#' which is out of scope here.
#' @noRd
.build_static_enum_map <- function(field, corpus_type, allowedvalues, static_enum,
                                    verbose = TRUE) {
  corpus_type <- as.character(corpus_type)
  allowedvalues <- as.character(allowedvalues)
  static_enum <- as.character(static_enum)

  if (is.na(corpus_type) || corpus_type != "static_enum") {
    if (verbose && !is.na(corpus_type) && grepl("dynamic_enum", corpus_type, fixed = TRUE)) {
      message(field, ": dynamic_enum-backed, no offline ontology mapping available - skipped")
    }
    return(NULL)
  }
  if (is.na(allowedvalues) || is.na(static_enum)) {
    if (verbose) message(field, ": static.enum mapping missing in dictionary - skipped")
    return(NULL)
  }
  labels <- trimws(strsplit(allowedvalues, "|", fixed = TRUE)[[1]])
  ids <- trimws(strsplit(static_enum, "|", fixed = TRUE)[[1]])
  if (length(labels) != length(ids) || any(!nzchar(labels)) || any(!nzchar(ids))) {
    if (verbose) message(field, ": allowedvalues/static.enum mapping is malformed - skipped")
    return(NULL)
  }
  stats::setNames(ids, labels)
}

#' Map a metadata column's values to ontology term IDs
#'
#' For multi-value cells, splits on the field's own delimiter (not every
#' multi-value field uses the same one - e.g. most use ";" but
#' feces_phenotype and probing_pocket_depth use "<;>"), maps each piece, and
#' rejoins with that same delimiter.
#' @noRd
.fill_ontology_column <- function(values, map, delimiter, multiplevalues) {
  values <- trimws(as.character(values))
  if (isTRUE(multiplevalues) && !is.na(delimiter) && nzchar(delimiter)) {
    vapply(values, function(x) {
      if (is.na(x) || !nzchar(x)) return(NA_character_)
      pieces <- trimws(strsplit(x, delimiter, fixed = TRUE)[[1]])
      mapped <- unname(map[pieces])
      if (any(is.na(mapped))) return(NA_character_)
      paste(mapped, collapse = delimiter)
    }, character(1), USE.NAMES = FALSE)
  } else {
    unname(map[values])
  }
}

.required_dict_columns <- c("col.name", "corpus.type", "allowedvalues",
                             "static.enum", "multiplevalues", "delimiter")

#' Add ontology term ID columns to a curated metadata table
#'
#' For every metadata column backed by a \code{static_enum} field in the data
#' dictionary (e.g. \code{sex}, \code{body_site}, \code{age_group}), derives
#' the matching \code{<column>_ontology_term_id} column from the dictionary's
#' \code{allowedvalues}/\code{static.enum} mapping, so curators no longer have
#' to hand-type ontology codes for these fields.
#'
#' Fields backed by a \code{dynamic_enum} (e.g. \code{disease},
#' \code{target_condition}) have no offline label-to-ID mapping in this
#' package and are skipped with a message; resolving them would require a
#' live ontology lookup service call, which this function does not make.
#'
#' @param data A data frame of curated metadata, such as read from a
#'   \code{*_sample.tsv} file.
#' @param dictionary Optional data dictionary data frame (same columns as
#'   \code{cMD_data_dictionary.csv}). If \code{NULL} (the default), it is
#'   loaded from the local \code{inst/extdata} tree or the installed package.
#' @param package Package whose \code{inst/extdata} contains the data
#'   dictionary, used when \code{dictionary} is \code{NULL}.
#' @param overwrite Logical. If \code{FALSE} (the default), existing non-blank
#'   \code{_ontology_term_id} values are left untouched and only blank/NA
#'   cells are filled. If \code{TRUE}, every derivable cell is recomputed.
#' @param verbose Logical. If \code{TRUE} (the default), print a message when
#'   a field can't be mapped offline (dynamic_enum, or a malformed
#'   static.enum/allowedvalues pairing), and for every other field how many
#'   of its values could be mapped to an ontology term ID (this reflects
#'   what the dictionary can derive, not how many cells were actually
#'   changed - with \code{overwrite = FALSE} an already-filled cell counts
#'   as mapped even though it was left untouched).
#'
#' @return \code{data} as a plain data frame (a tibble or data.table input is
#'   coerced), with \code{_ontology_term_id} columns added (immediately after
#'   their source column) or filled in.
#'
#' @examples
#' \dontrun{
#' sample_data <- read.delim("inst/curated/AsnicarF_2017/AsnicarF_2017_sample.tsv",
#'                            stringsAsFactors = FALSE)
#' add_ontology_columns(sample_data)
#' }
#' @export
add_ontology_columns <- function(data,
                                  dictionary = NULL,
                                  package = "curatedMetagenomicDataCuration",
                                  overwrite = FALSE,
                                  verbose = TRUE) {
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  dict <- if (is.null(dictionary)) .load_ontology_dictionary(package) else dictionary

  missing_cols <- setdiff(.required_dict_columns, names(dict))
  if (length(missing_cols) > 0) {
    stop("Data dictionary is missing required column(s): ",
         paste(missing_cols, collapse = ", "))
  }

  fields <- intersect(dict$col.name, names(data))

  for (field in fields) {
    row <- dict[which(dict$col.name == field)[1], , drop = FALSE]
    map <- .build_static_enum_map(field, row$corpus.type, row$allowedvalues,
                                   row$static.enum, verbose)
    if (is.null(map)) next

    multiplevalues <- isTRUE(as.logical(as.character(row$multiplevalues)))
    delimiter <- as.character(row$delimiter)
    derived <- .fill_ontology_column(data[[field]], map, delimiter, multiplevalues)
    n_mapped <- sum(!is.na(derived) & nzchar(derived))

    oid_col <- paste0(field, "_ontology_term_id")
    if (oid_col %in% names(data)) {
      existing <- as.character(data[[oid_col]])
      if (overwrite) {
        keep <- is.na(derived)
        derived[keep] <- existing[keep]
        data[[oid_col]] <- derived
      } else {
        blank <- is.na(existing) | !nzchar(trimws(existing))
        existing[blank] <- derived[blank]
        data[[oid_col]] <- existing
      }
    } else {
      insert_after <- match(field, names(data))
      data[[oid_col]] <- derived
      col_order <- append(seq_len(ncol(data) - 1L), ncol(data), after = insert_after)
      data <- data[col_order]
    }

    if (verbose) {
      n_values <- sum(!is.na(data[[field]]) & nzchar(trimws(as.character(data[[field]]))))
      message(field, ": mapped ", n_mapped, "/", n_values, " value(s)")
    }
  }

  data
}
