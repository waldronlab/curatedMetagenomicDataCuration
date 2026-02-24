#' @title Load Validation Schema
#' @description Load the metadata schema from OmicsMLRepoCuration package
#' @return List containing the loaded schema
#' @export
load_validation_schema <- function() {
  schema_file <- system.file("schema", "cmd_data_dictionary.yaml",
                            package = "OmicsMLRepoCuration")
  OmicsMLRepoCuration::load_metadata_schema(schema_file)
}

#' @title Find Metadata Files
#' @description Find all *_sample.tsv files in harmonized directory
#' @param path Base path to search (default: "inst/harmonized/")
#' @return Character vector of file paths
#' @export
find_metadata_files <- function(path = "inst/harmonized/") {
  list.files(path, pattern = "_sample\\.tsv$",
             full.names = TRUE, recursive = TRUE)
}

#' @title Validate Single Study
#' @description Validate a single metadata file against schema
#' @param file Path to TSV file
#' @param schema Validation schema object
#' @return List with validation results and metadata
#' @export
validate_single_study <- function(file, schema) {
  study_name <- basename(dirname(file))

  tryCatch({
    data <- read.delim(file, stringsAsFactors = FALSE, check.names = FALSE)
    result <- OmicsMLRepoCuration::validate_data_against_schema(data, schema)

    list(
      study_name = study_name,
      file = file,
      rows = nrow(data),
      cols = ncol(data),
      data = data,
      errors = result$errors,
      warnings = result$warnings,
      success = TRUE
    )
  }, error = function(e) {
    list(
      study_name = study_name,
      file = file,
      rows = NA,
      cols = NA,
      data = NULL,
      errors = paste("VALIDATION ERROR:", conditionMessage(e)),
      warnings = character(0),
      success = FALSE
    )
  })
}

#' Validate a single study's harmonized metadata
#'
#' A user-friendly wrapper around the internal validation pipeline.
#' Accepts a study name (e.g. \code{"LeeKA_2022"}) or a direct path to a
#' \code{*_sample.tsv} file, loads the schema automatically, and prints a
#' readable summary to the console.
#'
#' @param study A study name (e.g. \code{"LeeKA_2022"}) to look up in the
#'   package's \code{inst/harmonized/} directory, or a full path to a
#'   \code{*_sample.tsv} file.
#' @param verbose Logical. If \code{TRUE} (default), print errors and warnings
#'   in full after the summary header.
#'
#' @return Invisibly returns a list with components:
#'   \describe{
#'     \item{study_name}{Study identifier derived from the directory name.}
#'     \item{file}{Resolved path to the TSV file.}
#'     \item{rows}{Number of samples (rows) in the file.}
#'     \item{cols}{Number of columns in the file.}
#'     \item{errors}{Character vector of validation errors (empty if none).}
#'     \item{warnings}{Character vector of validation warnings (empty if none).}
#'     \item{passed}{Logical; \code{TRUE} when there are no errors.}
#'   }
#'
#' @examples
#' \dontrun{
#' # By study name (looks up inst/harmonized/LeeKA_2022/LeeKA_2022_sample.tsv)
#' validateStudy("LeeKA_2022")
#'
#' # By explicit file path
#' validateStudy("path/to/MyStudy_2024_sample.tsv")
#' }
#'
#' @export
validateStudy <- function(study, verbose = TRUE) {
  # ── Resolve file path ────────────────────────────────────────────────────────
  if (file.exists(study)) {
    file <- study
  } else {
    # Treat `study` as a study name; look it up in inst/harmonized/
    harmonized_dir <- system.file("harmonized", package = "curatedMetagenomicDataCuration")
    if (!nzchar(harmonized_dir)) {
      # Fall back to working-directory layout (development use)
      harmonized_dir <- file.path("inst", "harmonized")
    }
    file <- file.path(harmonized_dir, study, paste0(study, "_sample.tsv"))
    if (!file.exists(file)) {
      stop(
        "Could not find metadata file for study '", study, "'.\n",
        "  Looked at: ", file, "\n",
        "  Pass a full file path or check that the study name is correct."
      )
    }
  }

  # ── Load schema & validate ───────────────────────────────────────────────────
  schema <- load_validation_schema()
  res    <- validate_single_study(file, schema)

  n_errors   <- length(res$errors)
  n_warnings <- length(res$warnings)
  passed     <- res$success && n_errors == 0L

  # ── Print summary ────────────────────────────────────────────────────────────
  status_icon <- if (passed) "[PASS]" else "[FAIL]"
  cat(rep("-", 60), "\n", sep = "")
  cat(status_icon, res$study_name, "\n")
  if (!is.na(res$rows))
    cat("  File   :", res$file, "\n",
        " Samples :", res$rows, "rows x", res$cols, "columns\n")

  if (!res$success) {
    cat("  ERROR  : could not read file\n")
  } else if (passed && n_warnings == 0L) {
    cat("  Result : no errors or warnings\n")
  } else {
    if (n_errors > 0L)
      cat("  Errors  :", n_errors, "\n")
    if (n_warnings > 0L)
      cat("  Warnings:", n_warnings, "\n")
  }
  cat(rep("-", 60), "\n", sep = "")

  if (verbose) {
    if (n_errors > 0L) {
      cat("Errors:\n")
      for (e in res$errors) cat("  ERROR:", e, "\n")
    }
    if (n_warnings > 0L) {
      cat("Warnings:\n")
      for (w in res$warnings) cat("  WARN :", w, "\n")
    }
  }

  invisible(list(
    study_name = res$study_name,
    file       = res$file,
    rows       = res$rows,
    cols       = res$cols,
    errors     = res$errors,
    warnings   = res$warnings,
    passed     = passed
  ))
}

#' @title Aggregate Validation Results
#' @description Combine validation results from all studies
#' @param results List of validation results
#' @return List with aggregated statistics
#' @export
aggregate_validation_results <- function(results) {
  total_errors <- sum(sapply(results, function(r) length(r$errors)))
  total_warnings <- sum(sapply(results, function(r) length(r$warnings)))
  all_valid <- all(sapply(results, function(r) length(r$errors) == 0))

  studies_with_issues <- results[sapply(results, function(r)
    length(r$errors) > 0 || length(r$warnings) > 0)]

  list(
    total_files = length(results),
    total_errors = total_errors,
    total_warnings = total_warnings,
    files_with_issues = length(studies_with_issues),
    all_valid = all_valid,
    status = ifelse(all_valid, "PASS", "FAIL"),
    studies_with_issues = studies_with_issues
  )
}
