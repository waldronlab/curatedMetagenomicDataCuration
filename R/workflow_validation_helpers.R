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
