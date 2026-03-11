#!/usr/bin/env Rscript

#' Test Excel Generation Functions
#'
#' This script tests the individual components of the Excel generation system
#' before running the full generation process.

# Load required functions
if (file.exists("R/generate_data_entry_excel.R")) {
  source("R/generate_data_entry_excel.R")
} else {
  library(curatedMetagenomicDataCuration)
}

library(httr2)
library(readr)

message("=================================================")
message("Testing Excel Generation Components")
message("=================================================")
message("")

# Test 1: OLS Query for a small ontology
message("Test 1: Query OLS for age_group (small static enum)")
message("-------------------------------------------------")
age_group_terms <- query_ols("NCIT:C16731", "children", size_threshold = 100)
if (!is.null(age_group_terms)) {
  message("Success! Found ", length(age_group_terms), " terms:")
  for (term in head(age_group_terms, 5)) {
    message("  - ", term)
  }
  if (length(age_group_terms) > 5) {
    message("  ... and ", length(age_group_terms) - 5, " more")
  }
} else {
  message("WARNING: OLS query returned NULL (too many terms or error)")
}
message("")

# Test 2: OLS Query for a larger ontology (should return NULL and fall back)
message("Test 2: Query OLS for disease (large dynamic enum)")
message("-------------------------------------------------")
disease_terms <- query_ols("NCIT:C7057;EFO:0000408", "descendant", size_threshold = 100)
if (is.null(disease_terms)) {
  message("Expected behavior: Ontology too large, will use existing values")
} else {
  message("Found ", length(disease_terms), " terms (within threshold)")
}
message("")

# Test 3: Get values from curated metadata
message("Test 3: Get unique values from curated metadata")
message("-------------------------------------------------")
metadata_file <- "inst/extdata/cMD_curated_metadata_release.csv"
if (file.exists(metadata_file)) {
  # Test with a simple column
  sex_values <- get_curated_values(metadata_file, "sex")
  message("sex column unique values: ", paste(head(sex_values, 10), collapse = ", "))

  # Test with a multi-value column
  disease_values <- get_curated_values(metadata_file, "disease")
  message("disease column unique values (first 10): ", paste(head(disease_values, 10), collapse = ", "))
  message("Total unique disease values: ", length(disease_values))
} else {
  message("WARNING: Metadata file not found at ", metadata_file)
}
message("")

# Test 4: Generate validation lists (subset)
message("Test 4: Generate validation lists for a few columns")
message("-------------------------------------------------")
dict_file <- "inst/extdata/cMD_data_dictionary.csv"
if (file.exists(dict_file) && file.exists(metadata_file)) {
  # Read dictionary and test with a few columns
  dict <- read.csv(dict_file, stringsAsFactors = FALSE)

  # Test columns: static, dynamic, and mixed
  test_cols <- c("sex", "body_site", "age_group", "country")

  for (col in test_cols) {
    col_info <- dict[dict$col.name == col, ]
    if (nrow(col_info) > 0) {
      message("  ", col, " (", col_info$corpus.type, ")")

      # Generate validation list for this column
      if (col_info$corpus.type == "static_enum") {
        values <- strsplit(col_info$allowedvalues, "\\|")[[1]]
        message("    Found ", length(values), " values: ", paste(head(values, 3), collapse = ", "), "...")
      } else if (col_info$corpus.type == "dynamic_enum") {
        ols_terms <- query_ols(col_info$dynamic.enum, col_info$dynamic.enum.property, 100)
        if (!is.null(ols_terms)) {
          message("    OLS: ", length(ols_terms), " terms")
        } else {
          curated_vals <- get_curated_values(metadata_file, col)
          message("    Curated: ", length(curated_vals), " values")
        }
      }
    }
  }
} else {
  message("WARNING: Dictionary or metadata file not found")
}
message("")

# Test 5: Check openxlsx2 availability
message("Test 5: Check openxlsx2 package")
message("-------------------------------------------------")
if (requireNamespace("openxlsx2", quietly = TRUE)) {
  message("openxlsx2 is installed and available")
  message("Version: ", as.character(packageVersion("openxlsx2")))
} else {
  message("WARNING: openxlsx2 is not installed")
  message("Install with: install.packages('openxlsx2')")
}
message("")

message("=================================================")
message("Testing Complete")
message("=================================================")
message("")
message("If all tests passed, you can run the full generation with:")
message("  Rscript inst/scripts/generate_excel_template.R")
message("")
message("Or from R:")
message("  source('R/generate_data_entry_excel.R')")
message("  create_data_entry_excel()")
message("")
