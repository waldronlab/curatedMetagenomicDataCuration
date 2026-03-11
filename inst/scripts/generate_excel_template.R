#!/usr/bin/env Rscript

#' Generate Data Entry Excel Template
#'
#' This script generates a dynamic Excel template for curatedMetagenomicData
#' curation based on the data dictionary and existing curated metadata.
#'
#' Usage:
#'   Rscript inst/scripts/generate_excel_template.R
#'
#' Or from R:
#'   source("inst/scripts/generate_excel_template.R")

# Load required packages
required_packages <- c("httr2", "readr", "openxlsx2")
missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]

if (length(missing_packages) > 0) {
  message("Installing missing packages: ", paste(missing_packages, collapse = ", "))
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}

# Load the packages explicitly
library(httr2)
library(readr)
library(openxlsx2)

# Load the package functions
if (file.exists("R/generate_data_entry_excel.R")) {
  source("R/generate_data_entry_excel.R")
} else {
  library(curatedMetagenomicDataCuration)
}

# Set file paths
dict_file <- "inst/extdata/cMD_data_dictionary.csv"
metadata_file <- "inst/extdata/cMD_curated_metadata_release.csv"
output_file <- "inst/extdata/cMD_data_entry_generated.xlsx"

# Check if files exist
if (!file.exists(dict_file)) {
  stop("Data dictionary not found at: ", dict_file)
}

if (!file.exists(metadata_file)) {
  stop("Curated metadata not found at: ", metadata_file)
}

# Generate the Excel file
message("=================================================")
message("Generating Data Entry Excel Template")
message("=================================================")
message("")
message("Input files:")
message("  - Data dictionary: ", dict_file)
message("  - Curated metadata: ", metadata_file)
message("")
message("Output file:")
message("  - ", output_file)
message("")
message("This may take several minutes as we query the Ontology Lookup Service...")
message("")

result <- create_data_entry_excel(
  dict_file = dict_file,
  metadata_file = metadata_file,
  output_file = output_file,
  ols_size_threshold = 1000
)

message("")
message("=================================================")
message("Summary")
message("=================================================")
message("Total columns: ", result$num_columns)
message("Columns with validation: ", result$num_validation_lists)
message("")
message("Validation columns:")
for (col in result$validation_columns) {
  message("  - ", col)
}
message("")
message("Excel file saved to: ", result$output_file)
message("=================================================")
