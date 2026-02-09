#!/usr/bin/env Rscript

# Load required libraries
library(curatedMetagenomicDataCuration)
library(OmicsMLRepoCuration)
library(jsonlite)

main <- function() {
  cat("🔬 Starting metadata validation workflow\n\n")

  # 1. Load schema
  cat("Loading schema from OmicsMLRepoCuration...\n")
  schema <- load_validation_schema()
  cat("✓ Schema loaded successfully\n\n")

  # 2. Find metadata files
  cat("Finding metadata files...\n")
  metadata_files <- find_metadata_files()
  cat("Found", length(metadata_files), "metadata files to validate\n\n")

  # 3. Validate all files
  cat("Validating files...\n")
  results <- lapply(metadata_files, function(file) {
    validate_single_study(file, schema)
  })
  cat("✓ Validation complete\n\n")

  # 4. Aggregate results
  validation_summary <- aggregate_validation_results(results)

  # 5. Collect statistics
  cat("Collecting metadata statistics...\n")
  stats <- collect_metadata_statistics(results)
  cat("✓ Statistics collected\n\n")

  # 6. Generate console report
  generate_console_report(validation_summary)

  # 7. Save outputs for GitHub Actions
  gh_output <- Sys.getenv("GITHUB_OUTPUT")
  if (nzchar(gh_output)) {
    cat("total_files=", validation_summary$total_files, "\n", sep = "",
        file = gh_output, append = TRUE)
    cat("total_errors=", validation_summary$total_errors, "\n", sep = "",
        file = gh_output, append = TRUE)
    cat("total_warnings=", validation_summary$total_warnings, "\n", sep = "",
        file = gh_output, append = TRUE)
    cat("files_with_issues=", validation_summary$files_with_issues, "\n", sep = "",
        file = gh_output, append = TRUE)
  }

  # 8. Generate Markdown summary
  cat("Generating markdown summary...\n")
  summary_md <- generate_markdown_summary(validation_summary, stats)
  writeLines(summary_md, "validation_summary.md")
  cat("✓ Markdown summary saved to validation_summary.md\n")

  # 9. Generate JSON report
  cat("Generating JSON report...\n")

  # Get workflow metadata from environment
  metadata <- list(
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    trigger = Sys.getenv("GITHUB_EVENT_NAME", "unknown"),
    branch = Sys.getenv("GITHUB_REF_NAME", "unknown"),
    schema_commit = Sys.getenv("SCHEMA_COMMIT", "unknown"),
    schema_commit_short = Sys.getenv("SCHEMA_COMMIT_SHORT", "unknown"),
    run_id = Sys.getenv("GITHUB_RUN_ID", "unknown")
  )

  json_report <- generate_json_report(validation_summary, stats, metadata)
  writeLines(json_report, "docs/validation_results.json")
  cat("✓ JSON report saved to docs/validation_results.json\n\n")

  # 10. Exit with appropriate status
  if (!validation_summary$all_valid) {
    cat("\n🚨 VALIDATION FAILED!\n\n")
    cat("Please update your metadata to match the latest schema from:\n")
    cat("https://github.com/shbrief/OmicsMLRepoCuration\n\n")
    stop("Metadata validation failed! See errors above.")
  }

  cat("\n✅ All metadata files are valid against the latest schema!\n")
}

# Run main function
main()
