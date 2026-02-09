#' @title Generate Console Report
#' @description Print validation results to console
#' @param validation_summary Aggregated validation results
#' @export
generate_console_report <- function(validation_summary) {
  studies <- validation_summary$studies_with_issues
  total_files <- validation_summary$total_files

  if (length(studies) > 0) {
    cat("\n", rep("=", 70), "\n", sep = "")
    cat("STUDIES WITH ERRORS/WARNINGS (", length(studies), " of ", total_files, ")\n", sep = "")
    cat(rep("=", 70), "\n", sep = "")

    for (study in studies) {
      cat("\n", rep("-", 70), "\n", sep = "")
      cat("Study:", study$study_name, "\n")
      cat("File:", study$file, "\n")
      if (!is.na(study$rows)) {
        cat("Rows:", study$rows, " | Columns:", study$cols, "\n")
      }
      cat(rep("-", 70), "\n", sep = "")

      if (length(study$errors) > 0) {
        cat("❌ ERRORS (", length(study$errors), "):\n", sep = "")
        for (i in seq_along(study$errors)) {
          cat("  ", i, ". ", study$errors[i], "\n", sep = "")
        }
      }

      if (length(study$warnings) > 0) {
        cat("⚠️  WARNINGS (", length(study$warnings), "):\n", sep = "")
        for (i in seq_along(study$warnings)) {
          cat("  ", i, ". ", study$warnings[i], "\n", sep = "")
        }
      }
    }
  } else {
    cat("\n✅ All studies passed validation without errors or warnings!\n")
  }

  # Print summary
  cat("\n", rep("=", 70), "\n", sep = "")
  cat("VALIDATION SUMMARY\n")
  cat(rep("=", 70), "\n", sep = "")
  cat("Total files validated:", validation_summary$total_files, "\n")
  cat("Total errors:", validation_summary$total_errors, "\n")
  cat("Total warnings:", validation_summary$total_warnings, "\n")
  cat("Status:", ifelse(validation_summary$all_valid, "✅ PASS", "❌ FAIL"), "\n")
  cat(rep("=", 70), "\n\n", sep = "")
}

#' @title Generate Markdown Summary
#' @description Create markdown summary for GitHub Actions
#' @param validation_summary Aggregated validation results
#' @param stats Metadata statistics
#' @return Character vector of markdown lines
#' @export
generate_markdown_summary <- function(validation_summary, stats) {
  summary_lines <- c(
    "# 🔬 Metadata Validation Summary",
    "",
    paste0("**Status:** ", ifelse(validation_summary$all_valid, "✅ PASS", "❌ FAIL")),
    paste0("**Date:** ", format(Sys.time(), "%Y-%m-%d %H:%M:%S UTC", tz = "UTC")),
    "",
    "## Validation Results",
    "",
    "| Metric | Value |",
    "|--------|------:|",
    paste0("| Files validated | ", validation_summary$total_files, " |"),
    paste0("| Files with issues | ", validation_summary$files_with_issues, " |"),
    paste0("| Total errors | ", validation_summary$total_errors, " |"),
    paste0("| Total warnings | ", validation_summary$total_warnings, " |"),
    "",
    "## Harmonized Metadata Statistics",
    "",
    "| Metric | Value |",
    "|--------|------:|",
    paste0("| Total studies | ", validation_summary$total_files, " |"),
    paste0("| Total samples | ", stats$total_samples, " |"),
    paste0("| Distinct countries | ", length(stats$country), " |"),
    paste0("| Distinct diseases | ", length(stats$disease), " |"),
    paste0("| Distinct body sites | ", length(stats$body_site), " |"),
    ""
  )

  # Add distribution summaries
  format_top <- function(counts, title, n = 5) {
    if (length(counts) == 0) return(character(0))
    counts <- sort(counts, decreasing = TRUE)
    top <- head(counts, n)
    c(
      paste0("### ", title, " (top ", min(n, length(top)), ")"),
      "",
      "| Name | Count |",
      "|------|------:|",
      vapply(names(top), function(nm) {
        paste0("| ", nm, " | ", top[[nm]], " |")
      }, character(1)),
      ""
    )
  }

  summary_lines <- c(summary_lines,
    format_top(stats$body_site, "🧬 Body Site"),
    format_top(stats$disease, "🏥 Disease"),
    format_top(stats$country, "🌍 Country"),
    format_top(stats$sex, "👤 Sex"),
    format_top(stats$age_group, "📅 Age Group")
  )

  # Add studies with issues
  if (length(validation_summary$studies_with_issues) > 0) {
    summary_lines <- c(summary_lines,
      "## Studies with Issues",
      "",
      "<details>",
      paste0("<summary>⚠️ ", length(validation_summary$studies_with_issues),
             " studies have errors or warnings (click to expand)</summary>"),
      ""
    )

    for (study in validation_summary$studies_with_issues) {
      summary_lines <- c(summary_lines, paste0("#### ", study$study_name))

      if (length(study$errors) > 0) {
        for (err in study$errors) {
          summary_lines <- c(summary_lines, paste0("- ❌ ", err))
        }
      }

      if (length(study$warnings) > 0) {
        for (wrn in study$warnings) {
          summary_lines <- c(summary_lines, paste0("- ⚠️ ", wrn))
        }
      }
      summary_lines <- c(summary_lines, "")
    }
    summary_lines <- c(summary_lines, "</details>", "")
  }

  summary_lines
}

#' @title Generate JSON Report
#' @description Create JSON report for GitHub Pages
#' @param validation_summary Aggregated validation results
#' @param stats Metadata statistics
#' @param metadata Workflow metadata (trigger, commit, etc.)
#' @return JSON string
#' @importFrom jsonlite toJSON
#' @export
generate_json_report <- function(validation_summary, stats, metadata = list()) {
  # Convert stats to distribution format
  distributions <- list(
    age_group = build_distribution(stats$age_group),
    body_site = build_distribution(stats$body_site),
    published_year = build_distribution(stats$published_year),
    country = build_distribution(stats$country),
    ancestry = build_distribution(stats$ancestry),
    disease = build_distribution(stats$disease),
    sex = build_distribution(stats$sex)
  )

  # Format studies with issues for JSON
  studies_json <- lapply(validation_summary$studies_with_issues, function(s) {
    list(
      name = s$study_name,
      file = s$file,
      rows = s$rows,
      cols = s$cols,
      errors = as.list(s$errors),
      warnings = as.list(s$warnings)
    )
  })

  json_report <- list(
    status = validation_summary$status,
    summary = list(
      total_files = validation_summary$total_files,
      total_errors = validation_summary$total_errors,
      total_warnings = validation_summary$total_warnings,
      files_with_issues = validation_summary$files_with_issues
    ),
    stats = list(
      total_studies = validation_summary$total_files,
      total_samples = as.integer(stats$total_samples),
      distributions = distributions
    ),
    metadata = metadata,
    studies_with_issues = studies_json
  )

  jsonlite::toJSON(json_report, auto_unbox = TRUE, pretty = TRUE)
}
