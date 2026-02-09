test_that("load_validation_schema works", {
  skip_if_not_installed("OmicsMLRepoCuration")
  schema <- load_validation_schema()
  expect_type(schema, "list")
})

test_that("find_metadata_files finds TSV files", {
  skip_if_not(dir.exists("../../inst/harmonized"), "inst/harmonized directory not found")
  files <- find_metadata_files()
  expect_true(length(files) > 0)
  expect_true(all(grepl("_sample\\.tsv$", files)))
})

test_that("validate_single_study handles errors gracefully", {
  skip_if_not_installed("OmicsMLRepoCuration")
  schema <- load_validation_schema()
  result <- validate_single_study("nonexistent.tsv", schema)
  expect_false(result$success)
  expect_true(length(result$errors) > 0)
})

test_that("aggregate_validation_results counts correctly", {
  # Mock validation results
  results <- list(
    list(errors = c("error1"), warnings = character(0)),
    list(errors = character(0), warnings = c("warning1", "warning2")),
    list(errors = character(0), warnings = character(0))
  )

  summary <- aggregate_validation_results(results)

  expect_equal(summary$total_files, 3)
  expect_equal(summary$total_errors, 1)
  expect_equal(summary$total_warnings, 2)
  expect_equal(summary$files_with_issues, 2)
  expect_false(summary$all_valid)
  expect_equal(summary$status, "FAIL")
})

test_that("aggregate_validation_results handles all valid", {
  # Mock validation results with no errors/warnings
  results <- list(
    list(errors = character(0), warnings = character(0)),
    list(errors = character(0), warnings = character(0))
  )

  summary <- aggregate_validation_results(results)

  expect_true(summary$all_valid)
  expect_equal(summary$status, "PASS")
  expect_equal(summary$files_with_issues, 0)
})
