test_that("load_validation_schema works", {
  skip_if_not_installed("OmicsMLRepoCuration")
  schema <- load_validation_schema()
  expect_type(schema, "list")
})

test_that("find_metadata_files finds TSV files", {
  skip_if_not(dir.exists("../../inst/curated"), "inst/curated directory not found")
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

test_that("uncurated_columns identifies prefixed columns only", {
  d <- data.frame(
    study_name = "S", age = 1L,
    uncurated_metadata = "a<;>b",
    uncurated_DAS28 = 3.1,
    check.names = FALSE
  )

  expect_setequal(
    uncurated_columns(d),
    c("uncurated_metadata", "uncurated_DAS28")
  )
  expect_length(uncurated_columns(d[, c("study_name", "age")]), 0L)
})

test_that("uncurated_columns does not match partial or infix prefixes", {
  d <- data.frame(
    not_uncurated_x = 1L, uncuratedfoo = 2L, uncurated_ok = 3L,
    check.names = FALSE
  )

  expect_equal(uncurated_columns(d), "uncurated_ok")
})
