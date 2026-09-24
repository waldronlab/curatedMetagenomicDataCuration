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

# --- issue #162: *_ontology_term_id columns must validate CURIE format ------

test_that("ONTOLOGY_TERM_ID_PATTERN accepts the canonical colon CURIE and rejects the underscore form", {
  anchored <- paste0("^(", ONTOLOGY_TERM_ID_PATTERN, ")$")

  expect_true(grepl(anchored, "NCIT:C142703"))
  expect_false(grepl(anchored, "NCIT_C142703"))
})

test_that("ONTOLOGY_TERM_ID_PATTERN is namespace-agnostic", {
  anchored <- paste0("^(", ONTOLOGY_TERM_ID_PATTERN, ")$")

  expect_true(all(grepl(anchored, c("UBERON:0001988", "NCBITaxon:1", "MONDO:0000001",
                                     "HANCESTRO:0004", "SNOMED:182922004"))))
  expect_false(grepl(anchored, "UBERON_0001988"))
})

test_dict_oid <- data.frame(
  col.name              = c("sex", "disease", "lifestyle"),
  col.class             = "character",
  unique                = "non-unique",
  required              = "optional",
  multiplevalues        = c(FALSE, TRUE, FALSE),
  description           = "d",
  allowedvalues         = c("Female|Male", NA, "Hunter-gatherer|Agriculturalist"),
  static.enum           = c("NCIT:C16576|NCIT:C20197", NA, NA),
  dynamic.enum          = c(NA, "NCIT:C7057|MONDO:0000001", NA),
  dynamic.enum.property = NA,
  delimiter             = c(NA, ";", NA),
  separator             = NA,
  corpus.type           = c("static_enum", "dynamic_enum|static_enum", "custom_enum"),
  display.order         = 1,
  display.group         = "g",
  stringsAsFactors = FALSE
)

test_that(".add_ontology_id_schema_rows adds a regexp row for a static_enum field", {
  augmented <- .add_ontology_id_schema_rows(test_dict_oid)
  row <- augmented[augmented$col.name == "sex_ontology_term_id", ]

  expect_equal(nrow(row), 1L)
  expect_equal(row$corpus.type, "regexp")
  expect_equal(row$allowedvalues, ONTOLOGY_TERM_ID_PATTERN)
  expect_identical(row$multiplevalues, FALSE)
  expect_true(is.na(row$delimiter))
})

test_that(".add_ontology_id_schema_rows also covers dynamic_enum-backed fields and mirrors their delimiter", {
  augmented <- .add_ontology_id_schema_rows(test_dict_oid)
  row <- augmented[augmented$col.name == "disease_ontology_term_id", ]

  expect_equal(nrow(row), 1L)
  expect_identical(row$multiplevalues, TRUE)
  expect_equal(row$delimiter, ";")
})

test_that(".add_ontology_id_schema_rows skips fields with no ontology mapping", {
  augmented <- .add_ontology_id_schema_rows(test_dict_oid)

  expect_false("lifestyle_ontology_term_id" %in% augmented$col.name)
})

test_that(".add_ontology_id_schema_rows does not duplicate an existing dictionary row", {
  dict_with_row <- rbind(test_dict_oid, test_dict_oid[1, ])
  dict_with_row$col.name[nrow(dict_with_row)] <- "sex_ontology_term_id"
  dict_with_row$corpus.type[nrow(dict_with_row)] <- "regexp"
  dict_with_row$allowedvalues[nrow(dict_with_row)] <- "existing_pattern"

  augmented <- .add_ontology_id_schema_rows(dict_with_row)

  expect_equal(sum(augmented$col.name == "sex_ontology_term_id"), 1L)
  expect_equal(augmented$allowedvalues[augmented$col.name == "sex_ontology_term_id"],
               "existing_pattern")
})

test_that("load_validation_schema's generated schema rejects the underscore CURIE and accepts the colon form", {
  skip_if_not_installed("OmicsMLRepoCuration")
  schema <- load_validation_schema()

  good <- data.frame(control_ontology_term_id = "NCIT:C142703", stringsAsFactors = FALSE)
  bad  <- data.frame(control_ontology_term_id = "NCIT_C142703", stringsAsFactors = FALSE)

  result_good <- OmicsMLRepoCuration::validate_data_against_schema(good, schema)
  result_bad  <- OmicsMLRepoCuration::validate_data_against_schema(bad, schema)

  expect_length(result_good$warnings, 0L)
  expect_true(length(result_bad$warnings) > 0L)
})

test_that("NA ontology_term_id values don't error and don't fail overall validation", {
  skip_if_not_installed("OmicsMLRepoCuration")
  schema <- load_validation_schema()
  data <- data.frame(control_ontology_term_id = c("NCIT:C142703", NA, "NCIT_C49152"),
                      stringsAsFactors = FALSE)

  result <- OmicsMLRepoCuration::validate_data_against_schema(data, schema)

  expect_true(result$valid)
  expect_true(any(grepl("NCIT_C49152", result$warnings)))
})
