test_that("query_ols returns character results for small ontology", {
  skip_on_cran()
  skip_if_not_installed("httr2")

  result <- tryCatch(
    query_ols("NCIT:C16731", "children", size_threshold = 1000),
    error = function(e) e
  )

  if (inherits(result, "error") || is.null(result)) {
    skip("OLS endpoint unavailable or returned no results")
  }

  expect_type(result, "character")
  expect_true(length(result) > 0)
})

test_that("query_ols returns NULL when threshold is exceeded", {
  skip_on_cran()
  skip_if_not_installed("httr2")

  result <- tryCatch(
    query_ols("NCIT:C7057;EFO:0000408", "descendant", size_threshold = 1),
    error = function(e) e
  )

  if (inherits(result, "error")) {
    skip("OLS endpoint unavailable for threshold test")
  }

  expect_null(result)
})

test_that("get_curated_values returns values for a known metadata column", {
  vals <- get_curated_values(column_name = "sex")
  expect_type(vals, "character")
  expect_true(length(vals) > 0)
})

test_that("get_curated_values returns empty vector for missing metadata file", {
  vals <- expect_warning(
    get_curated_values(metadata_file = tempfile(fileext = ".csv"), column_name = "sex"),
    "Metadata file not found"
  )
  expect_type(vals, "character")
  expect_length(vals, 0)
})

test_that("generate_validation_lists builds enum lists from dictionary", {
  lists <- generate_validation_lists(
    dict_file = system.file("extdata", "cMD_data_dictionary.csv", package = "curatedMetagenomicDataCuration"),
    metadata_file = system.file("extdata", "cMD_curated_metadata_release.csv", package = "curatedMetagenomicDataCuration"),
    ols_size_threshold = 1
  )

  expect_type(lists, "list")
  expect_true("body_site" %in% names(lists))
  expect_true(length(lists$body_site) > 0)
})

test_that("generate_validation_lists includes binary corpus.type columns", {
  lists <- generate_validation_lists(
    dict_file = system.file("extdata", "cMD_data_dictionary.csv", package = "curatedMetagenomicDataCuration"),
    metadata_file = system.file("extdata", "cMD_curated_metadata_release.csv", package = "curatedMetagenomicDataCuration"),
    ols_size_threshold = 1
  )

  expect_true("antibiotics_current_use" %in% names(lists))
  expect_equal(sort(lists$antibiotics_current_use), c("No", "Yes"))
})

test_that("generate_data_entry_excel writes workbook with deterministic local inputs", {
  skip_if_not_installed("openxlsx2")

  dict_file <- tempfile(fileext = ".csv")
  metadata_file <- tempfile(fileext = ".csv")
  output_file <- tempfile(fileext = ".xlsx")

  writeLines(
    c(
      "col.name,col.class,unique,required,multiplevalues,description,allowedvalues,static.enum,dynamic.enum,dynamic.enum.property,delimiter,separator,corpus.type,display.group,display.order",
      "study_name,character,non-unique,required,FALSE,Study name,,NA,NA,NA,NA,NA,any,Identifiers,1",
      "sample_id,character,non-unique,required,FALSE,Sample id,,NA,NA,NA,NA,NA,any,Identifiers,2",
      "subject_id,character,non-unique,required,FALSE,Subject id,,NA,NA,NA,NA,NA,any,Identifiers,3",
      "target_condition,character,non-unique,required,FALSE,Condition,Case|Control,NCIT:C49152|NCIT:C142703,NA,NA,NA,NA,static_enum,Clinical,4",
      "sex,character,non-unique,optional,FALSE,Sex,Female|Male,NA,NA,NA,NA,NA,static_enum,Clinical,5"
    ),
    con = dict_file
  )

  writeLines(
    c(
      "study_name,sample_id,subject_id,target_condition,sex",
      "DemoStudy,S1,U1,Case,Female",
      "DemoStudy,S2,U2,Control,Male"
    ),
    con = metadata_file
  )

  result <- suppressWarnings(
    generate_data_entry_excel(
      dict_file = dict_file,
      metadata_file = metadata_file,
      output_file = output_file,
      prefill_metadata = NULL
    )
  )

  expect_true(file.exists(output_file))
  expect_type(result, "list")
  expect_equal(result$output_file, output_file)
  expect_equal(result$num_columns, 5)
  expect_true("target_condition" %in% result$validation_columns)
})

test_that("generate_data_entry_excel accepts data.frame prefill metadata", {
  skip_if_not_installed("openxlsx2")

  dict_file <- tempfile(fileext = ".csv")
  metadata_file <- tempfile(fileext = ".csv")
  output_file <- tempfile(fileext = ".xlsx")

  writeLines(
    c(
      "col.name,col.class,unique,required,multiplevalues,description,allowedvalues,static.enum,dynamic.enum,dynamic.enum.property,delimiter,separator,corpus.type,display.group,display.order",
      "study_name,character,non-unique,required,FALSE,Study name,,NA,NA,NA,NA,NA,any,Identifiers,1",
      "sample_id,character,non-unique,required,FALSE,Sample id,,NA,NA,NA,NA,NA,any,Identifiers,2",
      "subject_id,character,non-unique,required,FALSE,Subject id,,NA,NA,NA,NA,NA,any,Identifiers,3",
      "target_condition,character,non-unique,optional,TRUE,Condition,Case|Control,NCIT:C49152|NCIT:C142703,NA,NA,;,NA,static_enum,Clinical,4"
    ),
    con = dict_file
  )

  writeLines(
    c(
      "study_name,sample_id,subject_id,target_condition",
      "DemoStudy,S1,U1,Case",
      "DemoStudy,S2,U2,Control"
    ),
    con = metadata_file
  )

  prefill_df <- data.frame(
    study_name = "PrefilledStudy",
    sample_id = "SAMP001",
    subject_id = "SUBJ001",
    target_condition = "Case;Control",
    stringsAsFactors = FALSE
  )

  result <- suppressWarnings(
    generate_data_entry_excel(
      dict_file = dict_file,
      metadata_file = metadata_file,
      output_file = output_file,
      prefill_metadata = prefill_df
    )
  )

  expect_true(file.exists(output_file))
  expect_type(result, "list")
  expect_equal(result$output_file, output_file)
})
