test_dict <- data.frame(
  col.name       = c("sex", "smoker", "feces_phenotype", "disease", "tumor_staging_ajcc"),
  corpus.type    = c("static_enum", "static_enum", "static_enum",
                      "dynamic_enum|static_enum", "static_enum"),
  allowedvalues  = c("Female|Male", "Yes|No|Ex-smoker", "Normal|Diarrhea|Constipation",
                      NA, "I|II|III"),
  static.enum    = c("NCIT:C16576|NCIT:C20197", "NCIT:C67147|NCIT:C67148|NCIT:C67149",
                      "NCIT:C41258|NCIT:C34674|NCIT:C37930", "NCIT:C115935", NA),
  multiplevalues = c(FALSE, TRUE, TRUE, TRUE, FALSE),
  delimiter      = c(NA, ";", "<;>", ";", NA),
  stringsAsFactors = FALSE
)

test_that("maps a clean single-value static_enum field", {
  data <- data.frame(sample_id = "S1", sex = "Male", stringsAsFactors = FALSE)
  result <- suppressMessages(add_ontology_columns(data, dictionary = test_dict))

  expect_equal(result$sex_ontology_term_id, "NCIT:C20197")
  expect_equal(match("sex_ontology_term_id", names(result)), match("sex", names(result)) + 1)
})

test_that("splits and rejoins multi-value cells on the field's own delimiter", {
  data <- data.frame(
    smoker = "Yes;Ex-smoker",
    feces_phenotype = "Normal<;>Diarrhea",
    stringsAsFactors = FALSE
  )
  result <- suppressMessages(add_ontology_columns(data, dictionary = test_dict))

  expect_equal(result$smoker_ontology_term_id, "NCIT:C67147;NCIT:C67149")
  expect_equal(result$feces_phenotype_ontology_term_id, "NCIT:C41258<;>NCIT:C34674")
})

test_that("skips dynamic_enum-backed fields without erroring", {
  data <- data.frame(disease = "Healthy", stringsAsFactors = FALSE)
  expect_message(
    result <- add_ontology_columns(data, dictionary = test_dict),
    "dynamic_enum-backed"
  )
  expect_false("disease_ontology_term_id" %in% names(result))
})

test_that("skips fields with missing static.enum mapping", {
  data <- data.frame(tumor_staging_ajcc = "II", stringsAsFactors = FALSE)
  expect_message(
    result <- add_ontology_columns(data, dictionary = test_dict),
    "missing in dictionary"
  )
  expect_false("tumor_staging_ajcc_ontology_term_id" %in% names(result))
})

test_that("does not overwrite existing curator-entered values by default", {
  data <- data.frame(
    sex = c("Male", "Female"),
    sex_ontology_term_id = c("BOGUS:1", NA),
    stringsAsFactors = FALSE
  )
  result <- suppressMessages(add_ontology_columns(data, dictionary = test_dict))

  expect_equal(result$sex_ontology_term_id, c("BOGUS:1", "NCIT:C16576"))
})

test_that("overwrite = TRUE recomputes all derivable cells", {
  data <- data.frame(
    sex = c("Male", "Female"),
    sex_ontology_term_id = c("BOGUS:1", NA),
    stringsAsFactors = FALSE
  )
  result <- suppressMessages(
    add_ontology_columns(data, dictionary = test_dict, overwrite = TRUE)
  )

  expect_equal(result$sex_ontology_term_id, c("NCIT:C20197", "NCIT:C16576"))
})

test_that("unmapped values become NA without erroring", {
  data <- data.frame(sex = c("Male", "Unknown"), stringsAsFactors = FALSE)
  result <- suppressMessages(add_ontology_columns(data, dictionary = test_dict))

  expect_equal(result$sex_ontology_term_id, c("NCIT:C20197", NA_character_))
})

test_that("a multi-value cell with one unmapped piece is left NA, not partially joined", {
  data <- data.frame(smoker = "Yes;Bogus;No", stringsAsFactors = FALSE)
  result <- suppressMessages(add_ontology_columns(data, dictionary = test_dict))

  expect_true(is.na(result$smoker_ontology_term_id))
})

test_that("matching is case-sensitive, not fuzzy", {
  data <- data.frame(sex = "male", stringsAsFactors = FALSE)
  result <- suppressMessages(add_ontology_columns(data, dictionary = test_dict))

  expect_true(is.na(result$sex_ontology_term_id))
})

test_that("leading/trailing whitespace in values still matches", {
  data <- data.frame(sex = "  Male  ", stringsAsFactors = FALSE)
  result <- suppressMessages(add_ontology_columns(data, dictionary = test_dict))

  expect_equal(result$sex_ontology_term_id, "NCIT:C20197")
})

test_that("running twice produces the same result (idempotent)", {
  data <- data.frame(sex = c("Male", "Female"), stringsAsFactors = FALSE)
  once <- suppressMessages(add_ontology_columns(data, dictionary = test_dict))
  twice <- suppressMessages(add_ontology_columns(once, dictionary = test_dict))

  expect_equal(once, twice)
})

test_that("verbose = FALSE prints nothing", {
  data <- data.frame(sex = "Male", disease = "Healthy", stringsAsFactors = FALSE)
  msgs <- capture_messages(add_ontology_columns(data, dictionary = test_dict, verbose = FALSE))

  expect_length(msgs, 0)
})

test_that("the verbose mapped count reflects only what could be derived, not carried-over values", {
  data <- data.frame(
    sex = c("Male", "Unknown", "Unknown", "Unknown", "Unknown"),
    sex_ontology_term_id = c(NA, "X:2", "X:3", "X:4", "X:5"),
    stringsAsFactors = FALSE
  )
  msgs <- capture_messages(
    add_ontology_columns(data, dictionary = test_dict, overwrite = TRUE)
  )

  expect_match(msgs[1], "sex: mapped 1/5 value\\(s\\)")
})

test_that("overwrite = TRUE keeps a curator's value where nothing could be derived", {
  data <- data.frame(
    sex = c("Male", "Unknown"),
    sex_ontology_term_id = c("BOGUS:1", "CURATOR:99"),
    stringsAsFactors = FALSE
  )
  result <- suppressMessages(
    add_ontology_columns(data, dictionary = test_dict, overwrite = TRUE)
  )

  expect_equal(result$sex_ontology_term_id, c("NCIT:C20197", "CURATOR:99"))
})

test_that("a whitespace-only existing value is treated as blank and filled", {
  data <- data.frame(sex = "Male", sex_ontology_term_id = "   ", stringsAsFactors = FALSE)
  result <- suppressMessages(add_ontology_columns(data, dictionary = test_dict))

  expect_equal(result$sex_ontology_term_id, "NCIT:C20197")
})

test_that("a factor-valued dictionary is coerced rather than erroring", {
  factor_dict <- test_dict
  factor_dict[] <- lapply(factor_dict, function(x) if (is.character(x)) factor(x) else x)
  data <- data.frame(sex = "Male", stringsAsFactors = FALSE)

  result <- suppressMessages(add_ontology_columns(data, dictionary = factor_dict))

  expect_equal(result$sex_ontology_term_id, "NCIT:C20197")
})

test_that("a dictionary missing required columns errors with a clear message", {
  incomplete_dict <- data.frame(col.name = "sex", corpus.type = "static_enum",
                                 stringsAsFactors = FALSE)
  data <- data.frame(sex = "Male", stringsAsFactors = FALSE)

  expect_error(
    add_ontology_columns(data, dictionary = incomplete_dict),
    "missing required column"
  )
})

test_that("leaves columns with no matching dictionary row untouched", {
  data <- data.frame(study_name = "EinsteinA_1922", sex = "Male", stringsAsFactors = FALSE)
  result <- suppressMessages(add_ontology_columns(data, dictionary = test_dict))

  expect_false("study_name_ontology_term_id" %in% names(result))
})

test_that("matches the real dictionary's mapping on real AsnicarF_2017 data", {
  # Compares against a mapping computed independently from the current
  # dictionary, not against the historical curated values in the TSV itself -
  # some curated files predate later dictionary edits (e.g. "control" is
  # recorded as "NCIT_C142703" in most studies, but the dictionary now
  # declares "NCIT:C142703"), so the TSV is not reliable ground truth here.
  real_file <- "../../inst/curated/AsnicarF_2017/AsnicarF_2017_sample.tsv"
  dict_file <- "../../inst/extdata/cMD_data_dictionary.csv"
  skip_if_not(file.exists(real_file))
  skip_if_not(file.exists(dict_file))

  real <- read.delim(real_file, stringsAsFactors = FALSE, check.names = FALSE)
  dict <- read.csv(dict_file, stringsAsFactors = FALSE)

  static_fields <- dict$col.name[!is.na(dict$corpus.type) & dict$corpus.type == "static_enum" &
                                    !is.na(dict$allowedvalues) & !is.na(dict$static.enum)]
  target_fields <- Filter(function(f) {
    labels <- strsplit(dict$allowedvalues[dict$col.name == f], "|", fixed = TRUE)[[1]]
    ids <- strsplit(dict$static.enum[dict$col.name == f], "|", fixed = TRUE)[[1]]
    f %in% names(real) && paste0(f, "_ontology_term_id") %in% names(real) &&
      length(labels) == length(ids)
  }, static_fields)
  expect_true(length(target_fields) > 0)

  oid_cols <- paste0(target_fields, "_ontology_term_id")
  stripped <- real
  stripped[oid_cols] <- NA_character_

  result <- suppressMessages(add_ontology_columns(stripped, dictionary = dict))

  for (field in target_fields) {
    row <- dict[dict$col.name == field, ]
    labels <- trimws(strsplit(row$allowedvalues, "|", fixed = TRUE)[[1]])
    ids <- trimws(strsplit(row$static.enum, "|", fixed = TRUE)[[1]])
    map <- stats::setNames(ids, labels)
    multiplevalues <- isTRUE(as.logical(row$multiplevalues))
    delimiter <- row$delimiter

    if (multiplevalues && !is.na(delimiter) && nzchar(delimiter)) {
      expected <- vapply(trimws(real[[field]]), function(x) {
        if (is.na(x) || !nzchar(x)) return(NA_character_)
        pieces <- trimws(strsplit(x, delimiter, fixed = TRUE)[[1]])
        mapped <- unname(map[pieces])
        if (any(is.na(mapped))) return(NA_character_)
        paste(mapped, collapse = delimiter)
      }, character(1), USE.NAMES = FALSE)
    } else {
      expected <- unname(map[trimws(real[[field]])])
    }
    expect_equal(result[[paste0(field, "_ontology_term_id")]], expected, info = field)
  }
})
