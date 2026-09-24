# Sourcing (rather than running) this script only defines its functions -
# the corpus-wide pass is gated on Rscript's --file= argument, which is not
# present when testthat sources this file. See inst/scripts/fix_ontology_term_id_curies.R.
source("../../inst/scripts/fix_ontology_term_id_curies.R", local = TRUE)

test_that("fixes only the malformed ontology_term_id cells, leaving other columns untouched", {
  tsv <- tempfile(fileext = ".tsv")
  writeLines(c(
    "sample_id\tcontrol\tcontrol_ontology_term_id\tnotes",
    "S1\tStudy Control\tNCIT_C142703\tunderscore_in_notes_ok",
    "S2\tCase\tNCIT:C49152\tanother_note",
    "S3\tNot Used\tNCIT_C69062\t"
  ), tsv)

  n <- fix_ontology_term_id_curies(tsv)

  expect_equal(n, 2L)
  expect_equal(
    readLines(tsv),
    c(
      "sample_id\tcontrol\tcontrol_ontology_term_id\tnotes",
      "S1\tStudy Control\tNCIT:C142703\tunderscore_in_notes_ok",
      "S2\tCase\tNCIT:C49152\tanother_note",
      "S3\tNot Used\tNCIT:C69062\t"
    )
  )
})

test_that("is idempotent", {
  tsv <- tempfile(fileext = ".tsv")
  writeLines(c("a\tb_ontology_term_id", "x\tNCIT_C142703"), tsv)

  once <- fix_ontology_term_id_curies(tsv)
  content_once <- readLines(tsv)
  twice <- fix_ontology_term_id_curies(tsv)

  expect_equal(once, 1L)
  expect_equal(twice, 0L)
  expect_equal(readLines(tsv), content_once)
})

test_that("preserves CRLF line endings and a missing trailing newline exactly", {
  tsv <- tempfile(fileext = ".tsv")
  con <- file(tsv, "wb")
  writeBin(charToRaw("a\tb_ontology_term_id\r\nx\tNCIT_C142703"), con)
  close(con)

  n <- fix_ontology_term_id_curies(tsv)
  after <- rawToChar(readBin(tsv, "raw", n = file.info(tsv)$size))

  expect_equal(n, 1L)
  expect_identical(after, "a\tb_ontology_term_id\r\nx\tNCIT:C142703")
})

test_that("leaves a file untouched when it has no _ontology_term_id column", {
  tsv <- tempfile(fileext = ".tsv")
  writeLines(c("a\tb", "NCIT_C142703\tx"), tsv)
  before <- readLines(tsv)

  n <- fix_ontology_term_id_curies(tsv)

  expect_equal(n, 0L)
  expect_equal(readLines(tsv), before)
})
