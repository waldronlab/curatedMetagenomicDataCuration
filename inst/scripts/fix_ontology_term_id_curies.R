#!/usr/bin/env Rscript
#
# fix_ontology_term_id_curies.R
# -----------------------------------------------------------------------------
# ONE-TIME CORPUS FIX for issue #162: some *_ontology_term_id cells were
# written with an underscore instead of the canonical CURIE colon separator
# (e.g. "NCIT_C142703" instead of "NCIT:C142703"). Corrects every such column,
# in place, across inst/curated/, leaving every other column and every
# already-correct value untouched. Idempotent - a file with nothing left to
# fix is left alone.
#
# Usage: Rscript inst/scripts/fix_ontology_term_id_curies.R
# -----------------------------------------------------------------------------

CURIE_UNDERSCORE_PATTERN <- "\\b([A-Za-z]+)_([A-Za-z]*[0-9][A-Za-z0-9]*)\\b"

#' Fix one tab-delimited field of `line`, identified by column index
#'
#' Column boundaries are computed from this line's own tab positions (not the
#' header's), and only the target field's substring is replaced - every other
#' byte of the line is left untouched. This deliberately avoids
#' strsplit(line, "\t") + paste(..., collapse = "\t"): strsplit() silently
#' drops a trailing empty field, which would corrupt any row whose last
#' column is blank.
#' @noRd
.fix_columns_in_line <- function(line, col_indices) {
  tabs <- gregexpr("\t", line, fixed = TRUE)[[1]]
  has_tabs <- tabs[1] != -1
  starts <- c(1L, if (has_tabs) tabs + 1L else integer(0))
  ends <- c(if (has_tabs) tabs - 1L else integer(0), nchar(line))
  n_fields <- length(starts)
  n_fixed <- 0L

  for (idx in col_indices) {
    if (idx > n_fields) next
    field <- substr(line, starts[idx], ends[idx])
    fixed <- gsub(CURIE_UNDERSCORE_PATTERN, "\\1:\\2", field, perl = TRUE)
    if (!identical(fixed, field)) {
      stopifnot(nchar(fixed) == nchar(field))
      line <- paste0(substr(line, 1L, starts[idx] - 1L), fixed,
                      substr(line, ends[idx] + 1L, nchar(line)))
      n_fixed <- n_fixed + 1L
    }
  }
  list(line = line, n_fixed = n_fixed)
}

#' Fix every *_ontology_term_id column of one curated TSV file, in place
#'
#' Columns are found by header name (`*_ontology_term_id`), not by filename
#' pattern, so this also reaches the handful of combined `<Study>.tsv` files
#' that predate the `<Study>_sample.tsv`/`<Study>_study.tsv` split.
#'
#' Reads and writes raw bytes rather than using readLines()/writeLines():
#' most curated files use bare "\\n" line endings, but a few use "\\r\\n", and
#' some have no trailing newline at all. readLines() strips line endings
#' entirely and writeLines() always re-adds a bare "\\n" after every line
#' (including the last), which would silently rewrite every "\\r\\n" file to
#' "\\n" and add a trailing newline to files that never had one. Splitting on
#' a zero-width lookbehind for "\\n" keeps each line's own original
#' terminator (or lack of one, for a final unterminated line) attached to it,
#' so only the matched CURIE cells change and every other byte - including
#' line-ending style - is preserved exactly.
#' @return Number of cells fixed.
#' @noRd
fix_ontology_term_id_curies <- function(path) {
  text <- rawToChar(readBin(path, "raw", n = file.info(path)$size))
  chunks <- strsplit(text, "(?<=\n)", perl = TRUE)[[1]]
  if (length(chunks) < 2L) return(0L)

  terminators <- sub(".*?(\r?\n)?$", "\\1", chunks, perl = TRUE)
  content <- substr(chunks, 1L, nchar(chunks) - nchar(terminators))

  header <- strsplit(content[1], "\t", fixed = TRUE)[[1]]
  col_indices <- grep("_ontology_term_id$", header)
  if (length(col_indices) == 0L) return(0L)

  n_fixed <- 0L
  for (i in 2:length(content)) {
    res <- .fix_columns_in_line(content[i], col_indices)
    if (res$n_fixed > 0L) {
      content[i] <- res$line
      n_fixed <- n_fixed + res$n_fixed
    }
  }
  if (n_fixed > 0L) {
    out <- paste0(content, terminators, collapse = "")
    writeBin(charToRaw(out), path)
  }
  n_fixed
}

## Only run the corpus-wide pass when THIS file is the one passed to
## `Rscript` (mirrors the --file= convention used by
## inst/scripts/make_ontology_terms.R), so it can be source()'d - including
## indirectly, e.g. by testthat under `Rscript tests/testthat.R` - to reuse
## the functions above without side effects. commandArgs() always reflects
## the outermost Rscript invocation, so a plain length(--file=) check would
## still fire in that case; comparing basenames tells "am I the entry point"
## from "was I merely source()'d by one".
.file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(.file_arg) &&
    basename(sub("^--file=", "", .file_arg)) == "fix_ontology_term_id_curies.R") {
  files <- list.files("inst/curated", pattern = "\\.tsv$", recursive = TRUE,
                       full.names = TRUE)
  total_fixed <- 0L
  changed_files <- character(0)
  for (f in files) {
    n <- fix_ontology_term_id_curies(f)
    if (n > 0L) {
      cat(sprintf("%s: fixed %d value(s)\n", f, n))
      total_fixed <- total_fixed + n
      changed_files <- c(changed_files, f)
    }
  }
  cat(sprintf("\nDone: %d value(s) fixed across %d file(s).\n",
              total_fixed, length(changed_files)))
}
