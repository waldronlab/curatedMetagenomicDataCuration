#' Make a combined metadata table from individual TSVs
#'
#' Reads all harmonized sample metadata TSV files from
#' \code{inst/harmonized/} and combines them into a single
#' \code{data.table}. A \code{dataset_name} column is added to each
#' table, derived from the filename (e.g. \code{StudyA_2021_sample.tsv}
#' becomes \code{StudyA_2021}).
#'
#' @return A \code{\link[data.table]{data.table}} containing sample-level
#'   metadata for all studies, with columns filled with \code{NA} where a
#'   study does not include a particular field.
#'
#' @examples
#' \dontrun{
#' meta <- makeCombinedMetadata()
#' meta[, .N, by = dataset_name]
#' }
#'
#' @importFrom data.table fread rbindlist :=
#' @export
makeCombinedMetadata <- function() {
  tsv_files <- list.files(
    system.file("harmonized", package = "curatedMetagenomicDataCuration"),
    recursive = TRUE,
    pattern = "_sample\\.tsv$",
    full.names = TRUE
  )
  tables <- lapply(tsv_files, function(f) {
    x <- fread(f)
    x[, dataset_name := sub("_sample.tsv", "", basename(f), fixed = TRUE)]
  })
  rbindlist(tables, fill = TRUE)
}