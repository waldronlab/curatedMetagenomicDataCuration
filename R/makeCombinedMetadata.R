#' Make a combined metadata table from individual TSVs
#'
#' Reads all curated sample metadata TSV files from
#' \code{inst/curated/} and combines them into a single
#' \code{data.table}. A \code{dataset_name} column is added to each
#' table, derived from the filename (e.g. \code{StudyA_2021_sample.tsv}
#' becomes \code{StudyA_2021}).
#'
#' @param include_before_harmonized If it is set to `TRUE`, the returned 
#' table will include not-harmonized (i.e., not-covered by the schema) 
#' attributes as well. Default is `FALSE`.
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
makeCombinedMetadata <- function(include_before_harmonized = FALSE) {
  
  tsv_files <- list.files(
    system.file("curated", package = "curatedMetagenomicDataCuration"),
    recursive = TRUE,
    pattern = "_sample\\.tsv$",
    full.names = TRUE
  )
  
  tables <- lapply(tsv_files, function(f) {
    x <- fread(f)
    x[, dataset_name := sub("_sample.tsv", "", basename(f), fixed = TRUE)]
  })
  
  all <- rbindlist(tables, fill = TRUE)
  
  if (isFALSE(include_before_harmonized)) {
    extDir <- system.file("extdata", package = "curatedMetagenomicDataCuration")
    harmonized_cols <- read.csv(file.path(extDir, "cMD_data_dictionary.csv")) %>% pull(col.name)
    all <- all %>% dplyr::select(any_of(harmonized_cols))
  } 
  
  key_id_cols <- c("study_name", "sample_id", "subject_id", "target_condition")
  all_reordered <- all %>% 
    select(any_of(key_id_cols), sort(setdiff(names(.), key_id_cols)))

  return(all_reordered)
}