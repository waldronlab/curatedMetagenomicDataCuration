#' Make a combined metadata table from individual TSVs
#'
#' @return A data.table containing metadata for all studies in inst/curated

makeCombinedMetadata <- function(){
  rbindlist(.fetchTSVs(), fill=TRUE) %>% return()
}

.fetchTSVs <- function(){
  studies <- system.file("curated", package = "curatedMetagenomicDataCuration") %>% 
    list.files(recursive = TRUE, pattern="\\.tsv$", full.names = TRUE) %>% 
    lapply(. %>% .fetchStudy())
  return(studies)
}


.fetchStudy <- function(filePath=""){
  x <- read.table(filePath, header=TRUE, sep="\t")
  studyName <- filePath %>% basename() %>% 
    sub(x=., "_metadata.tsv", "", fixed=TRUE)
  cbind(dataset_name=studyName, x) %>% return()
}
