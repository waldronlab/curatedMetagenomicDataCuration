#' Get relevant metadata for each SRR using SRP
#'
#' @param srp An SRP as a string beginning with the prefix "SRP"
#'
#' @return a data.frame with sampleID, sequencing_platform, number_reads, and avg_read_length
#' @importFrom jsonlite fromJSON
#' @importFrom curl curl
#' @importFrom dplyr group_by summarize %>% bind_rows 
#' @importFrom omicidxClientR ApiClient SRAApi
#' @importFrom tibble add_column
#' @export get_metadata
#' @details Supports studies with up to 99,990 runs. See Examples for the template used for curatedMetagenomicData. 
#' The data.frame has five columns:
#'
#' 1. "sampleID" Sample ID.
#' 2. "sequencing_platform": Sequencing platform used.
#' 3. "number_reads": Number of reads. If there are multiple SRRs, the sum of number_reads is used.
#' 4. "avg_read_length": Average read length If there are multiple SRRs, the mean avg_read_length  is used.
#' 5. "SRRs": A list of SRRs associated with the sample ID. 
#' 
#
get_metadata <- function(srp){
  
  cursor=""
  i=1
  srrlist <- vector(mode = "list", length = 100)
  basePath <- "https://api.omicidx.cancerdatasci.org"
  client <-  ApiClient$new(basePath = basePath)
  api <-  SRAApi$new(apiClient = client)
  
  while(is.null(cursor)==FALSE){
    res <- api$RunsForStudySraStudiesAccessionRunsGet(srp, size=999, cursor=cursor)
    srrlist[[i]] <- res$hits
    cursor <- res$cursor
    i=i+1
  }
  
  srrs <- bind_rows(srrlist)
  
  srrcols <- c("sample_accession", "accession", "experiment$platform", 
               "experiment$library_layout", "total_spots", "avg_length")
  
  missingcols <- srrcols[!srrcols %in% names(srrs)] %>% sapply(., .addNA)
  srrswithmiss <- srrs %>% select(any_of(srrcols)) %>% add_column(!!!missingcols)
  
  df_srr <- data.frame(sampleID=srrswithmiss$sample_accession,
                       SRR=srrswithmiss$accession,
                       sequencing_platform=srrswithmiss$`experiment$platform`,
                       number_reads=ifelse(srrswithmiss$`experiment$library_layout`=="PAIRED", as.numeric(srrs$total_spots)*2, srrs$total_spots),
                       avg_read_length=srrswithmiss$avg_length)
  
  options(dplyr.summarise.inform = FALSE)
  df_srr <- df_srr %>%
    group_by(sampleID) %>%
    summarize(avg_read_length=mean(as.double(avg_read_length)),
              number_reads=sum(number_reads),
              SRRs=list(SRR[sampleID==sampleID]),
              sequencing_platform=first(sequencing_platform))
  options(dplyr.summarise.inform = TRUE)
  
  return(df_srr)
}

.addNA <-function(colname){
  c(colname=NA) %>% return()
}
