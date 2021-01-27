#' Get relevant metadata for each SRR using SRP
#' Get relevant metadata for each SRR using SRP
#'
#' @param srp An SRP as a string beginning with the prefix "SRP"
#'
#' @return a data.frame with sampleID, sequencing_platform, number_reads, and avg_read_length
#' @importFrom jsonlite fromJSON
#' @importFrom curl curl
#' @importFrom dplyr group_by summarize
#' @export get_metadata
#' @details See Examples for the template used for curatedMetagenomicData. 
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
  url_lookup <- paste('https://api.omicidx.cancerdatasci.org/sra/studies/', srp, '/runs?size=500', sep="")
  srrs <- jsonlite::fromJSON(curl(url_lookup))$hits
  df_srr <- data.frame(sampleID=srrs$sample_accession, SRR=srrs$accession,
                       sample_title = srrs$sample$title,
                       sample_description = srrs$sample,
                       sequencing_platform=srrs$experiment$platform,
                       number_reads =ifelse(srrs$experiment$library_layout=="PAIRED", as.numeric(srrs$total_spots)*2, srrs$total_spots),
                       avg_read_length=srrs$avg_length)
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
