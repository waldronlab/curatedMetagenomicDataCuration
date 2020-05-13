#' Get relevant metadata for each SRR using SRP
#'
#' @param srp An SRP as a string beginning with the prefix "SRP"
#'
#' @return a data.frame with two elements, [["colnames"]] and [["values]]
#' @importFrom jsonlite fromJSON
#' @importFrom curl curl
#' @export get_metadata
#' @details See Examples for the template used for curatedMetagenomicData. 
#' The template has five columns:
#'
#' 1. "col.name" specifies the name of the column in the curated data.frame.
#' 2. "multiplevalues": multiple semicolon-separated values are allowed
#' 3. "uniqueness": unique means each value must be unique, non-unique means repeated values are allowed
#' 4. "requiredness": if "required", there must be no missing (NA) values. If "optional", missing values are allowed.
#' 5. "allowedvalues": a regex defining allowable values for the column
#' 6. "description": a free-form description of the variable
#' 
#
get_metadata <- function(srp){
  url_lookup <- paste('https://api.omicidx.cancerdatasci.org/sra/studies/', srp, '/runs?size=500', sep="")
  srrs <- jsonlite::fromJSON(curl(url_lookup))$hits
  df_srr <- data.frame(sampleID=srrs$sample_accession, SRR=srrs$accession,
                       sequencing_platform=srrs$experiment$platform,
                       number_reads =ifelse(srrs$experiment$library_layout=="PAIRED", as.numeric(srrs$total_spots)*2, srrs$total_spots),
                       avg_read_length=srrs$avg_length)
  return(df_srr)
}



