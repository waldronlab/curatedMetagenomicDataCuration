library(curl)
library(jsonlite)

#' Get relevant metadata for each SRR using SRP
get_metadata <- function(srp){
  url_lookup <- paste('https://api.omicidx.cancerdatasci.org/sra/studies/', srp, '/runs?size=500', sep="")
  srrs <- jsonlite::fromJSON(curl(url_lookup))$hits
  df_srr <- data.frame(sampleID=srrs$sample_accession, SRR=srrs$accession,
                       sample_title = srrs$sample$title,
                       sample_description = srrs$sample,
                       sequencing_platform=srrs$experiment$platform,
                       number_reads =ifelse(srrs$experiment$library_layout=="PAIRED", as.numeric(srrs$total_spots)*2, srrs$total_spots),
                       avg_read_length=srrs$avg_length)
  return(df_srr)
}



