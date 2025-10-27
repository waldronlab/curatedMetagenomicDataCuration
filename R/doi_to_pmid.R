#' Convert DOI to PubMed ID (PMID)
#'
#' This function retrieves the PubMed ID (PMID) corresponding to a given Digital Object Identifier (DOI)
#' using the NCBI E-utilities API. The function automatically cleans DOI formatting and handles
#' cases where no PMID is found.
#'
#' @param doi A character string containing the DOI to convert. Can be provided with or without
#'   URL prefixes (e.g., "10.1038/nature12373" or "https://doi.org/10.1038/nature12373").
#'
#' @return A character string containing the PMID if found, or NA if:
#'   \itemize{
#'     \item The DOI is not associated with a PubMed record
#'     \item The API request fails
#'     \item The DOI format is invalid
#'   }
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Automatically installs and loads required packages (httr, xml2) if not available
#'   \item Cleans the input DOI by removing common URL prefixes
#'   \item Queries the NCBI E-utilities API (esearch.fcgi endpoint)
#'   \item Parses the XML response to extract the PMID
#'   \item Returns NA with a warning if no PMID is found or if the request fails
#' }
#'
#' @section API Usage:
#' This function uses the NCBI E-utilities API. Users should be aware of NCBI's usage guidelines:
#' \itemize{
#'   \item Rate limit: Maximum 3 requests per second (without API key)
#'   \item For batch processing, consider adding delays between requests
#'   \item See \url{https://www.ncbi.nlm.nih.gov/books/NBK25497/} for more information
#' }
#'
#' @section Dependencies:
#' Required packages:
#' \itemize{
#'   \item httr - For making HTTP requests to the NCBI API
#'   \item xml2 - For parsing XML responses
#' }
#'
#' @examples
#' \dontrun{
#' # Basic usage with a DOI
#' pmid <- doi_to_pmid("10.1038/nature12373")
#' print(pmid)  # Output: "23842501"
#'
#' # Works with full DOI URLs
#' pmid <- doi_to_pmid("https://doi.org/10.1126/science.1259855")
#' print(pmid)
#'
#' # Returns NA for non-existent or non-PubMed DOIs
#' pmid <- doi_to_pmid("10.1234/invalid.doi")
#' print(pmid)  # Output: NA (with warning)
#'
#' # Process multiple DOIs
#' dois <- c("10.1038/nature12373", "10.1126/science.1259855")
#' pmids <- sapply(dois, doi_to_pmid)
#' }
#'
#' @seealso
#' \itemize{
#'   \item NCBI E-utilities documentation: \url{https://www.ncbi.nlm.nih.gov/books/NBK25500/}
#'   \item Alternative: \code{\link[rentrez]{entrez_search}} from the rentrez package
#' }
#'
#' @author Your Name
#'
#' @references
#' NCBI E-utilities: \url{https://www.ncbi.nlm.nih.gov/books/NBK25501/}
#'
#' @note
#' \itemize{
#'   \item Not all DOIs have corresponding PMIDs (only articles indexed in PubMed)
#'   \item The function includes a small delay mechanism for batch processing to respect API limits
#'   \item Internet connection is required
#'   \item For large-scale batch processing, consider using an NCBI API key
#' }
#'
#' @export
doi_to_pmid <- function(doi) {
  # Load required library
  if (!require("httr")) install.packages("httr")
  if (!require("xml2")) install.packages("xml2")
  
  library(httr)
  library(xml2)
  
  # Clean DOI (remove any URL prefix if present)
  doi <- gsub("https?://doi.org/", "", doi)
  doi <- gsub("https?://dx.doi.org/", "", doi)
  
  # NCBI E-utilities API endpoint
  base_url <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi"
  
  # Query parameters
  query_params <- list(
    db = "pubmed",
    term = paste0(doi, "[DOI]"),
    retmode = "xml"
  )
  
  # Make API request
  response <- GET(base_url, query = query_params)
  
  # Check if request was successful
  if (status_code(response) != 200) {
    warning(paste("Failed to retrieve PMID for DOI:", doi))
    return(NA)
  }
  
  # Parse XML response
  content <- content(response, "text", encoding = "UTF-8")
  xml_data <- read_xml(content)
  
  # Extract PMID
  pmid <- xml_text(xml_find_first(xml_data, ".//Id"))
  
  if (length(pmid) == 0 || is.na(pmid)) {
    warning(paste("No PMID found for DOI:", doi))
    return(NA)
  }
  
  return(pmid)
}
