#' Extract Pattern Matches from PubMed Central Full Text Articles
#'
#' @description
#' Searches for and extracts all occurrences of a specified pattern (e.g., 
#' database accession numbers, identifiers, DOIs) from the full text of a 
#' PubMed Central (PMC) article. This is useful for finding biological database
#' accessions, identifiers, or other structured text that may not be captured
#' in article abstracts or metadata.
#' 
#' @import rentrez
#' @import xml2 
#' @import stringr
#'
#' @param pmid Character string or numeric. The PubMed ID (PMID) of the article
#'   to search. Must be a valid NCBI PubMed identifier.
#' @param pattern Character string. A regular expression pattern to search for
#'   in the full text. Common patterns include accession numbers for BioProject,
#'   BioSample, SRA, GEO, GenBank, and other biological databases. See Examples
#'   for common patterns.
#'
#' @return A list containing:
#'   \itemize{
#'     \item \code{found}: Logical. TRUE if PMC full text was available and 
#'       searched, FALSE if no full text was found
#'     \item \code{matches}: Character vector of unique pattern matches found 
#'       in the full text. NULL if no full text available or no matches found
#'     \item \code{count}: Integer. Total number of pattern matches found 
#'       (including duplicates)
#'     \item \code{pmcid}: Character string. The PMC ID used to retrieve the 
#'       full text
#'     \item \code{source}: Character string. Description of the data source
#'       ("PMC full text" if successful, "No PMC full text" if unavailable)
#'   }
#'
#' @details
#' This function performs the following steps:
#' \enumerate{
#'   \item Queries NCBI to check if the PMID has an associated PMC full text article
#'   \item If available, retrieves the full text XML from PubMed Central
#'   \item Parses the XML and extracts all text content
#'   \item Searches for all matches of the provided regex pattern
#'   \item Returns unique matches along with metadata
#' }
#' 
#' Note that only open access articles in PubMed Central can be accessed. 
#' Articles that are not in PMC or are behind paywalls will return 
#' \code{found = FALSE}.
#'
#' @note
#' \itemize{
#'   \item Requires an active internet connection
#'   \item Only works with articles that have open access full text in PMC
#'   \item Rate limiting applies (NCBI allows 3 requests/second without API key)
#'   \item Regular expressions are case-sensitive by default
#'   \item Large articles may take longer to process
#'   \item The function extracts all text including references, which may 
#'     contain false positive matches
#' }
#'
#' @examples
#' \dontrun{
#' # Example 1: Search for BioProject accessions
#' pmid <- "25359968"
#' result <- extract_pattern_from_fulltext(pmid, "PRJ[NED][A-Z][0-9]+")
#' 
#' if (result$found) {
#'   cat("Found", result$count, "BioProject accession(s):\n")
#'   print(result$matches)
#' } else {
#'   cat(result$source, "\n")
#' }
#' 
#' # Example 2: Search for multiple types of accessions
#' patterns <- list(
#'   bioproject = "PRJ[NED][A-Z][0-9]+",
#'   biosample = "SAM[NED][A-Z]?[0-9]+",
#'   sra = "SRR[0-9]{6,}",
#'   geo = "GSE[0-9]+"
#' )
#' 
#' results <- lapply(patterns, function(p) {
#'   extract_pattern_from_fulltext(pmid, p)
#' })
#' 
#' # Display results for each pattern type
#' for (name in names(results)) {
#'   if (results[[name]]$found && length(results[[name]]$matches) > 0) {
#'     cat("\n", toupper(name), "accessions found:\n", sep = "")
#'     print(results[[name]]$matches)
#'   }
#' }
#' 
#' # Example 3: Extract DOIs from references
#' result <- extract_pattern_from_fulltext(pmid, "10\\.\\d{4,}/[-._;()/:A-Za-z0-9]+")
#' if (result$found) {
#'   cat("DOIs found:", length(result$matches), "\n")
#'   print(head(result$matches))
#' }
#' 
#' # Example 4: Case-insensitive search using regex flags
#' # Search for gene names (example: TP53, BRCA1)
#' gene_pattern <- "(?i)\\b(TP53|BRCA1|EGFR)\\b"
#' result <- extract_pattern_from_fulltext(pmid, gene_pattern)
#' 
#' # Example 5: Handle article without full text
#' result <- extract_pattern_from_fulltext("99999999", "PRJ[NED][A-Z][0-9]+")
#' if (!result$found) {
#'   cat("Error:", result$source, "\n")
#' }
#' }
#'
#' @section Common Regex Patterns:
#' Here are useful patterns for extracting common biological database identifiers:
#' \itemize{
#'   \item \strong{BioProject}: \code{"PRJ[NED][A-Z][0-9]+"}
#'   \item \strong{BioSample}: \code{"SAM[NED][A-Z]?[0-9]+"}
#'   \item \strong{SRA (Run)}: \code{"SRR[0-9]{6,}"}
#'   \item \strong{SRA (Experiment)}: \code{"SRX[0-9]{6,}"}
#'   \item \strong{SRA (Study)}: \code{"SRP[0-9]{6,}"}
#'   \item \strong{GEO Series}: \code{"GSE[0-9]+"}
#'   \item \strong{GEO Sample}: \code{"GSM[0-9]+"}
#'   \item \strong{GenBank}: \code{"[A-Z]{1,2}_?[0-9]{5,}"}
#'   \item \strong{UniProt}: \code{"[A-Z][0-9][A-Z0-9]{3}[0-9]"}
#'   \item \strong{DOI}: \code{"10\\\\.\\\\d{4,}/[-._;()/:A-Za-z0-9]+"}
#'   \item \strong{PMID}: \code{"PMID:?\\\\s*[0-9]{7,8}"}
#'   \item \strong{Gene Ontology}: \code{"GO:[0-9]{7}"}
#'   \item \strong{PubChem}: \code{"CID[0-9]{1,9}"}
#'   \item \strong{MeSH}: \code{"D[0-9]{6}"}
#'   \item \strong{Ensembl Gene}: \code{"ENSG[0-9]{11}"}
#'   \item \strong{RefSeq}: \code{"[NXW][MRP]_[0-9]+"}
#' }
#'
#' @seealso
#' \itemize{
#'   \item \code{\link[rentrez]{entrez_link}} for linking between NCBI databases
#'   \item \code{\link[rentrez]{entrez_fetch}} for retrieving records
#'   \item \code{\link[stringr]{str_extract_all}} for pattern extraction
#'   \item \code{\link[xml2]{read_xml}} for XML parsing
#'   \item PubMed Central: \url{https://www.ncbi.nlm.nih.gov/pmc/}
#'   \item Regular Expressions in R: \url{https://stringr.tidyverse.org/articles/regular-expressions.html}
#' }
#'
#' @author Your Name
#' @export
extract_pattern_from_fulltext <- function(pmid, pattern) {
  
  # Input validation
  if (missing(pmid) || is.null(pmid)) {
    stop("pmid is required")
  }
  
  if (missing(pattern) || is.null(pattern) || pattern == "") {
    stop("pattern is required and cannot be empty")
  }
  
  # Convert pmid to character if numeric
  pmid <- as.character(pmid)
  
  # Check if full text available in PMC
  tryCatch({
    links <- entrez_link(dbfrom = "pubmed", id = pmid, db = "pmc")
  }, error = function(e) {
    stop(paste("Error linking PMID to PMC:", e$message))
  })
  
  if (length(links$links$pubmed_pmc) == 0) {
    return(list(
      found = FALSE, 
      matches = NULL, 
      count = 0,
      pmcid = NULL,
      source = "No PMC full text available"
    ))
  }
  
  pmcid <- links$links$pubmed_pmc[1]
  
  # Fetch full text XML
  tryCatch({
    full_text_xml <- entrez_fetch(db = "pmc", id = pmcid, rettype = "xml")
  }, error = function(e) {
    stop(paste("Error fetching PMC full text:", e$message))
  })
  
  # Parse XML
  tryCatch({
    doc <- read_xml(full_text_xml)
  }, error = function(e) {
    stop(paste("Error parsing XML:", e$message))
  })
  
  # Extract all text content
  all_text <- xml_text(doc)
  
  # Find all matches
  matches <- str_extract_all(all_text, pattern)[[1]]
  unique_matches <- unique(matches)
  
  return(list(
    found = TRUE,
    matches = if(length(unique_matches) > 0) unique_matches else NULL,
    count = length(matches),
    pmcid = pmcid,
    source = "PMC full text"
  ))
}


# Predefined common patterns for convenience ----

#' Common Regex Patterns for Biological Database Identifiers
#'
#' @description
#' A named list of regular expression patterns for extracting common biological
#' database accession numbers and identifiers from text. Use with
#' \code{\link{extract_pattern_from_fulltext}}.
#'
#' @format A list with 11 elements:
#' \describe{
#'   \item{bioproject}{BioProject accessions (PRJNA, PRJD, PRJE)}
#'   \item{biosample}{BioSample accessions (SAMN, SAMD, SAME)}
#'   \item{sra}{SRA run accessions (SRR)}
#'   \item{geo}{GEO series accessions (GSE)}
#'   \item{genbank}{GenBank accessions}
#'   \item{uniprot}{UniProt accessions}
#'   \item{doi}{Digital Object Identifiers}
#'   \item{pmid}{PubMed IDs}
#'   \item{gene_ontology}{Gene Ontology terms}
#'   \item{pubchem}{PubChem Compound IDs}
#'   \item{mesh}{MeSH descriptor IDs}
#' }
#'
#' @examples
#' \dontrun{
#' # Use predefined patterns
#' pmid <- "25359968"
#' 
#' # Extract BioProject accessions
#' result <- extract_pattern_from_fulltext(pmid, common_patterns$bioproject)
#' 
#' # Extract multiple types at once
#' all_results <- lapply(common_patterns, function(pattern) {
#'   extract_pattern_from_fulltext(pmid, pattern)
#' })
#' }
#'
#' @export
common_patterns <- list(
  bioproject = "PRJ[NED][A-Z][0-9]+",
  biosample = "SAM[NED][A-Z]?[0-9]+",
  sra = "SRR[0-9]{6,}",
  geo = "GSE[0-9]+",
  genbank = "[A-Z]{1,2}_?[0-9]{5,}",
  uniprot = "[A-Z][0-9][A-Z0-9]{3}[0-9]",
  doi = "10\\.\\d{4,}/[-._;()/:A-Za-z0-9]+",
  pmid = "PMID:?\\s*[0-9]{7,8}",
  gene_ontology = "GO:[0-9]{7}",
  pubchem = "CID[0-9]{1,9}",
  mesh = "D[0-9]{6}"
)


# Example usage demonstration ----
if (FALSE) {  # Set to TRUE to run examples
  
  # Basic usage
  pmid <- "25359968"
  result <- extract_pattern_from_fulltext(pmid, common_patterns$bioproject)
  
  print(result)
  
  # Extract multiple patterns
  pmid <- "25359968"
  patterns_to_search <- c("bioproject", "biosample", "sra", "geo")
  
  all_results <- list()
  for (pattern_name in patterns_to_search) {
    cat("\nSearching for", pattern_name, "...\n")
    result <- extract_pattern_from_fulltext(pmid, common_patterns[[pattern_name]])
    
    if (result$found && !is.null(result$matches)) {
      cat("  Found", length(result$matches), "unique match(es)\n")
      all_results[[pattern_name]] <- result$matches
    } else {
      cat("  No matches found\n")
    }
    
    Sys.sleep(0.34)  # Rate limiting
  }
  
  # Display summary
  cat("\n=== SUMMARY ===\n")
  for (name in names(all_results)) {
    cat("\n", toupper(name), ":\n", sep = "")
    print(all_results[[name]])
  }
}