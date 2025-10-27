#' Extract Pattern Matches from PubMed Central Full Text Articles
#'
#' @description
#' Searches for and extracts all occurrences of a specified pattern (e.g., 
#' database accession numbers, identifiers, DOIs) from the full text of 
#' PubMed Central (PMC) articles. This is useful for finding biological database
#' accessions, identifiers, or other structured text that may not be captured
#' in article abstracts or metadata. Can process a single PMID or multiple PMIDs.
#' 
#' @import rentrez
#' @import xml2 
#' @import stringr
#'
#' @param pmid Character string/numeric or character vector. The PubMed ID(s) 
#'   (PMID) of the article(s) to search. Must be valid NCBI PubMed identifier(s).
#' @param pattern Character string. A regular expression pattern to search for
#'   in the full text. Common patterns include accession numbers for BioProject,
#'   BioSample, SRA, GEO, GenBank, and other biological databases. See Examples
#'   for common patterns.
#' @param return_format Character string. Format of the return value. Options:
#'   \itemize{
#'     \item \code{"auto"} (default): Returns a list for single PMID, data frame for multiple PMIDs
#'     \item \code{"list"}: Always returns a list (original format)
#'     \item \code{"dataframe"}: Always returns a data frame
#'   }
#' @param progress Logical. If TRUE, displays progress messages when processing
#'   multiple PMIDs. Default is TRUE.
#' @param rate_limit Numeric. Number of seconds to wait between API calls when
#'   processing multiple PMIDs. Default is 0.34 (approximately 3 requests/second).
#'
#' @return 
#' For a single PMID (when \code{return_format = "list"} or \code{"auto"}):
#' A list containing:
#'   \itemize{
#'     \item \code{pmid}: The input PMID
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
#' For multiple PMIDs (when \code{return_format = "dataframe"} or \code{"auto"}):
#' A data frame with columns:
#'   \itemize{
#'     \item \code{pmid}: The input PMID
#'     \item \code{found}: Logical indicating if full text was available
#'     \item \code{match}: Individual pattern matches (one row per match)
#'     \item \code{pmcid}: The PMC ID (if available)
#'     \item \code{source}: Description of data source
#'     \item \code{total_matches}: Total count of matches for this PMID
#'     \item \code{error}: Error message if processing failed (NA otherwise)
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
#' When processing multiple PMIDs, the function includes rate limiting and
#' error handling to ensure robust processing of large batches.
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
#' # Example 1: Single PMID (returns list)
#' pmid <- "25359968"
#' result <- extract_pattern_from_fulltext(pmid, "PRJ[NED][A-Z][0-9]+")
#' 
#' if (result$found) {
#'   cat("Found", result$count, "BioProject accession(s):\n")
#'   print(result$matches)
#' }
#' 
#' # Example 2: Multiple PMIDs (returns data frame)
#' pmids <- c("25359968", "26819408", "27924034")
#' results_df <- extract_pattern_from_fulltext(pmids, "PRJ[NED][A-Z][0-9]+")
#' 
#' # View results
#' print(results_df)
#' 
#' # Get PMIDs with matches
#' pmids_with_matches <- unique(results_df$pmid[!is.na(results_df$match)])
#' 
#' # Count matches per PMID
#' library(dplyr)
#' match_summary <- results_df %>%
#'   group_by(pmid) %>%
#'   summarize(
#'     found = first(found),
#'     n_matches = sum(!is.na(match)),
#'     matches = paste(na.omit(match), collapse = ", ")
#'   )
#' print(match_summary)
#' 
#' # Example 3: Multiple PMIDs with multiple patterns
#' pmids <- c("25359968", "26819408")
#' patterns <- list(
#'   bioproject = "PRJ[NED][A-Z][0-9]+",
#'   sra = "SRR[0-9]{6,}",
#'   geo = "GSE[0-9]+"
#' )
#' 
#' all_results <- list()
#' for (pattern_name in names(patterns)) {
#'   cat("\nSearching for", pattern_name, "...\n")
#'   all_results[[pattern_name]] <- extract_pattern_from_fulltext(
#'     pmids, 
#'     patterns[[pattern_name]]
#'   )
#' }
#' 
#' # Combine results
#' combined_df <- bind_rows(all_results, .id = "pattern_type")
#' 
#' # Example 4: Force data frame output for single PMID
#' result_df <- extract_pattern_from_fulltext(
#'   "25359968", 
#'   "PRJ[NED][A-Z][0-9]+",
#'   return_format = "dataframe"
#' )
#' 
#' # Example 5: Process with custom rate limiting
#' pmids <- c("25359968", "26819408", "27924034", "28949297")
#' results <- extract_pattern_from_fulltext(
#'   pmids, 
#'   "PRJ[NED][A-Z][0-9]+",
#'   rate_limit = 0.5,  # Slower rate
#'   progress = TRUE
#' )
#' 
#' # Save results
#' write.csv(results, "pattern_extraction_results.csv", row.names = FALSE)
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
extract_pattern_from_fulltext <- function(pmid, 
                                          pattern, 
                                          return_format = "auto",
                                          progress = TRUE,
                                          rate_limit = 0.34) {
  
  # Input validation
  if (missing(pmid) || is.null(pmid)) {
    stop("pmid is required")
  }
  
  if (missing(pattern) || is.null(pattern) || pattern == "") {
    stop("pattern is required and cannot be empty")
  }
  
  # Convert pmid to character vector
  pmid <- as.character(pmid)
  
  # Determine if processing single or multiple PMIDs
  is_multiple <- length(pmid) > 1
  
  # Helper function to process a single PMID
  process_single_pmid <- function(single_pmid) {
    
    result <- list(
      pmid = single_pmid,
      found = FALSE,
      matches = NULL,
      count = 0,
      pmcid = NULL,
      source = NULL,
      error = NA
    )
    
    tryCatch({
      # Check if full text available in PMC
      links <- entrez_link(dbfrom = "pubmed", id = single_pmid, db = "pmc")
      
      if (length(links$links$pubmed_pmc) == 0) {
        result$source <- "No PMC full text available"
        return(result)
      }
      
      pmcid <- links$links$pubmed_pmc[1]
      result$pmcid <- pmcid
      
      # Fetch full text XML
      full_text_xml <- entrez_fetch(db = "pmc", id = pmcid, rettype = "xml")
      
      # Parse XML
      doc <- read_xml(full_text_xml)
      
      # Extract all text content
      all_text <- xml_text(doc)
      
      # Find all matches
      matches <- str_extract_all(all_text, pattern)[[1]]
      unique_matches <- unique(matches)
      
      result$found <- TRUE
      result$matches <- if(length(unique_matches) > 0) unique_matches else NULL
      result$count <- length(matches)
      result$source <- "PMC full text"
      
    }, error = function(e) {
      result$error <<- e$message
      result$source <<- "Error during processing"
    })
    
    return(result)
  }
  
  # Process single or multiple PMIDs
  if (is_multiple) {
    # Process multiple PMIDs
    if (progress) {
      cat(paste("Processing", length(pmid), "PMIDs...\n"))
    }
    
    all_results <- list()
    
    for (i in seq_along(pmid)) {
      if (progress && i %% 10 == 0) {
        cat(paste("  Processed", i, "of", length(pmid), "\n"))
      }
      
      all_results[[i]] <- process_single_pmid(pmid[i])
      
      # Rate limiting (skip on last iteration)
      if (i < length(pmid)) {
        Sys.sleep(rate_limit)
      }
    }
    
    if (progress) {
      cat("Processing complete!\n")
    }
    
    # Convert to data frame format
    results_df <- data.frame()
    
    for (result in all_results) {
      if (!is.null(result$matches) && length(result$matches) > 0) {
        # One row per match
        for (match in result$matches) {
          results_df <- rbind(results_df, data.frame(
            pmid = result$pmid,
            found = result$found,
            match = match,
            pmcid = result$pmcid,
            source = result$source,
            total_matches = result$count,
            error = result$error,
            stringsAsFactors = FALSE
          ))
        }
      } else {
        # One row with NA match if no matches found
        results_df <- rbind(results_df, data.frame(
          pmid = result$pmid,
          found = result$found,
          match = NA,
          pmcid = result$pmcid,
          source = result$source,
          total_matches = result$count,
          error = result$error,
          stringsAsFactors = FALSE
        ))
      }
    }
    
    # Handle return format
    if (return_format == "auto" || return_format == "dataframe") {
      return(results_df)
    } else if (return_format == "list") {
      return(all_results)
    } else {
      stop("Invalid return_format. Choose 'auto', 'list', or 'dataframe'.")
    }
    
  } else {
    # Process single PMID
    result <- process_single_pmid(pmid[1])
    
    # Handle return format
    if (return_format == "dataframe") {
      # Convert single result to data frame
      if (!is.null(result$matches) && length(result$matches) > 0) {
        results_df <- data.frame(
          pmid = result$pmid,
          found = result$found,
          match = result$matches,
          pmcid = result$pmcid,
          source = result$source,
          total_matches = result$count,
          error = result$error,
          stringsAsFactors = FALSE
        )
      } else {
        results_df <- data.frame(
          pmid = result$pmid,
          found = result$found,
          match = NA,
          pmcid = result$pmcid,
          source = result$source,
          total_matches = result$count,
          error = result$error,
          stringsAsFactors = FALSE
        )
      }
      return(results_df)
    } else {
      # Return as list (default for single PMID)
      return(result)
    }
  }
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
#' # Use predefined patterns with multiple PMIDs
#' pmids <- c("25359968", "26819408", "27924034")
#' 
#' # Extract BioProject accessions
#' result <- extract_pattern_from_fulltext(pmids, common_patterns$bioproject)
#' 
#' # Extract multiple types at once
#' all_results <- lapply(names(common_patterns), function(pattern_name) {
#'   df <- extract_pattern_from_fulltext(pmids, common_patterns[[pattern_name]])
#'   df$pattern_type <- pattern_name
#'   return(df)
#' })
#' 
#' combined_results <- do.call(rbind, all_results)
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


# # Example usage demonstration ----
# if (FALSE) {  # Set to TRUE to run examples
#   
#   # Example 1: Single PMID (returns list by default)
#   pmid <- "25359968"
#   result <- extract_pattern_from_fulltext(pmid, common_patterns$bioproject)
#   print(result)
#   
#   # Example 2: Multiple PMIDs (returns data frame by default)
#   pmids <- c("25359968", "26819408", "27924034")
#   results_df <- extract_pattern_from_fulltext(pmids, common_patterns$bioproject)
#   print(results_df)
#   
#   # Example 3: Get summary statistics
#   library(dplyr)
#   
#   summary_stats <- results_df %>%
#     group_by(pmid) %>%
#     summarize(
#       found = first(found),
#       pmcid = first(pmcid),
#       n_unique_matches = sum(!is.na(match)),
#       matches = paste(na.omit(match), collapse = ", "),
#       source = first(source)
#     )
#   
#   print(summary_stats)
#   
#   # Example 4: Extract multiple pattern types
#   pmids <- c("25359968", "26819408")
#   patterns_to_search <- c("bioproject", "biosample", "sra", "geo")
#   
#   all_results <- list()
#   for (pattern_name in patterns_to_search) {
#     cat("\nSearching for", pattern_name, "...\n")
#     df <- extract_pattern_from_fulltext(pmids, common_patterns[[pattern_name]])
#     df$pattern_type <- pattern_name
#     all_results[[pattern_name]] <- df
#   }
#   
#   # Combine all results
#   combined_df <- bind_rows(all_results)
#   
#   # View summary
#   pattern_summary <- combined_df %>%
#     filter(!is.na(match)) %>%
#     group_by(pmid, pattern_type) %>%
#     summarize(
#       n_matches = n(),
#       matches = paste(match, collapse = ", "),
#       .groups = "drop"
#     )
#   
#   print(pattern_summary)
#   
#   # Save results
#   write.csv(combined_df, "all_pattern_matches.csv", row.names = FALSE)
#   write.csv(pattern_summary, "pattern_summary.csv", row.names = FALSE)
# }