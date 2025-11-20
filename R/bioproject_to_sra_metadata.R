#' Get SRA Accessions and Metadata from BioProject ID
#'
#' @description
#' Retrieves all SRA (Sequence Read Archive) run accessions and associated 
#' metadata for a given BioProject ID from NCBI. Handles projects of any size
#' by using pagination and batch processing to overcome API limitations.
#'
#' @import rentrez
#' 
#' @param bioproject_id Character string. The BioProject accession ID 
#'   (e.g., "PRJNA123456"). Must be a valid NCBI BioProject identifier.
#' @param fetch_size Integer. 
#'
#' @return A list containing:
#'   \itemize{
#'     \item \code{bioproject_id}: The input BioProject ID
#'     \item \code{total_runs}: Total number of SRA runs found for this BioProject
#'     \item \code{retrieved_runs}: Number of runs successfully retrieved
#'     \item \code{sra_run_ids}: Character vector of SRA run accessions (SRR IDs)
#'     \item \code{full_metadata}: Data frame with complete metadata for all runs,
#'       including columns such as Run, ReleaseDate, LoadDate, spots, bases, 
#'       avgLength, size_MB, Experiment, LibraryName, LibraryStrategy, 
#'       LibrarySelection, LibrarySource, LibraryLayout, Platform, Model, 
#'       SRAStudy, BioProject, Sample, BioSample, SampleType, TaxID, 
#'       ScientificName, and more.
#'   }
#'   Returns NULL if no SRA runs are found for the BioProject.
#'
#' @details
#' This function queries NCBI's SRA database using the rentrez package. It:
#' \itemize{
#'   \item First determines the total number of SRA runs for the BioProject
#'   \item Retrieves all SRA IDs using pagination (batches of 9,999) if needed
#'   \item Fetches detailed metadata in batches of 500 runs to respect API limits
#'   \item Implements rate limiting (3 requests/second) to comply with NCBI guidelines
#'   \item Provides progress updates for large datasets
#' }
#'
#' @note
#' \itemize{
#'   \item Requires an active internet connection
#'   \item Processing time increases with the number of runs (approximately 
#'     0.34 seconds per batch due to rate limiting)
#'   \item For very large projects (10,000+ runs), consider running during 
#'     off-peak hours
#'   \item NCBI may throttle requests if rate limits are exceeded
#' }
#'
#' @examples
#' \dontrun{
#' # Retrieve SRA data for a BioProject
#' sra_result <- bioproject_to_sra_metadata("PRJNA123456")
#' 
#' # Check retrieval success
#' if (!is.null(sra_result)) {
#'   cat("Total runs:", sra_result$total_runs, "\n")
#'   cat("Retrieved runs:", sra_result$retrieved_runs, "\n")
#'   
#'   # View first few SRA Run IDs
#'   head(sra_result$sra_run_ids)
#'   
#'   # View metadata structure
#'   str(sra_result$full_metadata)
#'   
#'   # Access specific metadata columns
#'   summary(sra_result$full_metadata$spots)
#'   table(sra_result$full_metadata$Platform)
#'   
#'   # Save results to files
#'   write.csv(sra_result$full_metadata, 
#'             "sra_metadata.csv", 
#'             row.names = FALSE)
#'   write.table(sra_result$sra_run_ids, 
#'               "sra_run_ids.txt",
#'               row.names = FALSE, 
#'               col.names = FALSE, 
#'               quote = FALSE)
#' }
#' 
#' # Handle case with no results
#' sra_result <- bioproject_to_sra_metadata("PRJNA999999")
#' if (is.null(sra_result)) {
#'   cat("No SRA runs found for this BioProject\n")
#' }
#' }
#'
#' @seealso
#' \itemize{
#'   \item \code{\link[rentrez]{entrez_search}} for NCBI database searches
#'   \item \code{\link[rentrez]{entrez_fetch}} for retrieving records
#'   \item NCBI SRA: \url{https://www.ncbi.nlm.nih.gov/sra}
#'   \item NCBI E-utilities: \url{https://www.ncbi.nlm.nih.gov/books/NBK25501/}
#' }
#'
#' @author Your Name
#' @export
bioproject_to_sra_metadata <- function(bioproject_id, fetch_size = 100) {
  
  # Input validation
  if (!is.character(bioproject_id) || length(bioproject_id) != 1) {
    stop("bioproject_id must be a single character string")
  }
  
  if (!grepl("^PRJ[NED][A-Z][0-9]+$", bioproject_id)) {
    warning("bioproject_id does not match expected format (PRJNA/PRJD/PRJE followed by numbers)")
  }
  
  # First, get the total count
  initial_search <- entrez_search(
    db = "sra",
    term = paste0(bioproject_id, "[BioProject]"),
    retmax = 0  # Just get the count
  )
  
  n_runs <- initial_search$count
  cat(paste("Found", n_runs, "SRA runs for", bioproject_id, "\n"))
  
  if (n_runs == 0) {
    return(NULL)
  }
  
  # Collect all IDs using pagination (max 10,000 per query)
  all_ids <- character()
  batch_size <- 9999  # Use 9999 to be safe with NCBI limits
  
  if (n_runs > batch_size) {
    cat("Retrieving IDs in batches...\n")
    
    for (start in seq(0, n_runs - 1, by = batch_size)) {
      cat(paste("  Fetching IDs", start + 1, "to", min(start + batch_size, n_runs), "\n"))
      
      search_result <- entrez_search(
        db = "sra",
        term = paste0(bioproject_id, "[BioProject]"),
        retmax = batch_size,
        retstart = start,
        use_history = FALSE
      )
      
      all_ids <- c(all_ids, search_result$ids)
      Sys.sleep(0.34)  # NCBI rate limit: 3 requests per second
    }
  } else {
    # If less than batch_size, get all at once
    search_result <- entrez_search(
      db = "sra",
      term = paste0(bioproject_id, "[BioProject]"),
      retmax = n_runs
    )
    all_ids <- search_result$ids
  }
  
  cat(paste("\nRetrieved", length(all_ids), "ID(s)\n"))
  cat("Fetching metadata...\n")
  
  # Fetch metadata in batches (NCBI recommends max 500 per fetch)
  fetch_batch_size <- fetch_size
  all_metadata <- list()
  
  for (i in seq(1, length(all_ids), by = fetch_batch_size)) {
    end_idx <- min(i + fetch_batch_size - 1, length(all_ids))
    batch_ids <- all_ids[i:end_idx]
    
    cat(paste("  Fetching metadata for runs", i, "to", end_idx, "\n"))
    
    tryCatch({
      sra_data <- entrez_fetch(
        db = "sra",
        id = batch_ids,
        rettype = "runinfo",
        retmode = "text"
      )
      
      # Parse the CSV-formatted data
      batch_df <- read.csv(text = sra_data, stringsAsFactors = FALSE)
      all_metadata[[length(all_metadata) + 1]] <- batch_df
      
    }, error = function(e) {
      warning(paste("Error fetching batch", i, "to", end_idx, ":", e$message))
    })
    
    Sys.sleep(0.34)  # Rate limiting
  }
  
  # Combine all batches
  if (length(all_metadata) == 0) {
    stop("Failed to retrieve any metadata")
  }
  
  sra_df <- do.call(rbind, all_metadata)
  
  # Create a summary
  result <- list(
    bioproject_id = bioproject_id,
    total_runs = n_runs,
    retrieved_runs = nrow(sra_df),
    sra_run_ids = sra_df$Run,
    full_metadata = sra_df
  )
  
  cat(paste("\nSuccessfully retrieved", nrow(sra_df), "run(s)\n\n"))
  
  return(result)
}


# Example usage demonstration ----
if (FALSE) {  # Set to TRUE to run examples
  
  # Basic usage
  sra_result <- bioproject_to_sra_metadata("PRJNA123456")
  
  if (!is.null(sra_result)) {
    # Verify completeness
    cat(paste("Total runs:", sra_result$total_runs, "\n"))
    cat(paste("Retrieved runs:", sra_result$retrieved_runs, "\n"))
    
    # Display first 10 SRA Run IDs
    cat("\nFirst 10 SRA Run IDs:\n")
    print(head(sra_result$sra_run_ids, 10))
    
    # Explore metadata
    cat("\nMetadata summary:\n")
    print(str(sra_result$full_metadata))
    
    # Common metadata analyses
    cat("\nPlatform distribution:\n")
    print(table(sra_result$full_metadata$Platform))
    
    cat("\nLibrary strategy:\n")
    print(table(sra_result$full_metadata$LibraryStrategy))
    
    # Save results with descriptive filenames
    write.csv(sra_result$full_metadata, 
              paste0(sra_result$bioproject_id, "_sra_metadata.csv"), 
              row.names = FALSE)
    
    write.table(sra_result$sra_run_ids, 
                paste0(sra_result$bioproject_id, "_sra_run_ids.txt"),
                row.names = FALSE, 
                col.names = FALSE, 
                quote = FALSE)
    
    cat("\nFiles saved successfully!\n")
  } else {
    cat("No SRA data found for this BioProject\n")
  }
}