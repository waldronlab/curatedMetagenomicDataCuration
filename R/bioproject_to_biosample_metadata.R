# Function to get BioSample IDs from a BioProject ID
.get_biosamples_from_bioproject <- function(bioproject_id) {
  tryCatch({
    # Search for BioSamples linked to the BioProject
    search_result <- entrez_search(
      db = "biosample",
      term = paste0(bioproject_id, "[BioProject]"),
      retmax = 10000
    )
    
    return(search_result$ids)
  }, error = function(e) {
    message(paste("Error fetching BioSamples for", bioproject_id, ":", e$message))
    return(NULL)
  })
}

# Function to extract metadata from BioSample XML
.parse_biosample_xml <- function(xml_content) {
  doc <- read_xml(xml_content)
  
  # Extract basic information
  biosample_id <- xml_text(xml_find_first(doc, ".//BioSample/@id"))
  accession <- xml_text(xml_find_first(doc, ".//BioSample/@accession"))
  
  # Extract organism information
  organism <- xml_text(xml_find_first(doc, ".//Organism/OrganismName"))
  taxonomy_id <- xml_text(xml_find_first(doc, ".//Organism/@taxonomy_id"))
  
  # Extract attributes
  attributes <- xml_find_all(doc, ".//Attribute")
  attr_list <- list()
  
  for (attr in attributes) {
    attr_name <- xml_attr(attr, "attribute_name")
    attr_value <- xml_text(attr)
    if (!is.na(attr_name) && attr_name != "") {
      attr_list[[attr_name]] <- attr_value
    }
  }
  
  # Combine all metadata
  metadata <- c(
    list(
      biosample_id = biosample_id,
      accession = accession,
      organism = organism,
      taxonomy_id = taxonomy_id
    ),
    attr_list
  )
  
  return(metadata)
}



#' BioProject ID to BioSample Metadata Converter
#'
#' This script retrieves BioSample metadata from BioProject IDs using NCBI E-utilities
#' 
#' @import rentrez 
#' @import xml2 
#' @import dplyr 
#' 
#' @param bioproject_id 
#' @param max_samples
#' 
#' @examples 
#' # Single BioProject
#' metadata <- bioproject_to_biosample_metadata("PRJNA13")
#' 
#' # Limit to first 50 samples
#' metadata <- bioproject_to_biosample_metadata("PRJEB9576", max_samples = 50)
#' 
#' # Multiple BioProjects
#' bioproject_ids <- c("PRJNA13", "PRJEB9576")
#' all_metadata <- lapply(bioproject_ids, function(id) {
#'   bioproject_to_biosample_metadata(id, max_samples = 50)
#' })
#' combined_metadata <- bind_rows(all_metadata)
#' 
#' @export
bioproject_to_biosample_metadata <- function(bioproject_id, 
                                             max_samples = NULL) {
    message(paste("Processing BioProject:", bioproject_id))
    
    # Get BioSample IDs
    biosample_ids <- .get_biosamples_from_bioproject(bioproject_id)
    
    if (is.null(biosample_ids) || length(biosample_ids) == 0) {
        message("No BioSamples found for this BioProject")
        return(NULL)
    }
    
    message(paste("Found", length(biosample_ids), "BioSamples"))
    
    # Limit number of samples if specified
    if (!is.null(max_samples)) {
        biosample_ids <- head(biosample_ids, max_samples)
        message(paste("Limiting to first", max_samples, "samples"))
    }
    
    # Fetch metadata for each BioSample
    all_metadata <- list()
    
    for (i in seq_along(biosample_ids)) {
        if (i %% 10 == 0) {
            message(paste("Processing sample", i, "of", length(biosample_ids)))
        }
        
        tryCatch({
            # Fetch BioSample record
            biosample_xml <- entrez_fetch(
                db = "biosample",
                id = biosample_ids[i],
                rettype = "xml"
            )
            
            # Parse XML and extract metadata
            metadata <- .parse_biosample_xml(biosample_xml)
            metadata$bioproject_id <- bioproject_id
            all_metadata[[i]] <- metadata
            
            # Be respectful to NCBI servers
            Sys.sleep(0.34)  # ~3 requests per second
            
        }, error = function(e) {
            message(paste("Error processing BioSample", biosample_ids[i], ":", e$message))
        })
    }
    
    # Convert list to data frame
    if (length(all_metadata) > 0) {
        # Find all unique column names
        all_cols <- unique(unlist(lapply(all_metadata, names)))
        
        # Create a data frame with all columns
        df <- data.frame(matrix(ncol = length(all_cols), nrow = length(all_metadata)))
        colnames(df) <- all_cols
        
        # Fill in the data
        for (i in seq_along(all_metadata)) {
            for (col in names(all_metadata[[i]])) {
                df[i, col] <- all_metadata[[i]][[col]]
            }
        }
        
        return(df)
    }
    
    return(NULL)
}