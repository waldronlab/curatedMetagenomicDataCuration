# Function to clean field names
.clean_field_name <- function(field_name) {
  # Remove backticks, replace colons and spaces with underscores
  cleaned <- gsub("`", "", field_name)
  cleaned <- gsub(":", "_", cleaned)
  cleaned <- gsub(" ", "_", cleaned)
  cleaned <- tolower(cleaned)
  return(cleaned)
}

#' Fetch and Parse Data from Genestack ODM API
#'
#' Retrieves sample data for a specified study from the Genestack ODM API and
#' parses the JSON response into a structured wide-format data frame. The function
#' handles API authentication, response validation, and automatic parsing of 
#' nested JSON structures including ontology terms.
#'
#' @import httr
#' @import jsonlite
#' @import dplyr
#' @import tidyr
#' @import stringr
#'
#' @param study_id Character. The unique identifier for the study in Genestack 
#'   ODM (e.g., "GSF004467").
#' @param api_token Character. Your Genestack API authentication token.
#' @param base_url Character. The base URL for the API endpoint. Default is
#'   "https://odm.drylab.tech/api/v1/as-user/integration/link/samples/by/study"
#'   for exporting sample-level metadata for study
#' @param response_format Character. Format for the API response. Default is 
#'   "term_id" which includes ontology term identifiers. (No other options are
#'   available at this point.)
#' @param page_limit Integer. Maximum number of records to return per page. 
#'   Default is 100. The maximum is 2,000.
#' @param metadata_fields Character. Specifies which metadata fields to include
#'   in the response. Valid options are:
#'   \itemize{
#'     \item \code{"minimal_data"} - Returns only metadata in accordance with 
#'           the default template
#'     \item \code{"extended_data_included"} - Returns metadata in accordance 
#'           with the applied template (default)
#'     \item \code{"original_data_included"} - Returns all metadata attributes
#'   }
#'   Note: Legacy values "template" (equivalent to "extended_data_included") and 
#'   "all" (equivalent to "original_data_included") are still supported for 
#'   backwards compatibility.
#'
#' @return A list containing two elements:
#'   \itemize{
#'     \item \code{metadata}: API response metadata including pagination info
#'     \item \code{data}: A data frame in wide format with samples as rows and 
#'           attributes as columns. Ontology terms are stored in separate columns
#'           with "_ontology_term_id" suffix
#'   }
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Constructs the API URL with the provided study_id
#'   \item Makes an authenticated GET request with specified query parameters
#'   \item Validates the response status code
#'   \item Parses the JSON response into a structured format
#'   \item Converts nested field structures into wide-format columns
#'   \item Extracts and formats ontology term IDs (converting underscore to colon)
#' }
#'
#' For fields with multiple values, column names are suffixed with sequential 
#' numbers (e.g., "field_name_2", "field_name_3").
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' api_token <- "your_api_token_here"
#' study_id <- "GSF004467"
#' 
#' result <- odm_get_samples(
#'   study_id = study_id,
#'   api_token = api_token
#' )
#' 
#' # Access the data
#' df <- result$data
#' metadata <- result$metadata
#' 
#' # View summary
#' head(df)
#' str(df)
#' 
#' # Custom parameters
#' result <- odm_get_samples(
#'   study_id = "GSF004467",
#'   api_token = api_token,
#'   page_limit = 200,
#'   response_format = "term_id"
#' )
#' }
#'
#' @seealso \code{\link{parse_response_data}} for the internal parsing logic
#'
#' @export
odm_get_samples <- function(study_id, 
                            api_token,
                            base_url = "https://odm.drylab.tech/api/v1/as-user/integration/link/samples/by/study",
                            response_format = "term_id",
                            page_limit = 100,
                            metadata_fields = "extended_data_included") {
  
  # Construct the full URL
  url <- paste0(base_url, "/", study_id)
  
  # Make the API call
  response <- GET(
    url = url,
    query = list(
      responseFormat = response_format,
      pageLimit = page_limit,
      returnedMetadataFields = metadata_fields
    ),
    add_headers(
      "accept" = "application/json",
      "Genestack-API-Token" = api_token
    )
  )
  
  # Check if request was successful
  if (status_code(response) != 200) {
    stop(paste("API request failed with status code:", status_code(response),
               "\nResponse:", content(response, "text")))
  }

  # Parse the response
  data <- content(response, "parsed", simplifyVector = FALSE)
  
  # Parse using the internal parsing logic
  result <- parse_response_data(data)
  
  # cat("Data parsed successfully!\n")
  # cat("Number of records:", nrow(result$data), "\n")
  # cat("Number of columns:", ncol(result$data), "\n")
  
  return(result)
}


#' Parse Genestack ODM API Response Data into Wide Format
#'
#' Internal function that transforms the nested JSON structure returned by the
#' Genestack ODM API into a flat, wide-format data frame suitable for analysis.
#' Handles multi-valued fields, ontology terms, and type conversions.
#'
#' @param data List. The parsed JSON response from the Genestack ODM API,
#'   typically obtained via \code{httr::content(response, "parsed")}.
#'
#' @return A list containing two elements:
#'   \itemize{
#'     \item \code{metadata}: The metadata component from the API response
#'     \item \code{data}: A data frame where each row represents a sample record
#'           and columns represent field values and ontology terms
#'   }
#'
#' @details
#' The function processes the API response structure as follows:
#' 
#' \strong{Field Processing:}
#' \itemize{
#'   \item Each field's values array is extracted and flattened
#'   \item For fields with multiple values, columns are created with numeric 
#'         suffixes (e.g., "field_1", "field_2")
#'   \item Ontology term IDs are extracted into separate columns with 
#'         "_ontology_term_id" suffix
#'   \item Term IDs are reformatted from "PREFIX_12345" to "PREFIX:12345"
#' }
#' 
#' \strong{Data Type Handling:}
#' All values are initially converted to character type to ensure successful
#' binding of rows. The comment in the code indicates that specific columns
#' should be converted back to appropriate types (numeric, date, etc.) in 
#' downstream processing.
#' 
#' \strong{Column Naming Convention:}
#' \itemize{
#'   \item Single values: \code{field_name}
#'   \item Multiple values: \code{field_name}, \code{field_name_2}, \code{field_name_3}, ...
#'   \item Ontology terms: \code{field_name_ontology_term_id}, \code{field_name_ontology_term_id_2}, ...
#' }
#'
#' @note
#' This is an internal function called by \code{odm_get_samples}. It is not
#' intended to be called directly by users but is documented for maintainability.
#' 
#' Dictionary information extraction is currently commented out but can be
#' enabled if needed by uncommenting the relevant code section.
#'
#' @section Known Limitations:
#' \itemize{
#'   \item All columns are returned as character type and require explicit type
#'         conversion for numeric or date fields
#'   \item Dictionary metadata is not currently extracted (code is commented out)
#' }
#'
#' @seealso \code{\link{odm_get_samples}} for the main API fetching function
#'
#' @keywords internal
parse_response_data <- function(data) {
  
  # Extract metadata
  metadata <- data$meta
  
  # Process each record in the data array
  records_list <- lapply(data$data, function(record) {
    # Initialize empty list for this record
    record_data <- list()
    
    # Process each field in the record
    for (field_name in names(record)) {
      field <- record[[field_name]]
      
      # Check if field has values
      if (!is.null(field$values) && length(field$values) > 0) {
        # Process each value in the values array
        for (i in seq_along(field$values)) {
          value_item <- field$values[[i]]
          
          # Extract the value
          if (!is.null(value_item$value)) {
            value_col_name <- field_name
            if (i > 1) {
              value_col_name <- paste0(value_col_name, "_", i)
            }
            record_data[[value_col_name]] <- value_item$value
          }
          
          # Extract the termId if it exists
          if (!is.null(value_item$termId)) {
            termid_col_name <- paste0(field_name, "_ontology_term_id")
            if (i > 1) {
              termid_col_name <- paste0(termid_col_name, "_", i)
            }
            ontology_term_id <- basename(value_item$termId) %>% 
              stringr::str_replace(., "_", ":")
            record_data[[termid_col_name]] <- ontology_term_id
          }
        }
      }
      
      # # Extract dictionary information if present
      # if (!is.null(field$dictionary) && !is.null(field$dictionary$name)) {
      #   dict_col_name <- paste0(field_name, "_dictionary")
      #   record_data[[dict_col_name]] <- field$dictionary$name
      # }
    }
    
    return(record_data)
  })
  
  # Convert all values to character before binding
  # Note: Specific columns should be converted back to appropriate types 
  # (numeric, date, etc.) in downstream processing
  df <- bind_rows(lapply(records_list, function(x) {
    lapply(x, as.character)
  }))
  colnames(df) <- .clean_field_name(colnames(df))
  
  # Return list with metadata and data
  result <- list(
    metadata = metadata,
    data = df
  )
  
  return(result)
}
