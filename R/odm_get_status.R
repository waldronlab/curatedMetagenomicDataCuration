#' Check Metadata Curation Status in ODM
#'
#' @description
#' This function interfaces with the Omics Data Manager (ODM) API to check 
#' the validation status of sample metadata for a specific study. It provides 
#' options to get detailed validation information or just a simple 
#' valid/invalid status.
#' 
#' @import httr
#' @import jsonlite
#'
#' @param host Character string. The ODM host URL.
#' @param token Character string. Your ODM API token for authentication.
#' @param study_id Character string. Optional study accession ID (e.g., "GSF004054"). 
#' If NULL, returns a list of all available studies.
#' @param page_limit Numeric. Maximum number of studies to return when 
#' listing all studies. Default is 2000.
#' @param validation_only Logical. If TRUE, returns just "Valid" or "Invalid" 
#' status. If FALSE, returns the complete validation data. Default is TRUE.
#'
#' @return 
#' If validation_only is TRUE: Prints "Valid" or "Invalid".
#' If validation_only is FALSE: Returns a list with detailed
#' validation information for the specified study.
#'
#' The validation information includes attributes with invalid values, error types,
#' error messages, and the count of samples with each error.
#' 
#' 
odm_get_status <- function(host, 
                           token, 
                           study_id = NULL, 
                           page_limit = 2000,
                           validation_only = TRUE) {
  
  # Validate inputs
  if (!is.character(study_id) || length(study_id) != 1) {
    stop("study_id must be a single character string")
  }
  if (!is.character(token) || length(token) != 1) {
    stop("token must be a single character string")
  }
  
  # Base URL
  base_url <- paste0(host, "/api/v1/as-curator")
  
  # Headers for API calls
  headers <- c(
    "accept" = "application/json",
    "Genestack-API-Token" = token
  )
  
  # Get validation summary for a specific study
  validation_url <- paste0(base_url, "/integration/studies/", study_id, "/validation-summary")
  response <- httr::GET(validation_url, httr::add_headers(.headers = headers))
  
  if (httr::status_code(response) != 200) {
    stop("Error fetching validation summary. Status code: ", httr::status_code(response))
  }
  
  validation_data <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
  
  if (validation_only) {
    invalids <- validation_data$samples$attributes[[1]]
    
    if (is.null(nrow(invalids))) {
      print("Valid")
    } else {
      print("Invalid")
    }
  } else {
    return(validation_data)
  }
}