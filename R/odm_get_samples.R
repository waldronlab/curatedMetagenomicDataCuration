# Function to clean field names
clean_field_name <- function(field_name) {
  # Remove backticks, replace colons and spaces with underscores
  cleaned <- gsub("`", "", field_name)
  cleaned <- gsub(":", "_", cleaned)
  cleaned <- gsub(" ", "_", cleaned)
  cleaned <- tolower(cleaned)
  return(cleaned)
}


#' Get samples by Genestack study accession
#' 
#' This function retrieves samples from the Genestack API, filters them by 
#' Genestack study accession.
#' 
#' @import httr
#' @import jsonlite
#' @import lubridate
#' 
#' @param host The host URL for the Genestack API
#' @param token The API token for authentication
#' @param study_id A character(1). Genestack accession id for the study.
#' @param page_limit The number of samples to retrieve per page 
#' (default: 100). The maximum is 2,000.
#' @param offset Show the page {pageOffset+1} results from the start of 
#' the results. (e.g., 100 will show a page of results starting from the 
#' 101st result (default: 0). To return all results iterate through pages 
#' using `offset` values of `n * page_limit` until it surpaces the total
#' number of samples. 
#' 
#' @return A data frame containing sample metadata. A message shows how many 
#' samples are retrieved out of the total samples.
#' 
#' @export
odm_get_samples <- function(host = "https://odm.drylab.tech",
                            token,
                            study_id = NULL,
                            page_limit = 100,
                            offset = 0) {
  
  # Validate inputs
  if (!is.character(study_id) || length(study_id) != 1) {
    stop("study_id must be a single character string")
  }
  if (!is.character(token) || length(token) != 1) {
    stop("token must be a single character string")
  }
  
  # Construct the URL
  base_url <- file.path(host, "api/v1/as-user/integration/link/samples/by/study/")
  url <- paste0(base_url, study_id)
  
  # Set up headers
  headers <- c(
    "accept" = "application/json",
    "Genestack-API-Token" = token
  )
  
  # Make the API call using httr package
  samples_url <- paste0(url, "?pageLimit=", page_limit, "&pageOffset=", offset)
  response <- tryCatch({
    httr::GET(samples_url, httr::add_headers(.headers = headers))
  }, error = function(e) {
    stop(paste("API request failed:", e$message))
  })
  
  # Check status code
  status <- httr::status_code(response)
  if (status != 200) {
    stop(paste("API request failed with status code:", status))
  }
  
  # Parse and return the JSON response
  content <- httr::content(response, "text", encoding = "UTF-8")
  result <- jsonlite::fromJSON(content)
  
  # Pagination
  pagination <- result$meta$pagination
  msg <- paste("Returning", pagination$count, "out of", pagination$total, 
               "samples.")
  data <- result$data
  
  # Clean up column names
  colnames(data) <- clean_field_name(colnames(data))
  
  message(msg)
  return(data)
}