#' Get Completed Runs from cMD MetaPhlAn4 Nextflow Telemetry API
#'
#' This function makes an API request to the Nextflow telemetry service
#' to retrieve information about completed runs.
#' 
#' @import httr
#' @import jsonlite
#'
#' @param limit Integer. Maximum number of runs to retrieve. Default is 250.
#' 
#' @return A data frame containing the API response. There are four columns
#' on 'run_id', 'run_name', 'utc_time', and 'sample_id'.
#'
#' @examples
#' # Get only 10 completed runs
#' runs <- nf_get_completed_run(limit = 10)
#' 
nf_get_completed_run <- function(limit = 250) {
    # Construct the API URL with the limit parameter
    api_url <- paste0(
        "https://nf-telemetry-819875667022.us-central1.run.app/nextflow-telemetry/completed_runs",
        "?limit=", limit
    )
    
    # Make the GET request
    response <- httr::GET(
        url = api_url,
        httr::add_headers(accept = "application/json")
    )
    
    # Check if request was successful
    if (httr::http_status(response)$category != "Success") {
        stop("API request failed with status: ", httr::http_status(response)$message)
    }
    
    # Parse and return the JSON response
    parsed_response <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
    return(parsed_response)
}