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
#' @return A data frame with one row per completed run and columns
#' including 'run_name', 'run_id', 'workflow_id', 'workflow_version', and
#' 'completed_at' (ISO 8601, UTC). The response does not list the samples
#' in each run.
#'
#' @examples
#' # Get only 10 completed runs
#' runs <- nf_get_completed_run(limit = 10)
#' 
#' @export
nf_get_completed_run <- function(limit = 250) {
    # Construct the API URL with the limit parameter
    api_url <- paste0(
        "https://nf-telemetry.seandavi.workers.dev/api/runs",
        "?status=completed&limit=", limit
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
    return(parsed_response$runs)
}