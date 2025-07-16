## Function to extract a specific value from a nested list structure ----------
extract_value <- function(study_item, field_name) {
  # Check if the field exists
  if (!field_name %in% names(study_item)) {
    return(NA)
  }
  
  # Try to extract values
  values <- study_item[[field_name]]$values
  if (length(values) == 0) {
    return(NA)
  }
  
  # Extract the first value if it exists
  if (length(values) > 0 && !is.null(values[[1]]$value)) {
    return(values[[1]]$value)
  } else {
    return(NA)
  }
}

## Function to clean field names --------------------------
clean_field_name <- function(field_name) {
  # Remove backticks, replace colons and spaces with underscores
  cleaned <- gsub("`", "", field_name)
  cleaned <- gsub(":", "_", cleaned)
  cleaned <- gsub(" ", "_", cleaned)
  return(cleaned)
}

## Function to process each study in the list --------------------
process_study <- function(study) {
  # Get all field names across all studies
  all_fields <- names(study)
  
  # Initialize a list to store values
  values_list <- list()
  
  # Extract values for each field
  for (field in all_fields) {
    # Create a clean column name
    clean_field <- clean_field_name(field)
    values_list[[clean_field]] <- extract_value(study, field)
  }
  
  # Convert to data frame
  result <- as.data.frame(values_list, stringsAsFactors = FALSE)
  return(result)
}

## Get all unique field names from all studies ------------------
get_all_fields <- function(study_data) {
  all_fields <- c()
  for (study in study_data) {
    all_fields <- union(all_fields, names(study))
  }
  return(all_fields)
}

## Process all studies in the list --------------------
create_study_table <- function(study_data) {
  # Get all unique fields
  all_fields <- get_all_fields(study_data)
  clean_fields <- sapply(all_fields, clean_field_name)
  
  # Initialize an empty data frame with all clean fields
  result <- data.frame(matrix(ncol = length(clean_fields), nrow = 0))
  colnames(result) <- clean_fields
  
  # Create a mapping between original and clean field names
  field_mapping <- setNames(clean_fields, all_fields)
  
  # Process each study and bind them together
  for (i in seq_along(study_data)) {
    study_row <- process_study(study_data[[i]])
    
    # Make sure the study_row has all columns (with NA for missing ones)
    for (orig_field in all_fields) {
      clean_field <- field_mapping[orig_field]
      if (!clean_field %in% colnames(study_row)) {
        study_row[clean_field] <- NA
      }
    }
    
    # Ensure columns are in the same order
    study_row <- study_row[, clean_fields]
    
    # Bind to result
    result <- rbind(result, study_row)
  }
  
  return(result)
}

#' Get the list of studies in ODM
#' 
#' 
#' 
#' 
odm_get_studies <- function(api_token) {
  # Load required library
  if (!require("httr")) {
    install.packages("httr")
    library(httr)
  }
  
  # API endpoint
  url <- "https://odm.drylab.tech/api/v1/as-curator/studies"
  
  # Set up request headers
  headers <- c(
    "accept" = "application/json",
    "Genestack-API-Token" = api_token
  )
  
  # Make the GET request
  response <- GET(
    url = url,
    query = list(responseFormat = "term_id"),
    add_headers(.headers = headers)
  )
  
  # Check if the request was successful
  if (http_status(response)$category == "Success") {
    # Parse and return the JSON response
    resp <- content(response, "parsed")
  } else {
    # Return error information
    stop(sprintf(
      "API request failed with status code %s: %s",
      status_code(response),
      http_status(response)$reason
    ))
  }
  
  # Format the JSON response as data table
  res <- create_study_table(resp$data)
  return(res)
}