#' Query Ontology Lookup Service (OLS) for ontology terms
#'
#' @param ontology_ids Character vector of ontology IDs (e.g., "NCIT:C7057")
#' @param relationship Type of relationship: "children" or "descendant"
#' @param size_threshold Threshold for "large" enums (default: 1000)
#' @return Character vector of ontology terms (label|ID format) or NULL if too large
#' @importFrom httr2 request req_perform resp_body_json req_url_query req_error
#' @export
query_ols <- function(ontology_ids, relationship = "children", size_threshold = 1000) {
  # Check for required packages
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("Package 'httr2' is required. Please install it with: install.packages('httr2')")
  }

  if (is.na(ontology_ids) || ontology_ids == "" || length(ontology_ids) == 0) {
    return(NULL)
  }

  # Handle NA relationship
  if (is.na(relationship) || relationship == "" || length(relationship) == 0) {
    warning(paste("No relationship specified for ontology", ontology_ids))
    return(NULL)
  }

  # Split multiple ontology IDs if semicolon-separated
  ids <- strsplit(ontology_ids, ";")[[1]]
  all_terms <- c()

  for (ontology_id in ids) {
    # Parse ontology ID (e.g., "NCIT:C7057" -> ontology="ncit", id="C7057")
    parts <- strsplit(ontology_id, ":")[[1]]
    if (length(parts) != 2) {
      warning(paste("Invalid ontology ID format:", ontology_id))
      next
    }

    ontology <- tolower(parts[1])
    term_id <- parts[2]

    # Build OLS API URL based on relationship type
    # OLS4 requires double URL encoding for the IRI
    base_url <- "http://www.ebi.ac.uk/ols4/api/ontologies"
    iri <- paste0("http://purl.obolibrary.org/obo/", gsub(":", "_", ontology_id))
    # Double encode: encode once, then manually encode the percent signs
    encoded_once <- URLencode(iri, reserved = TRUE)
    encoded_iri <- gsub("%", "%25", encoded_once, fixed = TRUE)

    if (relationship == "children") {
      endpoint <- paste0(base_url, "/", ontology, "/terms/", encoded_iri, "/children")
    } else if (relationship == "descendant") {
      endpoint <- paste0(base_url, "/", ontology, "/terms/", encoded_iri, "/descendants")
    } else {
      warning(paste("Unknown relationship type:", relationship))
      next
    }

    tryCatch({
      # Query OLS API with pagination
      terms <- c()
      page <- 0
      repeat {
        resp <- httr2::request(endpoint) |>
          httr2::req_url_query(size = 500, page = page) |>
          httr2::req_error(is_error = function(resp) FALSE) |>
          httr2::req_perform()

        if (resp$status_code != 200) {
          warning(paste("OLS API returned status", resp$status_code, "for", ontology_id))
          break
        }

        data <- httr2::resp_body_json(resp, simplifyVector = TRUE)

        if (is.null(data$`_embedded`) || is.null(data$`_embedded`$terms)) {
          break
        }

        page_terms <- data$`_embedded`$terms

        # Extract label and short_form (ID)
        for (i in seq_len(nrow(page_terms))) {
          label <- page_terms$label[i]
          short_form <- page_terms$short_form[i]
          if (!is.na(label) && !is.na(short_form)) {
            terms <- c(terms, paste0(label, "|", short_form))
          }
        }

        # Check if we've exceeded the threshold
        if (length(terms) > size_threshold) {
          message(paste("Ontology", ontology_id, "has more than", size_threshold,
                       "terms (found", length(terms),
                       "). Will use existing values from curated metadata instead."))
          return(NULL)
        }

        # Check if there are more pages
        if (is.null(data$page) || data$page$number >= data$page$totalPages - 1) {
          break
        }

        page <- page + 1
      }

      all_terms <- c(all_terms, terms)

    }, error = function(e) {
      warning(paste("Error querying OLS for", ontology_id, ":", conditionMessage(e)))
    })
  }

  if (length(all_terms) == 0) {
    return(NULL)
  }

  unique(all_terms)
}


#' Get unique values from curated metadata for a specific column
#'
#' @param metadata_file Path to the curated metadata CSV file. Defaults to the
#'   installed package copy in `extdata`.
#' @param column_name Name of the column to extract unique values from
#' @return Character vector of unique values (excluding NA)
#' @importFrom readr read_csv
#' @export
get_curated_values <- function(
  metadata_file = system.file(
    "extdata",
    "cMD_curated_metadata_release.csv",
    package = "curatedMetagenomicDataCuration"
  ),
  column_name
) {
  # Check for required packages
  if (!requireNamespace("readr", quietly = TRUE)) {
    stop("Package 'readr' is required. Please install it with: install.packages('readr')")
  }

  if (!file.exists(metadata_file)) {
    warning(paste("Metadata file not found:", metadata_file))
    return(character(0))
  }

  tryCatch({
    # Read only the specific column to save memory
    metadata <- readr::read_csv(metadata_file,
                        col_select = dplyr::all_of(column_name),
                        show_col_types = FALSE)

    values <- unique(metadata[[column_name]])
    values <- values[!is.na(values) & values != "" & values != "NA"]

    # Handle multi-value fields (separated by semicolon)
    if (any(grepl(";", values))) {
      values <- unlist(strsplit(as.character(values), ";"))
      values <- unique(trimws(values))
      values <- values[values != "" & values != "NA"]
    }

    sort(as.character(values))
  }, error = function(e) {
    warning(paste("Error reading metadata for column", column_name, ":", conditionMessage(e)))
    character(0)
  })
}


#' Generate validation lists for all columns based on data dictionary
#'
#' @param dict_file Path to the data dictionary CSV file. Defaults to the
#'   installed package copy in `extdata`.
#' @param metadata_file Path to the curated metadata CSV file. Defaults to the
#'   installed package copy in `extdata`.
#' @param ols_size_threshold Threshold for switching from OLS to curated values (default: 1000)
#' @return Named list where each element is a character vector of allowed values
#' @export
generate_validation_lists <- function(
  dict_file = system.file(
    "extdata",
    "cMD_data_dictionary.csv",
    package = "curatedMetagenomicDataCuration"
  ),
  metadata_file = system.file(
    "extdata",
    "cMD_curated_metadata_release.csv",
    package = "curatedMetagenomicDataCuration"
  ),
  ols_size_threshold = 1000
) {
  dict <- read.csv(dict_file, stringsAsFactors = FALSE)
  validation_lists <- list()

  for (i in seq_len(nrow(dict))) {
    col_name <- dict$col.name[i]
    corpus_type <- dict$corpus.type[i]

    # Skip if not an enum or binary type
    if (is.na(corpus_type) || (!grepl("enum", corpus_type) && corpus_type != "binary")) {
      next
    }

    message(paste("Processing column:", col_name, "(", corpus_type, ")"))

    if (corpus_type == "static_enum" || corpus_type == "custom_enum" || corpus_type == "binary") {
      # Static enum: parse from allowedvalues
      allowed_values <- dict$allowedvalues[i]
      if (!is.na(allowed_values) && allowed_values != "" && allowed_values != "NA") {
        values <- strsplit(allowed_values, "\\|")[[1]]

        # If static.enum has ontology IDs, format as "label|ID"
        if (!is.na(dict$static.enum[i]) && dict$static.enum[i] != "") {
          ontology_ids <- strsplit(dict$static.enum[i], "\\|")[[1]]
          if (length(values) == length(ontology_ids)) {
            values <- paste(values, ontology_ids, sep = "|")
          }
        }

        validation_lists[[col_name]] <- values
      }

    } else if (corpus_type == "dynamic_enum") {
      # Dynamic enum: query OLS
      dynamic_enum <- dict$dynamic.enum[i]
      dynamic_enum_property <- dict$dynamic.enum.property[i]

      # Skip if dynamic enum info is missing
      if (is.na(dynamic_enum) || dynamic_enum == "" || dynamic_enum == "NA") {
        message(paste("  Skipping", col_name, "- no dynamic enum specified"))
        next
      }

      # Use default relationship if not specified
      if (is.na(dynamic_enum_property) || dynamic_enum_property == "" || dynamic_enum_property == "NA") {
        dynamic_enum_property <- "descendant"
        message(paste("  Using default relationship 'descendant' for", col_name))
      }

      if (!is.na(dynamic_enum) && dynamic_enum != "" && dynamic_enum != "NA") {
        ols_terms <- query_ols(dynamic_enum, dynamic_enum_property, ols_size_threshold)

        if (is.null(ols_terms)) {
          # OLS returned too many terms or failed, use curated metadata
          message(paste("  Using existing values from curated metadata for", col_name))
          values <- get_curated_values(metadata_file, col_name)
        } else {
          message(paste("  Found", length(ols_terms), "terms from OLS for", col_name))
          values <- ols_terms
        }

        if (length(values) > 0) {
          validation_lists[[col_name]] <- values
        }
      }
    } else if (corpus_type == "dynamic_enum;static_enum") {
      # Mixed: combine both OLS and static enum
      # First get static enum
      static_values <- c()
      if (!is.na(dict$static.enum[i]) && dict$static.enum[i] != "") {
        allowed_values <- dict$allowedvalues[i]
        if (!is.na(allowed_values) && allowed_values != "") {
          static_labels <- strsplit(allowed_values, "\\|")[[1]]
          ontology_ids <- strsplit(dict$static.enum[i], "\\|")[[1]]
          if (length(static_labels) == length(ontology_ids)) {
            static_values <- paste(static_labels, ontology_ids, sep = "|")
          }
        }
      }

      # Then get dynamic enum
      dynamic_values <- c()
      dynamic_enum <- dict$dynamic.enum[i]
      dynamic_enum_property <- dict$dynamic.enum.property[i]

      # Use default relationship if not specified
      if (is.na(dynamic_enum_property) || dynamic_enum_property == "" || dynamic_enum_property == "NA") {
        dynamic_enum_property <- "descendant"
      }

      if (!is.na(dynamic_enum) && dynamic_enum != "" && dynamic_enum != "NA") {
        ols_terms <- query_ols(dynamic_enum, dynamic_enum_property, ols_size_threshold)

        if (is.null(ols_terms)) {
          message(paste("  Using existing values from curated metadata for", col_name))
          dynamic_values <- get_curated_values(metadata_file, col_name)
        } else {
          message(paste("  Found", length(ols_terms), "terms from OLS for", col_name))
          dynamic_values <- ols_terms
        }
      }

      # Combine both
      values <- unique(c(static_values, dynamic_values))
      if (length(values) > 0) {
        validation_lists[[col_name]] <- values
      }
    }
  }

  validation_lists
}


#' Create Excel data entry file with data validation
#'
#' @param dict_file Path to the data dictionary CSV file. Defaults to the
#'   installed package copy in `extdata`.
#' @param metadata_file Path to the curated metadata CSV file. Defaults to the
#'   installed package copy in `extdata`.
#' @param output_file Path for the output Excel file. Defaults to a file in the
#'   current working directory.
#' @param ols_size_threshold Threshold for switching from OLS to curated values (default: 1000)
#' @param example_data Logical; if TRUE, prefill rows 3-6 and columns 1-10 in
#'   `Curator_Entry` with demonstration values (default: TRUE)
#' @importFrom openxlsx2 wb_workbook wb_add_worksheet wb_add_data wb_add_data_validation
#' @importFrom openxlsx2 wb_set_col_widths wb_save
#' @export
create_data_entry_excel <- function(
  dict_file = system.file(
    "extdata",
    "cMD_data_dictionary.csv",
    package = "curatedMetagenomicDataCuration"
  ),
  metadata_file = system.file(
    "extdata",
    "cMD_curated_metadata_release.csv",
    package = "curatedMetagenomicDataCuration"
  ),
  output_file = file.path(getwd(), "cMD_data_entry_generated.xlsx"),
  ols_size_threshold = 1000,
  example_data = TRUE
) {

  # Check if openxlsx2 is available
  if (!requireNamespace("openxlsx2", quietly = TRUE)) {
    stop("Package 'openxlsx2' is required. Please install it with: install.packages('openxlsx2')")
  }

  message("Reading data dictionary...")
  dict <- read.csv(dict_file, stringsAsFactors = FALSE)

  # Sort by display.group first, then by display.order within each group
  if ("display.group" %in% colnames(dict) && "display.order" %in% colnames(dict)) {
    dict <- dict[order(dict$display.group, dict$display.order), ]
  } else if ("display.order" %in% colnames(dict)) {
    dict <- dict[order(dict$display.order), ]
  }

  message("Generating validation lists...")
  validation_lists <- generate_validation_lists(dict_file, metadata_file, ols_size_threshold)

  message("Creating Excel workbook...")
  wb <- openxlsx2::wb_workbook()

  # Number of _pick columns for multi-value fields
  num_pick_cols <- 5
  max_curator_row <- 10000
  max_export_row <- 3001

  # Create column mapping for Curator_Entry (with _pick columns)
  curator_cols <- list()
  curator_col_idx <- 1

  for (i in seq_len(nrow(dict))) {
    col_name <- dict$col.name[i]
    corpus_type <- dict$corpus.type[i]
    multiplevalues <- if ("multiplevalues" %in% colnames(dict)) dict$multiplevalues[i] else FALSE
    display_group <- if ("display.group" %in% colnames(dict)) dict$display.group[i] else ""

    # Convert multiplevalues to logical if it's not already
    if (!is.logical(multiplevalues)) {
      multiplevalues <- as.character(multiplevalues) == "TRUE" || as.character(multiplevalues) == "true"
    }

    if (multiplevalues) {
      # Multi-value field: create _pick1, _pick2, etc.
      for (j in 1:num_pick_cols) {
        pick_col <- paste0(col_name, "_pick", j)
        curator_cols[[length(curator_cols) + 1]] <- list(
          curator_name = pick_col,
          base_name = col_name,
          pick_num = j,
          dict_idx = i,
          display_group = display_group
        )
      }
    } else if (!is.na(corpus_type) && corpus_type == "dynamic_enum") {
      # Single-value dynamic enum: create _pick column (no number)
      pick_col <- paste0(col_name, "_pick")
      curator_cols[[length(curator_cols) + 1]] <- list(
        curator_name = pick_col,
        base_name = col_name,
        pick_num = NA,
        dict_idx = i,
        display_group = display_group
      )
    } else {
      # Static/custom enum or other type: use column name as is
      curator_cols[[length(curator_cols) + 1]] <- list(
        curator_name = col_name,
        base_name = col_name,
        pick_num = NA,
        dict_idx = i,
        display_group = display_group
      )
    }
  }

  base_to_curator_indices <- split(
    seq_along(curator_cols),
    vapply(curator_cols, function(col_info) col_info$base_name, character(1))
  )

  # Create Curator_Entry sheet
  message("Creating Curator_Entry sheet...")
  wb$add_worksheet("Curator_Entry")

  # Build row 1: Category headers (show on every column)
  # Build row 2: Column headers (_pick columns for multi-value fields)
  row1 <- character(length(curator_cols))
  row2 <- character(length(curator_cols))

  for (i in seq_along(curator_cols)) {
    col_info <- curator_cols[[i]]
    current_group <- if (!is.null(col_info$display_group) && !is.na(col_info$display_group)) {
      col_info$display_group
    } else {
      ""
    }

    # Show group header on every column
    row1[i] <- current_group

    row2[i] <- col_info$curator_name
  }

  # Write row 1 and row 2
  wb$add_data("Curator_Entry", t(as.matrix(row1)), start_row = 1, col_names = FALSE)
  wb$add_data("Curator_Entry", t(as.matrix(row2)), start_row = 2, col_names = FALSE)

  # Add entry rows (rows 3-10). Optionally prefill first 10 columns for rows 3-6.
  for (row_num in 3:10) {
    row_values <- rep("", length(curator_cols))

    if (isTRUE(example_data) && row_num <= 6) {
      n_prefill <- min(10, length(curator_cols))
      prefill_values <- rep("", n_prefill)

      # Demonstration values for core identifier/publication fields.
      prefill_values[1] <- paste0("DemoStudy_", 2022 + (row_num - 3))
      if (n_prefill >= 2) prefill_values[2] <- paste0("SUBJ", sprintf("%03d", row_num - 2))
      if (n_prefill >= 3) prefill_values[3] <- paste0("SAMP", sprintf("%03d", row_num - 2))
      if (n_prefill >= 9) prefill_values[9] <- as.character(10000000 + (row_num - 3))
      if (n_prefill >= 10) prefill_values[10] <- "ExampleCohort"

      # If target_condition dropdown values are available, prefill first pick column.
      if (n_prefill >= 4 && "target_condition" %in% names(validation_lists)) {
        tc_values <- validation_lists[["target_condition"]]
        if (length(tc_values) > 0) {
          prefill_values[4] <- tc_values[((row_num - 3) %% length(tc_values)) + 1]
        }
      }

      row_values[seq_len(n_prefill)] <- prefill_values

      # Fill curator picks with demonstration curator aliases when present.
      curator_examples <- c(
        "Anonymous Alligator",
        "Bold Badger",
        "Curious Coyote",
        "Diligent Dolphin",
        "Eager Eagle"
      )
      for (pick_idx in seq_along(curator_examples)) {
        pick_name <- paste0("curator_pick", pick_idx)
        col_idx <- which(vapply(curator_cols, function(x) x$curator_name, character(1)) == pick_name)
        if (length(col_idx) > 0) {
          row_values[col_idx[1]] <- curator_examples[pick_idx]
        }
      }

      # Fill first two target_condition picks with example ontology values when available.
      if ("target_condition" %in% names(validation_lists)) {
        tc_values <- validation_lists[["target_condition"]]
        if (length(tc_values) > 0) {
          tc_start <- ((row_num - 3) * 2 %% length(tc_values)) + 1
          tc_pick1 <- tc_values[tc_start]
          tc_pick2 <- tc_values[(tc_start %% length(tc_values)) + 1]

          tc_pick1_idx <- which(vapply(curator_cols, function(x) x$curator_name, character(1)) == "target_condition_pick1")
          if (length(tc_pick1_idx) > 0) {
            row_values[tc_pick1_idx[1]] <- tc_pick1
          }

          tc_pick2_idx <- which(vapply(curator_cols, function(x) x$curator_name, character(1)) == "target_condition_pick2")
          if (length(tc_pick2_idx) > 0) {
            row_values[tc_pick2_idx[1]] <- tc_pick2
          }
        }
      }
    }

    wb$add_data("Curator_Entry", t(as.matrix(row_values)), start_row = row_num, col_names = FALSE)
  }

  # Freeze the first two rows
  wb$freeze_pane("Curator_Entry", first_active_row = 3, first_active_col = 1)

  # Add styling to header rows
  message("Adding header styling...")

  # Style row 1 (group headers) with bold text
  for (i in seq_along(curator_cols)) {
    wb$add_font(
      sheet = "Curator_Entry",
      dims = paste0(openxlsx2::int2col(i), "1"),
      bold = TRUE,
      size = 11
    )
  }

  # Style row 2 (column headers) with bold text
  for (i in seq_along(curator_cols)) {
    wb$add_font(
      sheet = "Curator_Entry",
      dims = paste0(openxlsx2::int2col(i), "2"),
      bold = TRUE,
      size = 10
    )
  }

  # Create Lists sheet for validation data
  message("Creating Lists sheet...")
  wb$add_worksheet("Lists", visible = FALSE)

  # Write validation lists to the Lists sheet
  current_col <- 1
  list_ranges <- list()

  for (col_name in names(validation_lists)) {
    values <- validation_lists[[col_name]]

    # Write values to Lists sheet
    wb$add_data("Lists",
                data.frame(values, stringsAsFactors = FALSE),
                start_col = current_col,
                start_row = 1,
                col_names = FALSE)

    # Store the range for data validation
    end_row <- length(values)
    list_ranges[[col_name]] <- list(
      col = current_col,
      range = paste0("Lists!$", openxlsx2::int2col(current_col), "$1:$",
                    openxlsx2::int2col(current_col), "$", end_row)
    )

    current_col <- current_col + 1
  }

  # Add data validation to Curator_Entry sheet
  message("Adding data validation rules...")

  # Add dropdown validations for enum fields
  for (col_name in names(list_ranges)) {
    # Find all curator columns that correspond to this base column (including _pick variants)
    for (i in seq_along(curator_cols)) {
      col_info <- curator_cols[[i]]
      if (col_info$base_name == col_name) {
        # Apply validation from row 3 to row 10000 (row 1 is categories, row 2 is headers)
        wb$add_data_validation(
          sheet = "Curator_Entry",
          cols = i,
          rows = 3:10000,
          type = "list",
          value = list_ranges[[col_name]]$range
        )
      }
    }
  }

  # Add numeric and text validations for non-enum fields
  message("Adding numeric and text validation rules...")
  for (i in seq_along(curator_cols)) {
    col_info <- curator_cols[[i]]
    dict_idx <- col_info$dict_idx
    col_name <- col_info$base_name
    corpus_type <- dict$corpus.type[dict_idx]
    allowed_values <- dict$allowedvalues[dict_idx]
    cell_ref <- paste0(openxlsx2::int2col(i), "3")
    is_required <- !is.na(dict$required[dict_idx]) && dict$required[dict_idx] == "required"
    has_list_validation <- col_name %in% names(list_ranges)

    # Escape any quotes that could appear in regex patterns for Excel formulas.
    escaped_allowed <- gsub('"', '""', as.character(allowed_values), fixed = TRUE)

    # For required columns that already have list validation, add a non-empty check.
    if (has_list_validation) {
      if (is_required) {
        wb$add_data_validation(
          sheet = "Curator_Entry",
          cols = i,
          rows = 3:10000,
          type = "custom",
          value = paste0("LEN(", cell_ref, ")>0")
        )
      }
      next
    }

    # Add validation based on corpus type
    if (!is.na(corpus_type)) {
      if (corpus_type == "integer") {
        # Integer constraints from dictionary pattern when available.
        if (!is.na(allowed_values) && allowed_values == "[0-9]{8}") {
          wb$add_data_validation(
            sheet = "Curator_Entry",
            cols = i,
            rows = 3:10000,
            type = "whole",
            operator = "between",
            value = c(10000000, 99999999)
          )
        } else {
          # Generic integer validation (non-negative)
          wb$add_data_validation(
            sheet = "Curator_Entry",
            cols = i,
            rows = 3:10000,
            type = "whole",
            operator = "greaterThanOrEqual",
            value = 0
          )
        }

        if (is_required) {
          wb$add_data_validation(
            sheet = "Curator_Entry",
            cols = i,
            rows = 3:10000,
            type = "custom",
            value = paste0("LEN(", cell_ref, ")>0")
          )
        }
      } else if (corpus_type %in% c("numeric", "double")) {
        # Numeric constraints from dictionary: strictly positive where required by pattern.
        numeric_operator <- "greaterThanOrEqual"
        numeric_value <- 0

        if (!is.na(allowed_values) && grepl("^\\^?\\[1-9\\]", allowed_values)) {
          numeric_operator <- "greaterThan"
          numeric_value <- 0
        }

        wb$add_data_validation(
          sheet = "Curator_Entry",
          cols = i,
          rows = 3:10000,
          type = "decimal",
          operator = numeric_operator,
          value = numeric_value
        )

        if (is_required) {
          wb$add_data_validation(
            sheet = "Curator_Entry",
            cols = i,
            rows = 3:10000,
            type = "custom",
            value = paste0("LEN(", cell_ref, ")>0")
          )
        }
      } else if (corpus_type == "regexp" && !is.na(allowed_values) && allowed_values != "" && allowed_values != "NA") {
        # For regex patterns, add a cell comment to guide users
        # and enforce the pattern with REGEXTEST in custom validation.
        description <- dict$description[dict_idx]
        required_text <- if (dict$required[dict_idx] == "required") "REQUIRED" else "Optional"

        # Create comment text with pattern and description
        comment_text <- paste0(
          col_name, " (", required_text, ")\n\n",
          "Expected format:\n",
          allowed_values, "\n\n"
        )

        if (!is.na(description) && description != "") {
          comment_text <- paste0(comment_text, "Description: ", description)
        }

        # Truncate if too long (Excel comments have limits)
        if (nchar(comment_text) > 500) {
          comment_text <- substr(comment_text, 1, 497)
          comment_text <- paste0(comment_text, "...")
        }

        # Add comment to header cell (row 2)
        tryCatch({
          wb$add_comment(
            sheet = "Curator_Entry",
            col = i,
            row = 2,
            comment = comment_text,
            author = "Data Dictionary"
          )
        }, error = function(e) {
          # Silently skip if comment fails
        })
        regex_formula <- if (is_required) {
          paste0(
            "AND(LEN(", cell_ref, ")>0,REGEXTEST(", cell_ref, ",\"", escaped_allowed, "\"))"
          )
        } else {
          paste0(
            "OR(LEN(", cell_ref, ")=0,REGEXTEST(", cell_ref, ",\"", escaped_allowed, "\"))"
          )
        }

        wb$add_data_validation(
          sheet = "Curator_Entry",
          cols = i,
          rows = 3:10000,
          type = "custom",
          value = regex_formula
        )
      } else {
        # Fallback for free-text types and any columns without explicit format regex.
        if (is_required) {
          wb$add_data_validation(
            sheet = "Curator_Entry",
            cols = i,
            rows = 3:10000,
            type = "custom",
            value = paste0("LEN(", cell_ref, ")>0")
          )
        }
      }
    }
  }

  export_formula <- function(base_name, export_row) {
    curator_indices <- base_to_curator_indices[[base_name]]

    if (is.null(curator_indices) || length(curator_indices) == 0) {
      return("")
    }

    curator_row <- export_row + 1
    refs <- paste0("Curator_Entry!", openxlsx2::int2col(curator_indices), curator_row)

    if (length(refs) == 1) {
      return(sprintf('IF(%s="","",%s)', refs, refs))
    }

    # Avoid TEXTJOIN to prevent Excel from rewriting as implicit-intersection '@TEXTJOIN'.
    pieces <- c()
    for (idx in seq_along(refs)) {
      ref <- refs[idx]
      if (idx == 1) {
        pieces <- c(pieces, sprintf('IF(%s="","",%s)', ref, ref))
      } else {
        prev_refs <- paste(refs[1:(idx - 1)], collapse = ",")
        pieces <- c(
          pieces,
          sprintf(
            'IF(%s="","",IF(COUNTA(%s)>0,";","")&%s)',
            ref,
            prev_refs,
            ref
          )
        )
      }
    }

    paste(pieces, collapse = "&")
  }

  # Create Export sheet (base columns populated from Curator_Entry formulas)
  message("Creating Export sheet...")
  wb$add_worksheet("Export")

  # Write base column names as header row
  export_headers <- dict$col.name
  wb$add_data("Export", t(as.matrix(export_headers)), start_row = 1, col_names = FALSE)

  export_rows <- 2:max_export_row
  export_formula_matrix <- do.call(
    rbind,
    lapply(export_rows, function(row_num) {
      vapply(export_headers, export_formula, character(1), export_row = row_num)
    })
  )

  for (col_idx in seq_along(export_headers)) {
    wb$add_formula(
      "Export",
      x = export_formula_matrix[, col_idx],
      start_col = col_idx,
      start_row = 2
    )
  }

  # Freeze the header row
  wb$freeze_pane("Export", first_row = TRUE)

  # Create README sheet
  message("Creating README sheet...")
  wb$add_worksheet("README")
  readme_text <- data.frame(
    Content = c(
      "curatedMetagenomicData Data Entry Template",
      "",
      "This Excel file was dynamically generated based on:",
      "1. Data dictionary: inst/extdata/cMD_data_dictionary.csv",
      "2. Ontology Lookup Service (OLS) for dynamic enum values",
      "3. Existing curated metadata for large dynamic enums",
      "",
      "Instructions:",
      "- Use the 'Curator_Entry' sheet to enter your data",
      "- Column headers in row 1 show the field name and description",
      "- Columns with dropdowns have predefined allowed values",
      "- Required fields must be filled in",
      "- Some fields allow multiple values (check the MultipleValues column)",
      "",
      "For more information, see:",
      "https://github.com/waldronlab/curatedMetagenomicDataCuration"
    ),
    stringsAsFactors = FALSE
  )
  wb$add_data("README", readme_text, col_names = FALSE)
  wb$set_col_widths("README", cols = 1, widths = 100)

  # Save the workbook
  message(paste("Saving workbook to", output_file, "..."))
  wb$save(output_file, overwrite = TRUE)

  message("Done! Excel file created successfully.")

  # Return summary
  list(
    output_file = output_file,
    num_columns = nrow(dict),
    num_validation_lists = length(validation_lists),
    validation_columns = names(validation_lists)
  )
}
