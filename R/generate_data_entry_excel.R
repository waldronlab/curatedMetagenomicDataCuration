#' Query Ontology Lookup Service (OLS) for ontology terms
#'
#' @param ontology_ids Character vector of ontology IDs (e.g., "NCIT:C7057")
#' @param relationship Type of relationship: "children" or "descendant"
#' @param size_threshold Threshold for "large" enums (default: 1000)
#' @return Character vector of ontology terms (label|ID format) or NULL if too large
#' @noRd
#' @importFrom httr2 request req_perform resp_body_json req_url_query req_error
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
#' @noRd
#' @importFrom readr read_csv
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
#' @noRd
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
#' @export
generate_data_entry_excel <- function(
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

  build_dict_github_permalink <- function(path) {
    dict_path <- normalizePath(path, winslash = "/", mustWork = FALSE)
    dict_dir <- dirname(dict_path)

    repo_root <- tryCatch(
      trimws(system2("git", c("-C", dict_dir, "rev-parse", "--show-toplevel"), stdout = TRUE, stderr = FALSE)),
      error = function(e) character(0)
    )

    if (length(repo_root) == 0 || repo_root == "") {
      return(NA_character_)
    }

    repo_root <- normalizePath(repo_root, winslash = "/", mustWork = FALSE)
    if (!startsWith(dict_path, paste0(repo_root, "/"))) {
      return(NA_character_)
    }

    commit_sha <- tryCatch(
      trimws(system2("git", c("-C", repo_root, "rev-parse", "HEAD"), stdout = TRUE, stderr = FALSE)),
      error = function(e) character(0)
    )
    if (length(commit_sha) == 0 || commit_sha == "") {
      return(NA_character_)
    }

    remote_url <- tryCatch(
      trimws(system2("git", c("-C", repo_root, "config", "--get", "remote.origin.url"), stdout = TRUE, stderr = FALSE)),
      error = function(e) character(0)
    )
    if (length(remote_url) == 0 || remote_url == "") {
      return(NA_character_)
    }

    if (grepl("^git@github.com:", remote_url)) {
      remote_url <- sub("^git@github.com:", "https://github.com/", remote_url)
    }
    remote_url <- sub("\\.git$", "", remote_url)

    if (!grepl("^https://github.com/", remote_url)) {
      return(NA_character_)
    }

    rel_path <- sub(paste0("^", repo_root, "/"), "", dict_path)
    paste0(remote_url, "/blob/", commit_sha, "/", rel_path)
  }

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
  max_curator_row <- 3002
  max_export_row <- 3001
  dictionary_rows <- c(
    "col.class",
    "unique",
    "required",
    "description",
    "regexp.rule",
    "regexp.examples",
    "corpus.type"
  )

  metadata_examples <- list()
  metadata_df <- tryCatch(
    read.csv(metadata_file, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) NULL
  )

  if (!is.null(metadata_df) && nrow(metadata_df) > 0) {
    regexp_cols <- dict$col.name[!is.na(dict$corpus.type) & dict$corpus.type == "regexp"]
    for (regexp_col in regexp_cols) {
      if (!regexp_col %in% names(metadata_df)) {
        next
      }

      raw_values <- as.character(metadata_df[[regexp_col]])
      raw_values <- raw_values[!is.na(raw_values)]
      raw_values <- trimws(raw_values)
      raw_values <- raw_values[raw_values != ""]

      split_values <- trimws(unlist(strsplit(raw_values, ";", fixed = TRUE), use.names = FALSE))
      split_values <- split_values[split_values != ""]
      unique_values <- unique(split_values)

      if (length(unique_values) > 0) {
        metadata_examples[[regexp_col]] <- paste0("e.g.: ", paste(head(unique_values, 3), collapse = ", "))
      }
    }
  }

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

  group_header_row <- 1
  metadata_start_row <- group_header_row + 1
  column_header_row <- metadata_start_row + length(dictionary_rows)
  first_data_row <- column_header_row + 1
  validation_rows <- first_data_row:max_curator_row
  export_to_curator_offset <- first_data_row - 2

  # Build row 1: Category headers (show on every column)
  # Build metadata rows from dictionary and column header row.
  row1 <- character(length(curator_cols))
  column_header_values <- character(length(curator_cols))

  for (i in seq_along(curator_cols)) {
    col_info <- curator_cols[[i]]
    current_group <- if (!is.null(col_info$display_group) && !is.na(col_info$display_group)) {
      col_info$display_group
    } else {
      ""
    }

    # Show group header on every column
    row1[i] <- current_group

    column_header_values[i] <- col_info$curator_name
  }

  # Build metadata rows (same order as dictionary_rows) before column names.
  metadata_matrix <- vapply(
    dictionary_rows,
    function(field_name) {
      vapply(
        curator_cols,
        function(col_info) {
          dict_idx <- col_info$dict_idx
          base_name <- col_info$base_name
          corpus_type <- dict$corpus.type[dict_idx]

          if (field_name == "regexp.rule") {
            if (!is.na(corpus_type) && corpus_type == "regexp") {
              value <- dict$allowedvalues[dict_idx]
              if (is.null(value) || is.na(value)) "" else as.character(value)
            } else {
              ""
            }
          } else if (field_name == "regexp.examples") {
            if (!is.na(corpus_type) && corpus_type == "regexp") {
              value <- metadata_examples[[base_name]]
              if (is.null(value) || is.na(value)) "" else as.character(value)
            } else {
              ""
            }
          } else {
            value <- dict[[field_name]][dict_idx]
            if (is.null(value) || is.na(value)) {
              ""
            } else {
              as.character(value)
            }
          }
        },
        character(1)
      )
    },
    FUN.VALUE = character(length(curator_cols))
  )

  # Write group header row, dictionary rows, and column header row.
  wb$add_data("Curator_Entry", t(as.matrix(row1)), start_row = group_header_row, col_names = FALSE)
  for (metadata_idx in seq_along(dictionary_rows)) {
    wb$add_data(
      "Curator_Entry",
      t(as.matrix(metadata_matrix[, metadata_idx])),
      start_row = metadata_start_row + metadata_idx - 1,
      col_names = FALSE
    )
  }
  wb$add_data("Curator_Entry", t(as.matrix(column_header_values)), start_row = column_header_row, col_names = FALSE)

  # Add entry rows. Optionally prefill first 10 columns for first 4 example rows.
  for (row_num in first_data_row:(first_data_row + 7)) {
    row_values <- rep("", length(curator_cols))
    example_offset <- row_num - first_data_row

    if (isTRUE(example_data) && example_offset <= 3) {
      n_prefill <- min(10, length(curator_cols))
      prefill_values <- rep("", n_prefill)

      # Demonstration values for core identifier/publication fields.
      prefill_values[1] <- "EinsteinA_1922"
      if (n_prefill >= 2) prefill_values[2] <- paste0("SUBJ", sprintf("%03d", example_offset + 1))
      if (n_prefill >= 3) prefill_values[3] <- paste0("SAMP", sprintf("%03d", example_offset + 1))
      if (n_prefill >= 9) prefill_values[9] <- "12345678"
      if (n_prefill >= 10) prefill_values[10] <- "ExampleCohort"

      # If target_condition dropdown values are available, prefill first pick column.
      if (n_prefill >= 4 && "target_condition" %in% names(validation_lists)) {
        tc_values <- validation_lists[["target_condition"]]
        if (length(tc_values) > 0) {
          prefill_values[4] <- tc_values[(example_offset %% length(tc_values)) + 1]
        }
      }

      row_values[seq_len(n_prefill)] <- prefill_values

      # Ensure demo row constants for all study_name / pmid columns.
      study_name_indices <- which(vapply(curator_cols, function(x) x$base_name, character(1)) == "study_name")
      if (length(study_name_indices) > 0) {
        row_values[study_name_indices] <- "EinsteinA_1922"
      }

      pmid_indices <- which(vapply(curator_cols, function(x) x$base_name, character(1)) == "pmid")
      if (length(pmid_indices) > 0) {
        row_values[pmid_indices] <- "12345678"
      }

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
          tc_start <- ((example_offset) * 2 %% length(tc_values)) + 1
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

  # Add styling to header rows
  message("Adding header styling...")

  # Style group header row with bold text
  for (i in seq_along(curator_cols)) {
    wb$add_font(
      sheet = "Curator_Entry",
      dims = paste0(openxlsx2::int2col(i), group_header_row),
      bold = TRUE,
      size = 11
    )
  }

  # Style column header row with bold text
  for (i in seq_along(curator_cols)) {
    wb$add_font(
      sheet = "Curator_Entry",
      dims = paste0(openxlsx2::int2col(i), column_header_row),
      bold = TRUE,
      size = 10
    )
  }

  wb$freeze_pane("Curator_Entry", first_active_row = first_data_row, first_active_col = 1)

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
    col_letter <- openxlsx2::int2col(current_col)
    list_dims <- paste0("$", col_letter, "$1:$", col_letter, "$", end_row)
    sheet_range <- paste0("Lists!", list_dims)

    list_ranges[[col_name]] <- list(
      col = current_col,
      range = sheet_range
    )

    # Create a named range for use in conditional formatting
    # Use LSTP_ prefix for dynamic enums, LST_ for others
    dict_idx <- which(dict$col.name == col_name)[1]
    corpus_type <- if (!is.na(dict_idx)) dict$corpus.type[dict_idx] else ""

    prefix <- if (!is.na(corpus_type) && grepl("dynamic_enum", corpus_type)) "LSTP_" else "LST_"
    range_name <- paste0(prefix, toupper(col_name))

    tryCatch({
      wb$add_named_region(
        name = range_name,
        sheet = "Lists",
        dims = list_dims
      )
      list_ranges[[col_name]]$named_range <- range_name
    }, error = function(e) {
      warning(paste("Could not create named range for", col_name, ":", conditionMessage(e)))
    })

    current_col <- current_col + 1
  }

  # Add data validation to Curator_Entry sheet
  message("Adding data validation rules...")

  add_validation_warning <- function(...) {
    wb$add_data_validation(..., error_style = "warning")
  }

  list_validation_ranges <- list()
  required_validation_cols <- integer(0)
  unique_validation_cols <- integer(0)
  format_validation_rules <- list()

  # Add dropdown validations for enum fields
  for (col_name in names(list_ranges)) {
    # Find all curator columns that correspond to this base column (including _pick variants)
    for (i in seq_along(curator_cols)) {
      col_info <- curator_cols[[i]]
      if (col_info$base_name == col_name) {
        # Apply validation across data-entry rows.
        add_validation_warning(
          sheet = "Curator_Entry",
          cols = i,
          rows = validation_rows,
          type = "list",
          value = list_ranges[[col_name]]$range
        )
        # Store named range for conditional formatting (if available)
        if (!is.null(list_ranges[[col_name]]$named_range)) {
          list_validation_ranges[[as.character(i)]] <- list_ranges[[col_name]]$named_range
        }
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
    cell_ref <- paste0(openxlsx2::int2col(i), first_data_row)
    is_required <- !is.na(dict$required[dict_idx]) && dict$required[dict_idx] == "required"
    unique_value <- ""
    if ("unique" %in% colnames(dict)) {
      unique_value <- as.character(dict$unique[dict_idx])
    }
    is_unique <- !is.na(unique_value) && tolower(trimws(unique_value)) == "unique"
    multiplevalues_base <- FALSE
    if ("multiplevalues" %in% colnames(dict)) {
      mv <- dict$multiplevalues[dict_idx]
      if (is.logical(mv)) {
        multiplevalues_base <- isTRUE(mv)
      } else {
        multiplevalues_base <- tolower(as.character(mv)) == "true"
      }
    }

    if (multiplevalues_base && !is.na(col_info$pick_num)) {
      is_required <- is_required && col_info$pick_num == 1
      is_unique <- is_unique && col_info$pick_num == 1
    }

    if (is_unique) {
      unique_validation_cols <- c(unique_validation_cols, i)
    }

    has_list_validation <- col_name %in% names(list_ranges)
    invalid_format_rule <- NULL

    # Escape any quotes that could appear in regex patterns for Excel formulas.
    escaped_allowed <- gsub('"', '""', as.character(allowed_values), fixed = TRUE)

    # For required columns that already have list validation, add a non-empty check.
    if (has_list_validation) {
      if (is_required) {
        required_validation_cols <- c(required_validation_cols, i)
        add_validation_warning(
          sheet = "Curator_Entry",
          cols = i,
          rows = validation_rows,
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
          add_validation_warning(
            sheet = "Curator_Entry",
            cols = i,
            rows = validation_rows,
            type = "whole",
            operator = "between",
            value = c(10000000, 99999999)
          )
          invalid_format_rule <- paste0(
            "AND(", cell_ref, "<>\"\",NOT(AND(LEN(", cell_ref, ")=8,ISNUMBER(--", cell_ref, "))))"
          )
        } else {
          # Generic integer validation (non-negative)
          add_validation_warning(
            sheet = "Curator_Entry",
            cols = i,
            rows = validation_rows,
            type = "whole",
            operator = "greaterThanOrEqual",
            value = 0
          )
          invalid_format_rule <- paste0(
            "AND(", cell_ref, "<>\"\",OR(NOT(ISNUMBER(", cell_ref, ")),", cell_ref, "<0,INT(", cell_ref, ")<>", cell_ref, "))"
          )
        }

        if (is_required) {
          required_validation_cols <- c(required_validation_cols, i)
          add_validation_warning(
            sheet = "Curator_Entry",
            cols = i,
            rows = validation_rows,
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

        add_validation_warning(
          sheet = "Curator_Entry",
          cols = i,
          rows = validation_rows,
          type = "decimal",
          operator = numeric_operator,
          value = numeric_value
        )

        if (numeric_operator == "greaterThan") {
          invalid_format_rule <- paste0(
            "AND(", cell_ref, "<>\"\",OR(NOT(ISNUMBER(", cell_ref, ")),", cell_ref, "<=0))"
          )
        } else {
          invalid_format_rule <- paste0(
            "AND(", cell_ref, "<>\"\",OR(NOT(ISNUMBER(", cell_ref, ")),", cell_ref, "<0))"
          )
        }

        if (is_required) {
          required_validation_cols <- c(required_validation_cols, i)
          add_validation_warning(
            sheet = "Curator_Entry",
            cols = i,
            rows = validation_rows,
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

        # Add comment to column header cell.
        tryCatch({
          wb$add_comment(
            sheet = "Curator_Entry",
            col = i,
            row = column_header_row,
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

        add_validation_warning(
          sheet = "Curator_Entry",
          cols = i,
          rows = validation_rows,
          type = "custom",
          value = regex_formula
        )

        invalid_format_rule <- paste0(
          "AND(", cell_ref, "<>\"\",NOT(REGEXTEST(", cell_ref, ",\"", escaped_allowed, "\")))"
        )

        if (is_required) {
          required_validation_cols <- c(required_validation_cols, i)
        }
      } else {
        # Fallback for free-text types and any columns without explicit format regex.
        if (is_required) {
          required_validation_cols <- c(required_validation_cols, i)
          add_validation_warning(
            sheet = "Curator_Entry",
            cols = i,
            rows = validation_rows,
            type = "custom",
            value = paste0("LEN(", cell_ref, ")>0")
          )
        }
      }

      if (!is.null(invalid_format_rule)) {
        format_validation_rules[[as.character(i)]] <- invalid_format_rule
      }
    }
  }

  # Add conditional formatting for visible quality checks in active rows.
  message("Adding conditional formatting highlights...")
  required_style <- openxlsx2::create_dxfs_style(bg_fill = openxlsx2::wb_color("FFEB9C"))
  mismatch_style <- openxlsx2::create_dxfs_style(bg_fill = openxlsx2::wb_color("FFC7CE"))
  duplicate_style <- openxlsx2::create_dxfs_style(bg_fill = openxlsx2::wb_color("D9D2E9"))

  required_style_name <- "CF_REQUIRED_EMPTY"
  mismatch_style_name <- "CF_ENUM_MISMATCH"
  duplicate_style_name <- "CF_DUPLICATE_VALUE"
  wb$styles_mgr$add(required_style, required_style_name)
  wb$styles_mgr$add(mismatch_style, mismatch_style_name)
  wb$styles_mgr$add(duplicate_style, duplicate_style_name)

  required_validation_cols <- unique(required_validation_cols)
  for (i in required_validation_cols) {
    col_letter <- openxlsx2::int2col(i)
    dims <- paste0(col_letter, first_data_row, ":", col_letter, max_curator_row)
    # Use relative row reference so formula adjusts for each row
    required_rule <- paste0("LEN(TRIM(", col_letter, first_data_row, "))=0")

    tryCatch({
      wb$add_conditional_formatting(
        sheet = "Curator_Entry",
        dims = dims,
        type = "expression",
        rule = required_rule,
        style = required_style_name
      )
    }, error = function(e) {
      warning(paste("Could not add conditional formatting for column", i, ":", conditionMessage(e)))
    })
  }

  for (i_chr in names(list_validation_ranges)) {
    i <- as.integer(i_chr)
    col_letter <- openxlsx2::int2col(i)
    dims <- paste0(col_letter, first_data_row, ":", col_letter, max_curator_row)
    named_range <- list_validation_ranges[[i_chr]]

    # Use named range with ISNA(MATCH(TRIM())) pattern for better compatibility
    mismatch_rule <- paste0(
      "AND(", col_letter, first_data_row, "<>\"\",",
      "ISNA(MATCH(TRIM(", col_letter, first_data_row, "),", named_range, ",0)))"
    )

    tryCatch({
      wb$add_conditional_formatting(
        sheet = "Curator_Entry",
        dims = dims,
        type = "expression",
        rule = mismatch_rule,
        style = mismatch_style_name
      )
    }, error = function(e) {
      warning(paste("Could not add conditional formatting for column", i, ":", conditionMessage(e)))
    })
  }

  for (i_chr in names(format_validation_rules)) {
    i <- as.integer(i_chr)
    col_letter <- openxlsx2::int2col(i)
    dims <- paste0(col_letter, first_data_row, ":", col_letter, max_curator_row)
    format_rule <- format_validation_rules[[i_chr]]

    tryCatch({
      wb$add_conditional_formatting(
        sheet = "Curator_Entry",
        dims = dims,
        type = "expression",
        rule = format_rule,
        style = mismatch_style_name
      )
    }, error = function(e) {
      warning(paste("Could not add format conditional formatting for column", i, ":", conditionMessage(e)))
    })
  }

  unique_validation_cols <- unique(unique_validation_cols)
  duplicate_window_end <- min(first_data_row + 499, max_curator_row)
  for (i in unique_validation_cols) {
    col_letter <- openxlsx2::int2col(i)
    dims <- paste0(col_letter, first_data_row, ":", col_letter, max_curator_row)
    duplicate_rule <- paste0(
      "AND(", col_letter, first_data_row, "<>\"\",",
      "COUNTIF($", col_letter, first_data_row, ":$", col_letter, duplicate_window_end, ",",
      col_letter, first_data_row, ")>1)"
    )

    tryCatch({
      wb$add_conditional_formatting(
        sheet = "Curator_Entry",
        dims = dims,
        type = "expression",
        rule = duplicate_rule,
        style = duplicate_style_name
      )
    }, error = function(e) {
      warning(paste("Could not add duplicate conditional formatting for column", i, ":", conditionMessage(e)))
    })
  }

  export_formula <- function(base_name, export_row) {
    curator_indices <- base_to_curator_indices[[base_name]]

    if (is.null(curator_indices) || length(curator_indices) == 0) {
      return("")
    }

    curator_row <- export_row + export_to_curator_offset
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

  generated_timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
  dict_permalink <- build_dict_github_permalink(dict_file)
  if (is.na(dict_permalink)) {
    dict_permalink <- paste0("Unavailable (non-git or non-GitHub path): ", normalizePath(dict_file, winslash = "/", mustWork = FALSE))
  }

  readme_text <- data.frame(
    Content = c(
      "curatedMetagenomicData Data Entry Template",
      "",
      paste0("Generated at: ", generated_timestamp),
      paste0("Data dictionary permalink: ", dict_permalink),
      "",
      "This Excel file was dynamically generated based on:",
      "1. Data dictionary: inst/extdata/cMD_data_dictionary.csv",
      "2. Ontology Lookup Service (OLS) for dynamic enum values",
      "3. Existing curated metadata for large dynamic enums",
      "",
      "Instructions:",
      "- Use the 'Curator_Entry' sheet to enter your data",
      "- Row 1 contains group headers; rows 2-8 contain data-dictionary metadata; row 9 contains column names",
      "- Columns with dropdowns have predefined allowed values",
      "- Required fields must be filled in",
      "- Some fields allow multiple values; see the field description or data dictionary for details",
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
