# Function for validating curated metadata

import_owl_id_label <- function(path) {
  results <- tryCatch(
    {
      # Read in OWL file
      doc <- xml2::read_xml(path, options = c("HUGE", "RECOVER"))
      
      # Parse classes
      classes <- xml2::xml_find_all(
        doc,
        ".//owl:Class",
        xml2::xml_ns(doc)
      )
      
      id <- xml2::xml_attr(classes, "about")
      label <- xml2::xml_text(
        xml2::xml_find_first(classes, "rdfs:label", xml2::xml_ns(doc))
      )
      
      # Present results
      results <- tibble::tibble(
        id = id,
        label = label,
        id_short = base::basename(id) |>
          stringr::str_replace("_", ":")
      )
      
      # Give back memory
      rm(doc, classes)
      gc(FALSE)
      
      results
    },
    error = function(e) {
      stop(
        sprintf(
          "Error while importing OWL file '%s': %s",
          path,
          conditionMessage(e)
        ),
        call. = FALSE
      )
    }
  )
  
  return(results)
}

check_required <- function(dict, data, include_all = FALSE) {
  # Which columns are required
  r_colnames <- dict$ColName[dict$Required == "required"]
  
  # Which required columns are missing from the data
  missing <- r_colnames[!r_colnames %in% colnames(data)]
  
  # Compile results
  if (include_all) {
    results <- data.frame(
      column = r_colnames,
      row = NA_integer_,
      value = NA_character_,
      check_type = "required",
      expected = "required_column",
      valid = !r_colnames %in% missing
    )
  } else {
    if (length(missing) == 0) {
      results <- data.frame(
        column = character(0),
        row = integer(0),
        value = character(0),
        check_type = character(0),
        expected = character(0),
        valid = logical(0)
      )
    } else {
      results <- data.frame(
        column = missing,
        row = NA_integer_,
        value = NA_character_,
        check_type = "required",
        expected = "required_column",
        valid = FALSE
      )
    }
  }
  
  return(results)
}

is_integer_str <- function(x) grepl("^-?[0-9]+$", x)
is_number_str <- function(x) !is.na(suppressWarnings(as.numeric(x)))
# Note: kept for potential future boolean string validation rules (e.g., in check_class).
is_bool_str <- function(x) tolower(x) %in% c("true", "false", "t", "f", "yes", "no", "1", "0")

allow_na <- function(fn) {
  function(x) {
    is_na <- is.na(x)
    out <- fn(x)
    out[is_na] <- TRUE
    out
  }
}

check_class <- function(dict, data, include_all = FALSE) {
  # Find intersecting columns
  cols_to_check <- intersect(dict$ColName, colnames(data))
  
  # Assign string parsers to each class listed
  class_functions <- list(
    character = function(x) rep(TRUE, length(x)),
    integer = allow_na(is_integer_str),
    double = allow_na(is_number_str),
    numeric = allow_na(is_number_str)
  )
  
  # Run appropriate string parsers and combine results
  if (length(cols_to_check) == 0) {
    col_res <- data.frame(
      column = character(0),
      row = integer(0),
      value = character(0),
      check_type = character(0),
      expected = character(0),
      valid = logical(0)
    )

    return(col_res)
  }
  
  results <- do.call(rbind, lapply(cols_to_check, function(col) {
    type <- dict$ColClass[dict$ColName == col]
    fn <- class_functions[[type]]

    if (is.null(fn)) {
      stop(
        sprintf(
          "Unrecognized column class '%s' for column '%s' in dict$ColClass. ",
          type,
          col
        ),
        call. = FALSE
      )
    }
    ok <- fn(data[[col]])
    
    if (include_all) {
      col_res <- data.frame(
        column = col,
        row = seq_along(ok),
        value = data[[col]],
        check_type = "class",
        expected = type,
        valid = ok
      )
    } else {
      bad <- which(!ok)
      if (length(bad) == 0) {
        col_res <- data.frame(
          column = character(0),
          row = integer(0),
          value = character(0),
          check_type = character(0),
          expected = character(0),
          valid = logical(0)
        )
      } else {
        col_res <- data.frame(
          column = col,
          row = bad,
          value = data[[col]][bad],
          check_type = "class",
          expected = type,
          valid = FALSE
        )
      }
    }
    
    return(col_res)
  }))
  
  return(results)
}

check_unique <- function(dict, data, include_all = FALSE) {
  # Which columns require uniqueness
  u_colnames <- dict$ColName[dict$Unique == "unique"]
  
  # Find intersecting columns
  cols_to_check <- intersect(u_colnames, colnames(data))
  
  # Which unique columns have duplicated values
  if (length(cols_to_check) == 0) {
    col_res <- data.frame(
      column = character(0),
      row = integer(0),
      value = character(0),
      check_type = character(0),
      expected = character(0),
      valid = logical(0)
    )
    
    return(col_res)
  }
  
  results <- do.call(rbind, lapply(cols_to_check, function(col) {
    dup <- duplicated(data[[col]]) | duplicated(data[[col]], fromLast = TRUE)
    dup[is.na(data[[col]])] <- FALSE
    
    if (include_all) {
      col_res <- data.frame(
        column = col,
        row = seq_along(dup),
        value = data[[col]],
        check_type = "unique",
        expected = "unique",
        valid = !dup
      )
    } else {
      bad <- which(dup)
      
      if (length(bad) == 0) {
        col_res <- data.frame(
          column = character(0),
          row = integer(0),
          value = character(0),
          check_type = character(0),
          expected = character(0),
          valid = logical(0)
        )
      } else {
        col_res <- data.frame(
          column = col,
          row = bad,
          value = data[[col]][bad],
          check_type = "unique",
          expected = "unique",
          valid = FALSE
        )
      }
    }
    
    return(col_res)
  }))
  
  return(results)
}

check_allowed_values <- function(dict, data, include_all = FALSE) {
  # Get columns with valid allowed values
  avals <- dict$ColName[!is.na(dict$AllowedValues) &
                          dict$AllowedValues != ""]
  
  # Find intersecting columns
  cols_to_check <- intersect(avals, colnames(data))
  
  # Check values
  if (length(cols_to_check) == 0) {
    col_res <- data.frame(
      column = character(0),
      row = integer(0),
      value = character(0),
      check_type = character(0),
      expected = character(0),
      valid = logical(0)
    )
    
    return(col_res)
  }
  
  results <- do.call(rbind, lapply(cols_to_check, function(col) {
    pattern <- dict$AllowedValues[dict$ColName == col]
    required <- dict$Required[dict$ColName == col]
    delim <- dict$Delimiter[dict$ColName == col]
    
    na_allowed <- required == "optional"
    # Normalize delimiter: treat NA and literal "NA" as no delimiter
    if (length(delim) == 0L || is.na(delim[1L])) {
      delim_use <- NA_character_
    } else {
      delim_use <- as.character(delim[1L])
      if (!is.na(delim_use) && identical(delim_use, "NA")) {
        delim_use <- NA_character_
      }
    }
    
    na_allowed <- required == "optional"
    
    ok <- sapply(seq_along(data[[col]]), function(i) {
      val <- data[[col]][i]
      if (is.na(val) && na_allowed) return(TRUE)
      if (pattern == "") return(TRUE)
      if (is.na(delim_use)) {
        vals <- val
      } else {
        vals <- unlist(strsplit(val, delim_use, fixed = TRUE))
      }
      all(grepl(paste0("^", gsub(";", "|", pattern), "$"), vals))
    })
    
    if (include_all) {
      col_res <- data.frame(
        column = col,
        row = seq_along(ok),
        value = data[[col]],
        check_type = "allowed_values",
        expected = pattern,
        valid = ok
      )
    } else {
      bad <- which(!ok)
      
      if (length(bad) == 0) {
        col_res <- data.frame(
          column = character(0),
          row = integer(0),
          value = character(0),
          check_type = character(0),
          expected = character(0),
          valid = logical(0)
        )
      } else {
        col_res <- data.frame(
          column = col,
          row = bad,
          value = data[[col]][bad],
          check_type = "allowed_values",
          expected = pattern,
          valid = FALSE
        )
      }
    }
    
    return(col_res)
  }))
  
  return(results)
}

check_dictionary_values <- function(file_dir, dict, data, include_all = FALSE) {
  # List available dictionary files
  owl_files <- list.files(file_dir, pattern = ".owl")
  csv_files <- list.files(file_dir, pattern = ".csv")
  
  # Get columns that have dictionary files
  ovals <- sapply(dict$ColName, function(x) owl_files[grep(paste0("^cmd_", x, "(_enums|_metric|\\.)"), owl_files)])
  cvals <- sapply(dict$ColName, function(x) csv_files[grep(paste0("^cmd_", x, "(_enums|_metric|\\.)"), csv_files)])
  
  ofiles <- ovals[which(lengths(ovals) > 0)]
  cfiles <- cvals[which(lengths(cvals) > 0)]
  
  # Find intersecting columns
  cols_to_check <- intersect(unique(c(names(ofiles), names(cfiles))),
                             colnames(data)) 
  
  # Load preferred file for each column
  aval_pref_list <- lapply(cols_to_check, function(col) {
    ofile <- ofiles[[col]]
    cfile <- cfiles[[col]]
    
    if (!is.null(ofile)) {
      avals <- import_owl_id_label(file.path(file_dir, ofile))$label
      pref_files[[col]] <<- ofile
    } else if (!is.null(cfile)) {
      avals <- read.csv(file.path(file_dir, cfile))$label
      val_file <- cfile
    } else {
      # Fallback in case neither an OWL nor CSV file is found
      avals <- character(0)
      val_file <- NA_character_
    }
    
    list(
      avals = avals,
      val_file = val_file
    )
  })
  names(aval_pref_list) <- cols_to_check
  
  aval_list <- lapply(aval_pref_list, `[[`, "avals")
  pref_files <- lapply(aval_pref_list, `[[`, "val_file")
  names(aval_list) <- cols_to_check
  
  # Check values
  if (length(cols_to_check) == 0) {
    col_res <- data.frame(
      column = character(0),
      row = integer(0),
      value = character(0),
      check_type = character(0),
      expected = character(0),
      valid = logical(0)
    )
    
    return(col_res)
  }
  
  results <- do.call(rbind, lapply(cols_to_check, function(col) {
    avals <- aval_list[[col]]
    val_file <- pref_files[[col]]
    required <- dict$Required[dict$ColName == col]
    delim <- dict$Delimiter[dict$ColName == col]
    
    na_allowed <- required == "optional"
    
    ok <- sapply(seq_along(data[[col]]), function(i) {
      val <- data[[col]][i]
      
      if (is.na(val) && na_allowed) return(TRUE)
      if (length(avals) == 0) return(TRUE)

      # Handle missing or sentinel delimiters ("NA") by not splitting
      if (length(delim) == 0L || is.na(delim) || identical(delim, "NA")) {
        vals <- val
      } else {
        vals <- unlist(strsplit(val, delim, fixed = TRUE))
      }
      all(vals %in% avals)
    })
    
    if (include_all) {
      col_res <- data.frame(
        column = col,
        row = seq_along(ok),
        value = data[[col]],
        check_type = "allowed_values",
        expected = paste0("See \'", val_file, "\'"),
        valid = ok
      )
    } else {
      bad <- which(!ok)
      
      if (length(bad) == 0) {
        col_res <- data.frame(
          column = character(0),
          row = integer(0),
          value = character(0),
          check_type = character(0),
          expected = character(0),
          valid = logical(0)
        )
      } else {
        col_res <- data.frame(
          column = col,
          row = bad,
          value = data[[col]][bad],
          check_type = "allowed_values",
          expected = paste0("See \'", val_file, "\'"),
          valid = FALSE
        )
      }
    }
    
    return(col_res)
  }))
  
  return(results)
}

