library(validate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(rdflib)
library(stringr)

import_owl_id_label <- function(path) {
  ## Load info from file
  g <- rdflib::rdf_parse(path, format = "rdfxml")
  
  ## Query with SPARQL
  query <- "
    SELECT ?id ?label
    WHERE {
      ?id a <http://www.w3.org/2002/07/owl#Class> .
      ?id <http://www.w3.org/2000/01/rdf-schema#label> ?label .
    }
  "
  df <- rdflib::rdf_query(g, query)
  
  ## Format fields
  df$id <- df$id |>
    basename() |>
    stringr::str_replace("_", ":")
  df$label <- as.character(df$label)
  dplyr::distinct(df)
  
  return(df)
}

validate_column <- function(validation_type, allowed, data) {
  if (validation_type == "character") {
    results <- unlist(lapply(data, function(x) if (is.na(x)) NA else x %in% allowed))
  } else if (validation_type == "function") {
    results <- unlist(lapply(data, function(x) if (is.na(x)) NA else allowed(x)))
  }
  
  rejected_ids <- which(!results)
  rejected <- data[rejected_ids]
  report <- list(results = results,
                 rejected_ids = rejected_ids,
                 rejected_values = rejected)
  return(report)
}

validateMetadata <- function(data_table, template_list) {
  ## Validate input
  # data_table
  
  # template_list
  
  ## Compare columns
  unvalidated_cols <- setdiff(colnames(data_table), names(template_list))
  missing_cols <- setdiff(names(template_list), colnames(data_table))
  
  effective_template <- template_list
  effective_template[missing_cols] <- NULL
  
  ## Get validation classes
  template_classes <- lapply(effective_template, class)
  
  ## Validate columns
  # Non-matching columns
  nm_cols <- c(unvalidated_cols, missing_cols)
  nm_results <- sapply(nm_cols, function(x) list(results = rep(NA, nrow(data_table)),
                                                 rejected_values = NA,
                                                 rejected_ids = NA),
                       simplify = FALSE)
  
  # NA columns
  na_cols <- effective_template[names(which(is.na(effective_template)))]
  na_results <- sapply(na_cols, function(x) list(results = rep(NA, nrow(data_table)),
                                                 rejected_values = as.character(),
                                                 rejected_ids = as.integer()),
                       simplify = FALSE)
  
  # character columns
  character_cols <- effective_template[names(which(template_classes == "character"))]
  character_results <- sapply(names(character_cols), function(x) {
    validate_column("character", character_cols[[x]], data_table[,x])
  },
  simplify = FALSE)
  
  # function columns
  function_cols <- effective_template[names(which(template_classes == "function"))]
  function_results <- sapply(names(function_cols), function(x) {
    validate_column("function", function_cols[[x]], data_table[,x])
  },
  simplify = FALSE)
  
  ## Create report
  all_results <- c(nm_results, na_results, character_results, function_results)
  report <- data.frame(column = c(names(template_list), unvalidated_cols)) %>%
    rowwise() %>%
    dplyr::mutate(template = column %in% names(template_list),
                  data = column %in% colnames(data_table)) %>%
    dplyr::mutate(results = list(all_results[[column]]$results),
                  rejected_values = list(all_results[[column]]$rejected_values),
                  rejected_ids = list(all_results[[column]]$rejected_ids))
  
  return(report)
}

plotValidation <- function(report) {
  rdat <- as.data.frame(report$results)
  names(rdat) <- report$column
  
  pdat <- rdat %>%
    pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%
    mutate(Value = factor(Value, levels = c(FALSE, TRUE, NA)))
  
  ggplot(pdat, aes(x = Variable, fill = Value)) +
    geom_bar(position = "fill") +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(values = c(palette.colors()[7], palette.colors()[4], palette.colors()[9])) +
    labs(
      title = "Validation Results",
      x = "Column",
      y = "Percent Validation Results",
      fill = "Result"
    ) +
    coord_flip()
}

#t <- import_owl(file = "inst/extdata/validation_dictionaries/cmd_age_group.owl")
#t <- read_xml("dictionaries/cmd_age_group.owl")

res <- import_owl_id_label("inst/extdata/validation_dictionaries/cmd_age_group.owl")

diet_dict <- read.csv("inst/extdata/validation_dictionaries/cmd_dietary_restriction.csv")
color_dict <- c("red", "green", "blue", "yellow")
#validate_age <- function(x) {
#    if (abs(x)-round(x) != 0) {
#        return(FALSE)
#    }
#    if (x < 0 | x > 120) {
#       return(FALSE)
#    }
#    return(TRUE)
#}

template <- list(id = function(x) !is.na(x),
                 dietary_restriction = diet_dict$label,
                 color = color_dict,
                 age = function(x) {in_range(x, 0, 120) & abs(x)-round(x) == 0},
                 species = is.character)

#ttemplate <- validator(!is.na(id),
#                       color %in% color_dict,
#                       dietary_restriction %in% diet_dict$label,
#                       in_range(age, 0, 120) & abs(age)-round(age) == 0)
#names(ttemplate) <- c("id_exists", "color_dictionary", "diet_dictionary", "age_integer_in_range")

ddata <- data.frame(id = c(1, 2, NA, 4, 5),
                    color = c("red", "green", "blue", "blue", "red"),
                    dietary_restriction = c("vegan", NA, "gluten", 1, "gluten"),
                    age = c(27, 48, 34, 23.5, 199))

#out <- confront(ddata, ttemplate)

out2 <- validateMetadata(ddata, template)

plotValidation(out2)

## Functions
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
#is_bool_str <- function(x) tolower(x) %in% c("true", "false", "t", "f", "yes", "no", "1", "0")

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
  results <- do.call(rbind, lapply(cols_to_check, function(col) {
    type <- dict$ColClass[dict$ColName == col]
    fn <- class_functions[[type]]
     
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
  results <- do.call(rbind, lapply(cols_to_check, function(col) {
    dup <- duplicated(data[[col]]) | duplicated(data[[col]], fromLast = TRUE)
    dup[is.na(data[[col]])] <- FALSE
    
    if (include_all) {
      col_res <- data.frame(
        column = col,
        row = seq_along(ok),
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

check_multiple <- function(x) {
  
}

check_allowed_values <- function(dict, data, include_all = FALSE) {
  # Get columns with valid allowed values
  avals <- dict$ColName[!is.na(dict$AllowedValues) &
                          dict$AllowedValues != ""]
  
  # Find intersecting columns
  cols_to_check <- intersect(avals, colnames(data))
  
  # Check values
  results <- do.call(rbind, lapply(cols_to_check, function(col) {
    pattern <- dict$AllowedValues[dict$ColName == col]
    required <- dict$Required[dict$ColName == col]
    
    na_allowed <- required == "optional"
    
    ok <- sapply(seq_along(data[[col]]), function(i) {
      val <- data[[col]][i]
      if (is.na(val) && na_allowed) return(TRUE)
      if (pattern == "") return(TRUE)
      grepl(paste0("^", pattern, "$"), val)
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

## Run test validation
# Import cMD data dictionary
cdd <- read.csv("inst/extdata/cMD_data_dictionary.csv",
                colClasses = "character")

# Load in test study
ts <- read.delim("inst/curated/ArtachoA_2021/ArtachoA_2021_metadata.tsv",
                 colClasses = "character")

report <- dplyr::bind_rows(
  check_required(cdd, ts),
  check_class(cdd, ts),
  check_unique(cdd, ts),
  check_allowed_values(cdd, ts)
)

full_report <- dplyr::bind_rows(
  check_required(cdd, ts, include_all = TRUE),
  check_class(cdd, ts, include_all = TRUE),
  check_unique(cdd, ts, include_all = TRUE),
  check_allowed_values(cdd, ts, include_all = TRUE)
)


