#' @title Normalize Values
#' @description Clean and normalize metadata values
#' @param x Character vector
#' @return Cleaned character vector
normalize_values <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x[!is.na(x) & nzchar(x)]
}

#' @title Split Multi-Value Fields
#' @description Split fields with multiple values (semicolon/pipe/comma separated)
#' @param x Character vector
#' @return Character vector with split values
split_values <- function(x) {
  x <- normalize_values(x)
  if (length(x) == 0) return(character(0))
  x <- unlist(strsplit(x, "\\s*[;|,]\\s*"))
  normalize_values(x)
}

#' @title Update Value Counts
#' @description Update count table with new values
#' @param counts Named integer vector
#' @param values Character vector of values
#' @return Updated counts
update_counts <- function(counts, values) {
  vals <- split_values(values)
  if (length(vals) == 0) return(counts)
  tbl <- table(vals)
  for (n in names(tbl)) {
    counts[n] <- if (!is.na(counts[n])) counts[n] + tbl[[n]] else tbl[[n]]
  }
  counts
}

#' @title Find Column by Candidates
#' @description Find first matching column from candidate names
#' @param cols Column names
#' @param candidates Candidate column names to match
#' @return Matched column name or NA
find_col <- function(cols, candidates) {
  idx <- which(tolower(cols) %in% tolower(candidates))
  if (length(idx) > 0) cols[idx[1]] else NA_character_
}

#' @title Collect Metadata Statistics
#' @description Collect statistics from all validated studies
#' @param results List of validation results (from validate_single_study)
#' @return List with metadata statistics
#' @export
collect_metadata_statistics <- function(results) {
  total_samples <- 0
  age_group_counts <- integer(0)
  body_site_counts <- integer(0)
  published_year_counts <- integer(0)
  country_counts <- integer(0)
  ancestry_counts <- integer(0)
  disease_counts <- integer(0)
  sex_counts <- integer(0)

  for (result in results) {
    if (is.null(result$data)) next

    data <- result$data
    cols <- colnames(data)
    total_samples <- total_samples + nrow(data)

    # Collect statistics for each metadata field
    age_group_col <- find_col(cols, c("age_group", "agegroup"))
    if (!is.na(age_group_col)) {
      age_group_counts <- update_counts(age_group_counts, data[[age_group_col]])
    }

    body_site_col <- find_col(cols, c("body_site", "bodysite", "body_site_ontology"))
    if (!is.na(body_site_col)) {
      body_site_counts <- update_counts(body_site_counts, data[[body_site_col]])
    }

    # Extract year from study name
    year_match <- regmatches(result$study_name, regexpr("\\d{4}$", result$study_name))
    if (length(year_match) == 1 && nzchar(year_match)) {
      yr <- year_match
      published_year_counts[yr] <- if (!is.na(published_year_counts[yr]))
        published_year_counts[yr] + as.integer(nrow(data))
      else
        as.integer(nrow(data))
    }

    country_col <- find_col(cols, c("country", "host_country", "location", "geo_loc_name"))
    if (!is.na(country_col)) {
      country_counts <- update_counts(country_counts, data[[country_col]])
    }

    ancestry_col <- find_col(cols, c("ancestry", "ethnicity", "race"))
    if (!is.na(ancestry_col)) {
      ancestry_counts <- update_counts(ancestry_counts, data[[ancestry_col]])
    }

    disease_col <- find_col(cols, c("disease", "study_condition", "diagnosis", "disease_type"))
    if (!is.na(disease_col)) {
      disease_counts <- update_counts(disease_counts, data[[disease_col]])
    }

    sex_col <- find_col(cols, c("sex", "gender", "host_sex"))
    if (!is.na(sex_col)) {
      sex_counts <- update_counts(sex_counts, data[[sex_col]])
    }
  }

  list(
    total_samples = total_samples,
    age_group = age_group_counts,
    body_site = body_site_counts,
    published_year = published_year_counts,
    country = country_counts,
    ancestry = ancestry_counts,
    disease = disease_counts,
    sex = sex_counts
  )
}

#' @title Build Distribution Summary
#' @description Create top-N distribution from counts
#' @param counts Named integer vector
#' @param top_n Number of top items to include
#' @return List with top items and totals
build_distribution <- function(counts, top_n = 10) {
  if (length(counts) == 0) {
    return(list(top = list(), total_distinct = 0, total_count = 0))
  }
  counts <- sort(counts, decreasing = TRUE)
  total_count <- sum(counts)
  total_distinct <- length(counts)
  top <- head(counts, top_n)
  top_list <- lapply(names(top), function(n) {
    list(name = n, count = as.integer(top[[n]]))
  })
  list(
    top = top_list,
    total_distinct = as.integer(total_distinct),
    total_count = as.integer(total_count)
  )
}
