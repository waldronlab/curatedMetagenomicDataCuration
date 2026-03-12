#!/usr/bin/env Rscript

parse_args <- function(args) {
  parsed <- list()
  for (arg in args) {
    if (startsWith(arg, "--")) {
      kv <- strsplit(sub("^--", "", arg), "=", fixed = TRUE)[[1]]
      key <- kv[1]
      value <- if (length(kv) > 1) paste(kv[-1], collapse = "=") else ""
      parsed[[key]] <- value
    }
  }
  parsed
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || identical(x, "")) y else x

resolve_lookup_functions <- function() {
  sra_fun <- NULL
  biosample_fun <- NULL

  sra_fun <- tryCatch(getExportedValue("curatedMetagenomicDataCuration", "bioproject_to_sra_metadata"), error = function(e) NULL)
  biosample_fun <- tryCatch(getExportedValue("curatedMetagenomicDataCuration", "bioproject_to_biosample_metadata"), error = function(e) NULL)

  if (is.null(sra_fun) || is.null(biosample_fun)) {
    if (file.exists("R/bioproject_to_sra_metadata.R")) source("R/bioproject_to_sra_metadata.R")
    if (file.exists("R/bioproject_to_biosample_metadata.R")) source("R/bioproject_to_biosample_metadata.R")

    if (exists("bioproject_to_sra_metadata", mode = "function")) {
      sra_fun <- get("bioproject_to_sra_metadata", mode = "function")
    }
    if (exists("bioproject_to_biosample_metadata", mode = "function")) {
      biosample_fun <- get("bioproject_to_biosample_metadata", mode = "function")
    }
  }

  if (is.null(sra_fun) || is.null(biosample_fun)) {
    stop("Could not resolve bioproject lookup functions")
  }

  list(sra_fun = sra_fun, biosample_fun = biosample_fun)
}

safe_read_tabular <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("tsv", "txt")) {
    return(utils::read.delim(path, stringsAsFactors = FALSE, check.names = FALSE))
  }

  out <- tryCatch(
    utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) NULL
  )

  if (!is.null(out)) return(out)
  utils::read.delim(path, stringsAsFactors = FALSE, check.names = FALSE)
}

standardize_names <- function(df) {
  names(df) <- trimws(names(df))
  df
}

choose_join_keys <- function(manual_df, lookup_df) {
  manual_cols <- names(manual_df)
  lookup_cols <- names(lookup_df)

  candidates <- list(
    c("sample_id", "BioSample"),
    c("sample_id", "Sample"),
    c("accession", "accession"),
    c("biosample_id", "biosample_id"),
    c("Run", "Run")
  )

  for (pair in candidates) {
    if (pair[1] %in% manual_cols && pair[2] %in% lookup_cols) {
      return(list(by_manual = pair[1], by_lookup = pair[2]))
    }
  }

  shared <- intersect(manual_cols, lookup_cols)
  if (length(shared) > 0) {
    return(list(by_manual = shared[1], by_lookup = shared[1]))
  }

  NULL
}

merge_metadata <- function(manual_df, lookup_df) {
  if (is.null(manual_df) && is.null(lookup_df)) return(NULL)
  if (is.null(manual_df)) return(lookup_df)
  if (is.null(lookup_df)) return(manual_df)

  keys <- choose_join_keys(manual_df, lookup_df)
  if (is.null(keys)) {
    max_n <- max(nrow(manual_df), nrow(lookup_df))
    if (nrow(manual_df) < max_n) {
      manual_df[(nrow(manual_df) + 1):max_n, ] <- NA
    }
    if (nrow(lookup_df) < max_n) {
      lookup_df[(nrow(lookup_df) + 1):max_n, ] <- NA
    }

    overlap <- intersect(names(manual_df), names(lookup_df))
    if (length(overlap) > 0) {
      names(lookup_df)[names(lookup_df) %in% overlap] <- paste0(names(lookup_df)[names(lookup_df) %in% overlap], "_lookup")
    }
    return(cbind(manual_df, lookup_df, stringsAsFactors = FALSE))
  }

  dplyr::left_join(manual_df, lookup_df, by = setNames(keys$by_lookup, keys$by_manual))
}

args <- parse_args(commandArgs(trailingOnly = TRUE))

bioproject_id <- args[["bioproject-id"]] %||% ""
attachment_url <- args[["attachment-url"]] %||% ""
output_metadata <- args[["output-metadata"]]
output_summary <- args[["output-summary"]]

if (is.null(output_metadata) || is.null(output_summary)) {
  stop("Usage: agent_metadata_lookup.R --bioproject-id=<id> --attachment-url=<url> --output-metadata=<path> --output-summary=<path>")
}

manual_df <- NULL
if (attachment_url != "") {
  tmp <- tempfile(fileext = ".csv")
  ok <- tryCatch({
    utils::download.file(attachment_url, tmp, mode = "wb", quiet = TRUE)
    TRUE
  }, error = function(e) FALSE)

  if (ok) {
    manual_df <- safe_read_tabular(tmp)
    manual_df <- standardize_names(manual_df)
  }
}

lookup_df <- NULL
if (bioproject_id != "") {
  funcs <- resolve_lookup_functions()
  sra <- tryCatch(funcs$sra_fun(bioproject_id), error = function(e) NULL)
  biosample <- tryCatch(funcs$biosample_fun(bioproject_id), error = function(e) NULL)

  sra_df <- if (!is.null(sra) && !is.null(sra$full_metadata)) sra$full_metadata else NULL
  if (!is.null(sra_df)) sra_df <- standardize_names(sra_df)
  if (!is.null(biosample)) biosample <- standardize_names(biosample)

  if (!is.null(sra_df) && !is.null(biosample)) {
    if ("BioSample" %in% names(sra_df) && "accession" %in% names(biosample)) {
      lookup_df <- dplyr::left_join(sra_df, biosample, by = c("BioSample" = "accession"))
    } else {
      lookup_df <- dplyr::bind_cols(sra_df, biosample)
    }
  } else if (!is.null(sra_df)) {
    lookup_df <- sra_df
  } else if (!is.null(biosample)) {
    lookup_df <- biosample
  }
}

merged_df <- merge_metadata(manual_df, lookup_df)

if (is.null(merged_df)) {
  merged_df <- data.frame(stringsAsFactors = FALSE)
}

dir.create(dirname(output_metadata), recursive = TRUE, showWarnings = FALSE)
utils::write.csv(merged_df, output_metadata, row.names = FALSE, na = "")

summary <- list(
  bioproject_id = bioproject_id,
  attachment_provided = attachment_url != "",
  manual_rows = if (is.null(manual_df)) 0 else nrow(manual_df),
  lookup_rows = if (is.null(lookup_df)) 0 else nrow(lookup_df),
  merged_rows = nrow(merged_df),
  merged_cols = ncol(merged_df)
)
jsonlite::write_json(summary, output_summary, pretty = TRUE, auto_unbox = TRUE)

cat("MERGED_ROWS=", summary$merged_rows, "\n", sep = "")
cat("MERGED_COLS=", summary$merged_cols, "\n", sep = "")
