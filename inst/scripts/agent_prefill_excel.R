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

resolve_generate_excel <- function() {
  fun <- tryCatch(getExportedValue("curatedMetagenomicDataCuration", "generate_data_entry_excel"), error = function(e) NULL)
  if (!is.null(fun)) {
    return(fun)
  }

  if (file.exists("R/generate_data_entry_excel.R")) {
    source("R/generate_data_entry_excel.R")
  }

  if (exists("generate_data_entry_excel", mode = "function")) {
    return(get("generate_data_entry_excel", mode = "function"))
  }

  stop("Could not resolve generate_data_entry_excel")
}

collect_mappings <- function(state) {
  accepted <- state$accepted_mappings %||% list()
  resolved <- state$resolved_questions %||% list()

  mappings <- list()

  if (length(accepted) > 0) {
    for (m in accepted) {
      if (!is.null(m$input_col) && !is.null(m$target_col) && !is.na(m$target_col) && m$target_col != "") {
        mappings[[length(mappings) + 1]] <- list(input_col = m$input_col, target_col = m$target_col)
      }
    }
  }

  if (length(resolved) > 0) {
    for (r in resolved) {
      if (!is.null(r$input_col) && !is.null(r$target_col) && !is.na(r$target_col) && r$target_col != "") {
        mappings[[length(mappings) + 1]] <- list(input_col = r$input_col, target_col = r$target_col)
      }
    }
  }

  if (length(mappings) == 0) return(data.frame(input_col = character(0), target_col = character(0), stringsAsFactors = FALSE))

  out <- unique(data.frame(
    input_col = vapply(mappings, function(x) x$input_col, character(1)),
    target_col = vapply(mappings, function(x) x$target_col, character(1)),
    stringsAsFactors = FALSE
  ))
  out[!(is.na(out$target_col) | out$target_col == ""), , drop = FALSE]
}

args <- parse_args(commandArgs(trailingOnly = TRUE))
state_file <- args[["state-file"]]
input_metadata <- args[["input-metadata"]]
dict_file <- args[["dict-file"]]
validation_metadata <- args[["validation-metadata"]]
output_excel <- args[["output-excel"]]
output_prefill <- args[["output-prefill"]]

if (is.null(state_file) || is.null(input_metadata) || is.null(dict_file) || is.null(validation_metadata) || is.null(output_excel) || is.null(output_prefill)) {
  stop("Usage: agent_prefill_excel.R --state-file=<json> --input-metadata=<csv> --dict-file=<csv> --validation-metadata=<csv> --output-excel=<xlsx> --output-prefill=<csv>")
}

state <- jsonlite::fromJSON(state_file, simplifyVector = FALSE)
source_df <- utils::read.csv(input_metadata, stringsAsFactors = FALSE, check.names = FALSE)
dict <- utils::read.csv(dict_file, stringsAsFactors = FALSE, check.names = FALSE)

mappings <- collect_mappings(state)
output_cols <- dict$col.name

prefill_df <- as.data.frame(matrix("", nrow = nrow(source_df), ncol = length(output_cols)), stringsAsFactors = FALSE)
names(prefill_df) <- output_cols

if (nrow(mappings) > 0 && nrow(source_df) > 0) {
  for (i in seq_len(nrow(mappings))) {
    from <- mappings$input_col[i]
    to <- mappings$target_col[i]

    if (from %in% names(source_df) && to %in% names(prefill_df)) {
      prefill_df[[to]] <- as.character(source_df[[from]])
    }
  }
}

dir.create(dirname(output_prefill), recursive = TRUE, showWarnings = FALSE)
utils::write.csv(prefill_df, output_prefill, row.names = FALSE, na = "")

generate_excel <- resolve_generate_excel()
generate_excel(
  dict_file = dict_file,
  metadata_file = validation_metadata,
  output_file = output_excel,
  prefill_metadata = prefill_df
)

cat("PREFILL_ROWS=", nrow(prefill_df), "\n", sep = "")
cat("PREFILL_COLS=", ncol(prefill_df), "\n", sep = "")
cat("OUTPUT_EXCEL=", output_excel, "\n", sep = "")
