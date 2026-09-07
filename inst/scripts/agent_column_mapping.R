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

normalize_name <- function(x) tolower(gsub("[^a-z0-9]", "", x))

sample_values <- function(df, col, n = 3) {
  vals <- unique(trimws(as.character(df[[col]])))
  vals <- vals[!is.na(vals) & vals != ""]
  head(vals, n)
}

heuristic_mapping <- function(input_df, dict_df, confidence_threshold = 0.85) {
  input_cols <- names(input_df)
  dict_cols <- dict_df$col.name

  accepted <- list()
  pending <- list()
  unmapped <- list()

  dict_norm <- vapply(dict_cols, normalize_name, character(1))

  for (col in input_cols) {
    col_norm <- normalize_name(col)
    dists <- adist(col_norm, dict_norm)
    best_idx <- which.min(dists)
    best_col <- dict_cols[best_idx]
    best_dist <- as.numeric(dists[best_idx])
    max_len <- max(nchar(col_norm), nchar(dict_norm[best_idx]), 1)
    confidence <- max(0, 1 - (best_dist / max_len))

    if (col_norm == dict_norm[best_idx]) {
      confidence <- 0.99
    }

    entry <- list(
      input_col = col,
      target_col = best_col,
      confidence = round(confidence, 3),
      sample_values = sample_values(input_df, col)
    )

    if (confidence >= confidence_threshold) {
      accepted[[length(accepted) + 1]] <- entry
    } else if (confidence >= 0.55) {
      pending[[length(pending) + 1]] <- entry
    } else {
      unmapped[[length(unmapped) + 1]] <- entry
    }
  }

  list(accepted = accepted, pending = pending, unmapped = unmapped)
}

extract_json_payload <- function(text) {
  txt <- trimws(text)
  if (grepl("^```", txt)) {
    txt <- gsub("^```[a-zA-Z]*\\n", "", txt)
    txt <- gsub("\\n```$", "", txt)
  }
  txt
}

call_github_models <- function(token, model, input_df, dict_df) {
  columns <- names(input_df)
  dict_subset <- dict_df[, intersect(c("col.name", "description", "corpus.type", "allowedvalues"), names(dict_df)), drop = FALSE]

  preview <- lapply(columns, function(col) {
    list(column = col, values = sample_values(input_df, col, 4))
  })

  prompt <- paste(
    "Map input metadata columns to the target dictionary columns.",
    "Return JSON only with keys accepted and pending.",
    "Each item must include input_col, target_col, confidence.",
    "Use confidence < 0.85 for uncertain mappings.",
    sep = "\n"
  )

  body <- list(
    model = model,
    messages = list(
      list(role = "system", content = "You are a metadata harmonization assistant. Output strict JSON."),
      list(role = "user", content = paste0(
        prompt,
        "\n\nInput column preview:\n",
        jsonlite::toJSON(preview, auto_unbox = TRUE, pretty = TRUE),
        "\n\nDictionary:\n",
        jsonlite::toJSON(dict_subset, auto_unbox = TRUE, pretty = TRUE)
      ))
    ),
    temperature = 0
  )

  resp <- httr2::request("https://models.inference.ai.azure.com/chat/completions") |>
    httr2::req_headers(
      Authorization = paste("Bearer", token),
      `Content-Type` = "application/json"
    ) |>
    httr2::req_body_json(body, auto_unbox = TRUE) |>
    httr2::req_perform()

  payload <- httr2::resp_body_json(resp, simplifyVector = FALSE)
  content <- payload$choices[[1]]$message$content
  parsed <- jsonlite::fromJSON(extract_json_payload(content), simplifyVector = FALSE)

  if (is.null(parsed$accepted)) parsed$accepted <- list()
  if (is.null(parsed$pending)) parsed$pending <- list()

  parsed
}

args <- parse_args(commandArgs(trailingOnly = TRUE))
input_file <- args[["input-metadata"]]
dict_file <- args[["dict-file"]]
output_mapping <- args[["output-mapping"]]
output_questions <- args[["output-questions"]]
confidence_threshold <- as.numeric(args[["confidence-threshold"]] %||% "0.85")
model <- args[["model"]] %||% "gpt-4o-mini"
token <- Sys.getenv("GITHUB_TOKEN", unset = "")

if (is.null(input_file) || is.null(dict_file) || is.null(output_mapping) || is.null(output_questions)) {
  stop("Usage: agent_column_mapping.R --input-metadata=<csv> --dict-file=<csv> --output-mapping=<json> --output-questions=<json>")
}

input_df <- utils::read.csv(input_file, stringsAsFactors = FALSE, check.names = FALSE)
dict_df <- utils::read.csv(dict_file, stringsAsFactors = FALSE, check.names = FALSE)

mapping <- NULL
if (token != "") {
  mapping <- tryCatch(call_github_models(token, model, input_df, dict_df), error = function(e) NULL)
}

if (is.null(mapping)) {
  mapping <- heuristic_mapping(input_df, dict_df, confidence_threshold = confidence_threshold)
}

accepted <- mapping$accepted %||% list()
pending <- mapping$pending %||% list()

questions <- list(questions = list())
if (length(pending) > 0) {
  for (i in seq_along(pending)) {
    p <- pending[[i]]
    questions$questions[[i]] <- list(
      id = paste0("Q", i),
      input_col = p$input_col,
      target_col = p$target_col,
      confidence = p$confidence,
      sample_values = p$sample_values %||% list()
    )
  }
}

dir.create(dirname(output_mapping), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(list(accepted = accepted, pending = pending, unmapped = mapping$unmapped %||% list()),
  output_mapping,
  pretty = TRUE,
  auto_unbox = TRUE,
  null = "null"
)
jsonlite::write_json(questions, output_questions, pretty = TRUE, auto_unbox = TRUE, null = "null")

cat("ACCEPTED_COUNT=", length(accepted), "\n", sep = "")
cat("PENDING_COUNT=", length(questions$questions), "\n", sep = "")
