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

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || identical(x, "")) y else x
}

read_text_file <- function(path) {
  if (!file.exists(path)) {
    return("")
  }
  paste(readLines(path, warn = FALSE), collapse = "\n")
}

write_json <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(x, path, pretty = TRUE, auto_unbox = TRUE, null = "null")
}

read_state <- function(path) {
  if (!file.exists(path)) {
    stop("State file not found: ", path)
  }
  jsonlite::fromJSON(path, simplifyVector = FALSE)
}

empty_state <- function(issue_number, bioproject_id, attachment_url, doi) {
  list(
    issue_number = as.integer(issue_number),
    bioproject_id = bioproject_id,
    attachment_url = attachment_url,
    doi = doi,
    status = "initialized",
    metadata_file = NULL,
    mapping_file = NULL,
    accepted_mappings = list(),
    pending_questions = list(),
    resolved_questions = list(),
    created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    updated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )
}

find_target_from_override <- function(override_text, dict_cols, fallback) {
  txt <- trimws(tolower(override_text))
  if (txt == "" || txt == "confirm") return(fallback)
  if (txt %in% c("skip", "ignore", "none")) return(NA_character_)

  dict_low <- tolower(dict_cols)
  hits <- dict_cols[dict_low %in% unlist(strsplit(gsub("[^a-z0-9_ ]", " ", txt), "\\s+"))]
  if (length(hits) > 0) return(hits[1])

  in_text <- dict_cols[vapply(dict_cols, function(x) grepl(tolower(x), txt, fixed = TRUE), logical(1))]
  if (length(in_text) > 0) return(in_text[1])

  fallback
}

args <- parse_args(commandArgs(trailingOnly = TRUE))
command <- args[["command"]]
state_file <- args[["state-file"]]

if (is.null(command) || is.null(state_file)) {
  stop("Usage: agent_state.R --command=<init|update_mapping|apply_response|status> --state-file=<path> [other args]")
}

if (command == "init") {
  issue_number <- args[["issue-number"]]
  if (is.null(issue_number) || issue_number == "") {
    stop("--issue-number is required for init")
  }

  state <- empty_state(
    issue_number = issue_number,
    bioproject_id = args[["bioproject-id"]] %||% "",
    attachment_url = args[["attachment-url"]] %||% "",
    doi = args[["doi"]] %||% ""
  )
  write_json(state, state_file)
  cat("STATE_STATUS=initialized\n")
  cat("PENDING_COUNT=0\n")
  quit(status = 0)
}

if (command == "update_mapping") {
  mapping_file <- args[["mapping-file"]]
  questions_file <- args[["questions-file"]]
  metadata_file <- args[["metadata-file"]]

  if (is.null(mapping_file) || is.null(questions_file)) {
    stop("--mapping-file and --questions-file are required for update_mapping")
  }

  state <- read_state(state_file)
  mapping <- jsonlite::fromJSON(mapping_file, simplifyVector = FALSE)
  questions <- jsonlite::fromJSON(questions_file, simplifyVector = FALSE)

  state$mapping_file <- mapping_file
  state$metadata_file <- metadata_file %||% state$metadata_file
  state$accepted_mappings <- mapping$accepted %||% list()
  state$pending_questions <- questions$questions %||% list()
  state$status <- if (length(state$pending_questions) > 0) "awaiting_curator_response" else "ready_for_generation"
  state$updated_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

  write_json(state, state_file)
  cat("STATE_STATUS=", state$status, "\n", sep = "")
  cat("PENDING_COUNT=", length(state$pending_questions), "\n", sep = "")
  quit(status = 0)
}

if (command == "apply_response") {
  comment_file <- args[["comment-file"]]
  dict_file <- args[["dict-file"]]
  if (is.null(comment_file) || is.null(dict_file)) {
    stop("--comment-file and --dict-file are required for apply_response")
  }

  state <- read_state(state_file)
  comment <- read_text_file(comment_file)
  dict <- utils::read.csv(dict_file, stringsAsFactors = FALSE)
  dict_cols <- dict$col.name

  pending <- state$pending_questions %||% list()
  resolved <- state$resolved_questions %||% list()

  if (length(pending) == 0) {
    state$status <- "ready_for_generation"
    write_json(state, state_file)
    cat("STATE_STATUS=ready_for_generation\n")
    cat("PENDING_COUNT=0\n")
    quit(status = 0)
  }

  keep_pending <- list()
  for (q in pending) {
    qid <- q$id
    confirm_pattern <- paste0("(?i)(✅\\s*", qid, "|", qid, "\\s*:\\s*confirm)")
    override_pattern <- paste0("(?i)", qid, "\\s*:\\s*([^\\n\\r]+)")

    is_confirm <- grepl(confirm_pattern, comment, perl = TRUE)
    override_match <- regexec(override_pattern, comment, perl = TRUE)
    override_groups <- regmatches(comment, override_match)[[1]]

    if (is_confirm || length(override_groups) > 1) {
      override_text <- if (length(override_groups) > 1) trimws(override_groups[2]) else "confirm"
      resolved_target <- find_target_from_override(override_text, dict_cols, q$target_col)

      resolved[[length(resolved) + 1]] <- list(
        id = qid,
        input_col = q$input_col,
        target_col = resolved_target,
        confidence = q$confidence,
        decision = if (tolower(override_text) == "confirm") "confirm" else "override",
        response = override_text
      )
    } else {
      keep_pending[[length(keep_pending) + 1]] <- q
    }
  }

  state$pending_questions <- keep_pending
  state$resolved_questions <- resolved
  state$status <- if (length(keep_pending) == 0) "ready_for_generation" else "awaiting_curator_response"
  state$updated_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

  write_json(state, state_file)
  cat("STATE_STATUS=", state$status, "\n", sep = "")
  cat("PENDING_COUNT=", length(keep_pending), "\n", sep = "")
  quit(status = 0)
}

if (command == "status") {
  state <- read_state(state_file)
  cat("STATE_STATUS=", state$status, "\n", sep = "")
  cat("PENDING_COUNT=", length(state$pending_questions %||% list()), "\n", sep = "")
  quit(status = 0)
}

stop("Unsupported command: ", command)
