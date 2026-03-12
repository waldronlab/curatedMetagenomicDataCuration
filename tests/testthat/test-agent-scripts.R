## Tests for the agent CLI scripts in inst/scripts/
## These tests invoke the scripts via Rscript subprocesses so that CLI
## argument parsing and quit() calls do not affect the test runner.

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

agent_script <- function(name) {
  system.file("scripts", name, package = "curatedMetagenomicDataCuration")
}

run_agent <- function(script_name, args = character(), env = character()) {
  script <- agent_script(script_name)
  if (script == "") {
    # Fall back to the source tree for development-time testing
    script <- file.path(
      system.file(package = "curatedMetagenomicDataCuration"),
      "..", "..", "inst", "scripts", script_name
    )
    script <- normalizePath(script, mustWork = FALSE)
    if (!file.exists(script)) {
      # Try relative from the package root
      pkg_root <- rprojroot::find_package_root_file()
      script <- file.path(pkg_root, "inst", "scripts", script_name)
    }
  }
  skip_if(!file.exists(script), paste("Script not found:", script_name))

  env_set <- c(
    "GITHUB_TOKEN=",          # disable LLM calls in all agent tests
    env
  )
  result <- system2(
    "Rscript", c("--vanilla", script, args),
    stdout = TRUE, stderr = TRUE,
    env = env_set
  )
  list(
    output = result,
    status = attr(result, "status") %||% 0L
  )
}

`%||%` <- function(x, y) if (is.null(x)) y else x

# Minimal data dictionary for mapping tests
make_mini_dict <- function(path) {
  d <- data.frame(
    col.name = c("study_name", "subject_id", "sample_id", "body_site",
                 "age", "sex", "target_condition", "control",
                 "disease", "country"),
    description = rep("", 10),
    corpus.type = rep("", 10),
    allowedvalues = rep("", 10),
    stringsAsFactors = FALSE
  )
  utils::write.csv(d, path, row.names = FALSE)
  invisible(path)
}

# ---------------------------------------------------------------------------
# agent_state.R — init command
# ---------------------------------------------------------------------------

test_that("agent_state init creates a valid JSON state file", {
  tmp_state <- tempfile(fileext = ".json")
  on.exit(unlink(tmp_state))

  res <- run_agent("agent_state.R", c(
    "--command=init",
    paste0("--state-file=", tmp_state),
    "--issue-number=42",
    "--bioproject-id=PRJNA123",
    "--doi=10.1234/test"
  ))

  expect_equal(res$status, 0L, info = paste(res$output, collapse = "\n"))
  expect_true(file.exists(tmp_state))

  state <- jsonlite::fromJSON(tmp_state, simplifyVector = FALSE)
  expect_equal(state$issue_number, 42L)
  expect_equal(state$bioproject_id, "PRJNA123")
  expect_equal(state$doi, "10.1234/test")
  expect_equal(state$status, "initialized")
  expect_true(!is.null(state$created_at))
})

test_that("agent_state init outputs STATE_STATUS=initialized and PENDING_COUNT=0", {
  tmp_state <- tempfile(fileext = ".json")
  on.exit(unlink(tmp_state))

  res <- run_agent("agent_state.R", c(
    "--command=init",
    paste0("--state-file=", tmp_state),
    "--issue-number=1"
  ))

  expect_true(any(grepl("STATE_STATUS=initialized", res$output)))
  expect_true(any(grepl("PENDING_COUNT=0", res$output)))
})

test_that("agent_state init fails without --issue-number", {
  tmp_state <- tempfile(fileext = ".json")
  on.exit(unlink(tmp_state))

  res <- suppressWarnings(run_agent("agent_state.R", c(
    "--command=init",
    paste0("--state-file=", tmp_state)
  )))
  expect_false(res$status == 0L)
})

# ---------------------------------------------------------------------------
# agent_state.R — update_mapping command
# ---------------------------------------------------------------------------

test_that("agent_state update_mapping with no pending => ready_for_generation", {
  tmp_dir <- tempdir()
  tmp_state <- tempfile(tmpdir = tmp_dir, fileext = ".json")
  tmp_mapping <- tempfile(tmpdir = tmp_dir, fileext = ".json")
  tmp_questions <- tempfile(tmpdir = tmp_dir, fileext = ".json")
  on.exit(unlink(c(tmp_state, tmp_mapping, tmp_questions)))

  # Init first
  run_agent("agent_state.R", c(
    "--command=init",
    paste0("--state-file=", tmp_state),
    "--issue-number=7"
  ))

  # Write mapping with no pending questions
  jsonlite::write_json(
    list(accepted = list(list(input_col = "study", target_col = "study_name", confidence = 0.99)),
         pending = list()),
    tmp_mapping, auto_unbox = TRUE
  )
  jsonlite::write_json(list(questions = list()), tmp_questions, auto_unbox = TRUE)

  res <- run_agent("agent_state.R", c(
    "--command=update_mapping",
    paste0("--state-file=", tmp_state),
    paste0("--mapping-file=", tmp_mapping),
    paste0("--questions-file=", tmp_questions)
  ))

  expect_equal(res$status, 0L)
  expect_true(any(grepl("STATE_STATUS=ready_for_generation", res$output)))
  expect_true(any(grepl("PENDING_COUNT=0", res$output)))

  state <- jsonlite::fromJSON(tmp_state, simplifyVector = FALSE)
  expect_equal(state$status, "ready_for_generation")
  expect_length(state$accepted_mappings, 1L)
})

test_that("agent_state update_mapping with pending => awaiting_curator_response", {
  tmp_dir <- tempdir()
  tmp_state <- tempfile(tmpdir = tmp_dir, fileext = ".json")
  tmp_mapping <- tempfile(tmpdir = tmp_dir, fileext = ".json")
  tmp_questions <- tempfile(tmpdir = tmp_dir, fileext = ".json")
  on.exit(unlink(c(tmp_state, tmp_mapping, tmp_questions)))

  run_agent("agent_state.R", c(
    "--command=init",
    paste0("--state-file=", tmp_state),
    "--issue-number=8"
  ))

  jsonlite::write_json(list(accepted = list(), pending = list(
    list(input_col = "weird_col", target_col = "disease", confidence = 0.6)
  )), tmp_mapping, auto_unbox = TRUE)
  jsonlite::write_json(list(questions = list(
    list(id = "Q1", input_col = "weird_col", target_col = "disease",
         confidence = 0.6, sample_values = list("Value1"))
  )), tmp_questions, auto_unbox = TRUE)

  res <- run_agent("agent_state.R", c(
    "--command=update_mapping",
    paste0("--state-file=", tmp_state),
    paste0("--mapping-file=", tmp_mapping),
    paste0("--questions-file=", tmp_questions)
  ))

  expect_equal(res$status, 0L)
  expect_true(any(grepl("STATE_STATUS=awaiting_curator_response", res$output)))
  expect_true(any(grepl("PENDING_COUNT=1", res$output)))
})

# ---------------------------------------------------------------------------
# agent_state.R — apply_response command
# ---------------------------------------------------------------------------

make_confirmed_state <- function(tmp_state, tmp_pending_question) {
  # Helper: create a state file already in awaiting_curator_response
  init_args <- c(
    "--command=init",
    paste0("--state-file=", tmp_state),
    "--issue-number=9"
  )
  run_agent("agent_state.R", init_args)

  tmp_mapping <- tempfile(fileext = ".json")
  tmp_questions <- tempfile(fileext = ".json")

  jsonlite::write_json(list(accepted = list(), pending = list(
    list(input_col = "myDisease", target_col = "disease", confidence = 0.7)
  )), tmp_mapping, auto_unbox = TRUE)
  jsonlite::write_json(list(questions = list(
    list(id = "Q1", input_col = "myDisease", target_col = "disease",
         confidence = 0.7, sample_values = list())
  )), tmp_questions, auto_unbox = TRUE)

  run_agent("agent_state.R", c(
    "--command=update_mapping",
    paste0("--state-file=", tmp_state),
    paste0("--mapping-file=", tmp_mapping),
    paste0("--questions-file=", tmp_questions)
  ))

  unlink(c(tmp_mapping, tmp_questions))
}

test_that("agent_state apply_response resolves Q1 via checkmark confirm", {
  tmp_dir <- tempdir()
  tmp_state <- tempfile(tmpdir = tmp_dir, fileext = ".json")
  tmp_comment <- tempfile(tmpdir = tmp_dir, fileext = ".txt")
  tmp_dict <- tempfile(tmpdir = tmp_dir, fileext = ".csv")
  on.exit(unlink(c(tmp_state, tmp_comment, tmp_dict)))

  make_confirmed_state(tmp_state, NULL)
  make_mini_dict(tmp_dict)

  writeLines("\u2705 Q1", tmp_comment)  # ✅ Q1

  res <- run_agent("agent_state.R", c(
    "--command=apply_response",
    paste0("--state-file=", tmp_state),
    paste0("--comment-file=", tmp_comment),
    paste0("--dict-file=", tmp_dict)
  ))

  expect_equal(res$status, 0L, info = paste(res$output, collapse = "\n"))
  expect_true(any(grepl("STATE_STATUS=ready_for_generation", res$output)))
  expect_true(any(grepl("PENDING_COUNT=0", res$output)))

  state <- jsonlite::fromJSON(tmp_state, simplifyVector = FALSE)
  expect_length(state$pending_questions, 0L)
  expect_length(state$resolved_questions, 1L)
  expect_equal(state$resolved_questions[[1]]$decision, "confirm")
})

test_that("agent_state apply_response resolves Q1 via skip override", {
  tmp_dir <- tempdir()
  tmp_state <- tempfile(tmpdir = tmp_dir, fileext = ".json")
  tmp_comment <- tempfile(tmpdir = tmp_dir, fileext = ".txt")
  tmp_dict <- tempfile(tmpdir = tmp_dir, fileext = ".csv")
  on.exit(unlink(c(tmp_state, tmp_comment, tmp_dict)))

  make_confirmed_state(tmp_state, NULL)
  make_mini_dict(tmp_dict)

  writeLines("Q1: skip", tmp_comment)

  res <- run_agent("agent_state.R", c(
    "--command=apply_response",
    paste0("--state-file=", tmp_state),
    paste0("--comment-file=", tmp_comment),
    paste0("--dict-file=", tmp_dict)
  ))

  expect_equal(res$status, 0L)
  state <- jsonlite::fromJSON(tmp_state, simplifyVector = FALSE)
  expect_length(state$pending_questions, 0L)
  resolved <- state$resolved_questions[[1]]
  expect_true(is.na(resolved$target_col) || identical(resolved$target_col, NA_character_) ||
                is.null(resolved$target_col))
  expect_equal(resolved$decision, "override")
})

test_that("agent_state apply_response resolves Q1 via column name override", {
  tmp_dir <- tempdir()
  tmp_state <- tempfile(tmpdir = tmp_dir, fileext = ".json")
  tmp_comment <- tempfile(tmpdir = tmp_dir, fileext = ".txt")
  tmp_dict <- tempfile(tmpdir = tmp_dir, fileext = ".csv")
  on.exit(unlink(c(tmp_state, tmp_comment, tmp_dict)))

  make_confirmed_state(tmp_state, NULL)
  make_mini_dict(tmp_dict)

  writeLines("Q1: target_condition", tmp_comment)

  res <- run_agent("agent_state.R", c(
    "--command=apply_response",
    paste0("--state-file=", tmp_state),
    paste0("--comment-file=", tmp_comment),
    paste0("--dict-file=", tmp_dict)
  ))

  expect_equal(res$status, 0L)
  state <- jsonlite::fromJSON(tmp_state, simplifyVector = FALSE)
  resolved <- state$resolved_questions[[1]]
  expect_equal(resolved$target_col, "target_condition")
})

test_that("agent_state apply_response leaves unaddressed questions pending", {
  tmp_dir <- tempdir()
  tmp_state <- tempfile(tmpdir = tmp_dir, fileext = ".json")
  tmp_comment <- tempfile(tmpdir = tmp_dir, fileext = ".txt")
  tmp_dict <- tempfile(tmpdir = tmp_dir, fileext = ".csv")
  on.exit(unlink(c(tmp_state, tmp_comment, tmp_dict)))

  make_confirmed_state(tmp_state, NULL)
  make_mini_dict(tmp_dict)

  # Comment that doesn't mention Q1 at all
  writeLines("This is a general comment without any answers.", tmp_comment)

  res <- run_agent("agent_state.R", c(
    "--command=apply_response",
    paste0("--state-file=", tmp_state),
    paste0("--comment-file=", tmp_comment),
    paste0("--dict-file=", tmp_dict)
  ))

  expect_equal(res$status, 0L)
  expect_true(any(grepl("STATE_STATUS=awaiting_curator_response", res$output)))
  expect_true(any(grepl("PENDING_COUNT=1", res$output)))
})

# ---------------------------------------------------------------------------
# agent_state.R — status command
# ---------------------------------------------------------------------------

test_that("agent_state status command reads an existing state file", {
  tmp_state <- tempfile(fileext = ".json")
  on.exit(unlink(tmp_state))

  run_agent("agent_state.R", c(
    "--command=init",
    paste0("--state-file=", tmp_state),
    "--issue-number=20"
  ))

  res <- run_agent("agent_state.R", c(
    "--command=status",
    paste0("--state-file=", tmp_state)
  ))

  expect_equal(res$status, 0L)
  expect_true(any(grepl("STATE_STATUS=initialized", res$output)))
  expect_true(any(grepl("PENDING_COUNT=0", res$output)))
})

# ---------------------------------------------------------------------------
# agent_column_mapping.R — heuristic mode (no GITHUB_TOKEN)
# ---------------------------------------------------------------------------

test_that("agent_column_mapping produces accepted/pending/unmapped JSON (heuristic)", {
  tmp_dir <- tempdir()
  tmp_input <- tempfile(tmpdir = tmp_dir, fileext = ".csv")
  tmp_dict <- tempfile(tmpdir = tmp_dir, fileext = ".csv")
  tmp_mapping <- tempfile(tmpdir = tmp_dir, fileext = ".json")
  tmp_questions <- tempfile(tmpdir = tmp_dir, fileext = ".json")
  on.exit(unlink(c(tmp_input, tmp_dict, tmp_mapping, tmp_questions)))

  # Input with an exact match, a near match, and a dissimilar column
  input <- data.frame(
    study_name = c("StudyA"),
    subjectid = c("S001"),       # near match for subject_id
    xyzabc_nomatch = c("foo"),   # unlikely to match anything
    stringsAsFactors = FALSE
  )
  utils::write.csv(input, tmp_input, row.names = FALSE)
  make_mini_dict(tmp_dict)

  res <- run_agent("agent_column_mapping.R", c(
    paste0("--input-metadata=", tmp_input),
    paste0("--dict-file=", tmp_dict),
    paste0("--output-mapping=", tmp_mapping),
    paste0("--output-questions=", tmp_questions)
  ))

  expect_equal(res$status, 0L, info = paste(res$output, collapse = "\n"))
  expect_true(file.exists(tmp_mapping))
  expect_true(file.exists(tmp_questions))

  mapping <- jsonlite::fromJSON(tmp_mapping, simplifyVector = FALSE)
  expect_true(!is.null(mapping$accepted))
  expect_true(!is.null(mapping$pending))

  # study_name is an exact match — should be accepted
  accepted_cols <- vapply(mapping$accepted, function(x) x$input_col, character(1))
  expect_true("study_name" %in% accepted_cols)
})

test_that("agent_column_mapping outputs ACCEPTED_COUNT and PENDING_COUNT", {
  tmp_dir <- tempdir()
  tmp_input <- tempfile(tmpdir = tmp_dir, fileext = ".csv")
  tmp_dict <- tempfile(tmpdir = tmp_dir, fileext = ".csv")
  tmp_mapping <- tempfile(tmpdir = tmp_dir, fileext = ".json")
  tmp_questions <- tempfile(tmpdir = tmp_dir, fileext = ".json")
  on.exit(unlink(c(tmp_input, tmp_dict, tmp_mapping, tmp_questions)))

  input <- data.frame(study_name = "StudyA", stringsAsFactors = FALSE)
  utils::write.csv(input, tmp_input, row.names = FALSE)
  make_mini_dict(tmp_dict)

  res <- run_agent("agent_column_mapping.R", c(
    paste0("--input-metadata=", tmp_input),
    paste0("--dict-file=", tmp_dict),
    paste0("--output-mapping=", tmp_mapping),
    paste0("--output-questions=", tmp_questions)
  ))

  expect_true(any(grepl("ACCEPTED_COUNT=", res$output)))
  expect_true(any(grepl("PENDING_COUNT=", res$output)))
})

test_that("agent_column_mapping assigns sequential Q-ids to pending questions", {
  tmp_dir <- tempdir()
  tmp_input <- tempfile(tmpdir = tmp_dir, fileext = ".csv")
  tmp_dict <- tempfile(tmpdir = tmp_dir, fileext = ".csv")
  tmp_mapping <- tempfile(tmpdir = tmp_dir, fileext = ".json")
  tmp_questions <- tempfile(tmpdir = tmp_dir, fileext = ".json")
  on.exit(unlink(c(tmp_input, tmp_dict, tmp_mapping, tmp_questions)))

  # Many oddly-named columns to force pending/unmapped
  input <- data.frame(
    kool_aid_col1 = "A",
    random_xyz_99 = "B",
    stringsAsFactors = FALSE
  )
  utils::write.csv(input, tmp_input, row.names = FALSE)
  make_mini_dict(tmp_dict)

  res <- run_agent("agent_column_mapping.R", c(
    paste0("--input-metadata=", tmp_input),
    paste0("--dict-file=", tmp_dict),
    paste0("--output-mapping=", tmp_mapping),
    paste0("--output-questions=", tmp_questions),
    "--confidence-threshold=0.85"
  ))

  expect_equal(res$status, 0L)
  questions <- jsonlite::fromJSON(tmp_questions, simplifyVector = FALSE)
  ids <- vapply(questions$questions, function(q) q$id, character(1))
  if (length(ids) > 0) {
    expect_equal(ids, paste0("Q", seq_along(ids)))
  }
})

test_that("agent_column_mapping errors without required arguments", {
  res <- suppressWarnings(run_agent("agent_column_mapping.R", character()))
  expect_false(res$status == 0L)
})
