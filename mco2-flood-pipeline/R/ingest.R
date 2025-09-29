# CSV ingestion handling with strict guards

suppressPackageStartupMessages({  # Silence package load messages
  library(readr)  # CSV parsing utilities
  library(tibble)  # Tibbles for tidy data frames
})

source("R/utils_log.R")  # Use project logging helpers

validate_ingest_path <- function(path) {  # Guard for input path sanity
  if (missing(path) || is.null(path) || !nzchar(path)) {  # Ensure non-empty path provided
    stop("Input path must be provided")  # Fail fast on invalid path argument
  }
  if (!file.exists(path)) {  # Ensure file exists on disk
    stop(sprintf("Input file does not exist: %s", path))  # Raise descriptive error
  }
  info <- file.info(path)  # Fetch file metadata
  if (isTRUE(info$isdir)) {  # Reject directories masquerading as files
    stop("Input path points to a directory, expected a file")  # Inform user of misuse
  }
  if (info$size == 0) {  # Detect empty files early
    stop("Input file is empty")  # Enforce non-empty dataset requirement
  }
  invisible(NULL)  # No explicit return value
}

check_header_duplicates <- function(df) {  # Confirm headers are unique
  dup <- names(df)[duplicated(names(df))]  # Determine duplicated column names
  if (length(dup) > 0) {  # When duplicates exist
    stop(sprintf("Duplicate column headers found: %s", paste(dup, collapse = ", ")))  # Abort with details
  }
  invisible(NULL)  # Return nothing
}

ingest_csv <- function(path) {  # Main ingestion function returning tibble
  validate_ingest_path(path)  # Run path validations first
  locale <- readr::locale(encoding = "UTF-8")  # Configure UTF-8 locale for parsing
  df <- readr::read_csv(  # Parse CSV into tibble
    file = path,  # Input file path
    locale = locale,  # Locale configuration
    guess_max = 10000,  # Expand guessing horizon for diverse columns
    na = c("", "NA", "N/A", "null", "NULL"),  # Accepted NA tokens
    show_col_types = FALSE,  # Suppress column type printing
    name_repair = "minimal"  # Preserve original header names (even duplicates)
  )
  if (nrow(df) == 0) {  # Ensure dataset contains rows
    stop("Input file has no data rows")  # Abort on empty dataset
  }
  if (ncol(df) == 0) {  # Ensure dataset contains columns
    stop("Input file has no columns")  # Abort on column-less dataset
  }
  check_header_duplicates(df)  # Validate header uniqueness
  problems_tbl <- readr::problems(df)  # Capture parse problems, if any
  attr(df, "ingest_problems") <- problems_tbl  # Attach problems tibble as attribute
  log_info("ingested rows=%d cols=%d parse_issues=%d", nrow(df), ncol(df), nrow(problems_tbl))  # Emit ingestion summary log
  df  # Return tibble unchanged (no transformations)
}
