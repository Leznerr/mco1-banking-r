# Output helpers for deterministic persistence

suppressPackageStartupMessages({  # Suppress library startup output
  library(readr)  # CSV reading and writing
  library(jsonlite)  # JSON serialization utilities
})

source("R/utils_format.R")  # Access formatting helpers within same project

ensure_outdir <- function(path) {  # Create output directory when missing
  dir.create(path, recursive = TRUE, showWarnings = FALSE)  # Create directory structure idempotently
  path  # Return the normalized directory path
}

atomic_write <- function(write_fn, path) {  # Internal helper to guarantee atomic writes
  dir <- dirname(path)  # Determine destination directory
  ensure_outdir(dir)  # Ensure directory exists before writing
  tmp <- tempfile(pattern = basename(path), tmpdir = dir)  # Create temp file in same directory
  write_fn(tmp)  # Execute provided writer into temporary file
  file.rename(tmp, path)  # Move temp file into final location atomically
}

write_report_csv <- function(df, path, exclude = NULL, exclude_regex = NULL) {  # Persist a formatted report to CSV
  formatted <- format_dataframe(df, exclude = exclude, exclude_regex = exclude_regex)  # Apply human-friendly formatting
  atomic_write(function(tmp) {  # Use atomic write to avoid partial files
    readr::write_csv(formatted, tmp, na = "")  # Write CSV with empty NA representation
  }, path)  # Finalize write operation
  invisible(path)  # Return invisibly for chaining
}

write_summary_json <- function(data, path) {  # Persist summary list to JSON
  atomic_write(function(tmp) {  # Atomic write for JSON as well
    jsonlite::write_json(data, tmp, auto_unbox = TRUE, pretty = TRUE, digits = NA)  # Serialize with requested options
  }, path)  # Finalize atomic write
  invisible(path)  # Return invisibly
}
