# Utility logging helpers with structured context support

log_env <- new.env(parent = emptyenv())  # Dedicated environment for logger state
log_env$level <- 20  # Default level INFO represented numerically
log_env$file_conn <- NULL  # Optional file connection sink for logs
log_env$context <- list()  # Named list of current contextual fields

log_levels <- c(DEBUG = 10, INFO = 20, WARN = 30, ERROR = 40)  # Mapping of level names to numeric severity

log_set_level <- function(level) {  # Public setter for global log level
  level <- toupper(level)  # Normalize input to uppercase
  if (!level %in% names(log_levels)) {  # Validate provided level exists
    stop(sprintf("Unknown log level '%s'", level))  # Fail fast on invalid input
  }
  log_env$level <- log_levels[[level]]  # Store numeric level in environment
  invisible(NULL)  # Return nothing explicitly
}

log_set_file <- function(path = NULL) {  # Configure optional log sink file
  if (!is.null(log_env$file_conn)) {  # If a previous connection exists
    close(log_env$file_conn)  # Close it to avoid descriptor leaks
  }
  if (is.null(path)) {  # When no path provided
    log_env$file_conn <- NULL  # Clear sink connection
    return(invisible(NULL))  # Exit without side effects
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)  # Ensure directory exists
  log_env$file_conn <- file(path, open = "a")  # Open append connection for logging
  invisible(NULL)  # Return nothing explicitly
}

log_context_set <- function(context) {  # Replace current context list
  stopifnot(is.list(context))  # Guard to ensure we receive a list
  log_env$context <- context  # Store provided context
  invisible(NULL)  # Return invisibly
}

with_log_context <- function(context, expr) {  # Temporarily extend context for expression
  stopifnot(is.list(context))  # Ensure context is a list
  old_ctx <- log_env$context  # Snapshot existing context
  on.exit(log_env$context <- old_ctx, add = TRUE)  # Restore context when exiting
  log_env$context <- modifyList(old_ctx, context)  # Merge old and new context fields
  force(expr)  # Evaluate expression within modified context
} 

format_log_context <- function() {  # Helper to serialize context key-value pairs
  if (length(log_env$context) == 0) {  # No context means empty string
    return("")  # Return empty context tag
  }
  parts <- sprintf("%s=%s", names(log_env$context), unlist(log_env$context, use.names = FALSE))  # Convert context list to strings
  paste(parts, collapse = " ")  # Join parts with spaces
}

log_should_emit <- function(level_num) {  # Determine if a log should be emitted
  level_num >= log_env$level  # Compare severity against configured level
}

log_emit <- function(level_name, message) {  # Core log emission function
  timestamp <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")  # Generate ISO-8601 timestamp in UTC
  ctx <- format_log_context()  # Serialize context fields
  line <- sprintf("%s [%s] %s", timestamp, level_name, message)  # Construct base log message
  if (nzchar(ctx)) {  # Append context if non-empty
    line <- sprintf("%s %s", line, ctx)  # Append serialized context
  }
  cat(line, "\n")  # Emit to stdout
  if (!is.null(log_env$file_conn)) {  # When file sink configured
    writeLines(line, log_env$file_conn)  # Write line to file
    flush(log_env$file_conn)  # Flush to ensure durability
  }
}

log_message <- function(level, fmt, ...) {  # Format message at specified level
  level_name <- toupper(level)  # Normalize to uppercase string
  level_num <- log_levels[[level_name]]  # Fetch numeric severity
  if (log_should_emit(level_num)) {  # Check emission eligibility
    msg <- sprintf(fmt, ...)  # Interpolate message string
    log_emit(level_name, msg)  # Emit formatted log line
  }
  invisible(NULL)  # Return invisibly
}

log_debug <- function(fmt, ...) log_message("DEBUG", fmt, ...)  # Debug-level logger wrapper
log_info <- function(fmt, ...) log_message("INFO", fmt, ...)  # Info-level logger wrapper
log_warn <- function(fmt, ...) log_message("WARN", fmt, ...)  # Warning-level logger wrapper
log_error <- function(fmt, ...) log_message("ERROR", fmt, ...)  # Error-level logger wrapper
