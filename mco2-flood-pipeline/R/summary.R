# Summary JSON generation

suppressPackageStartupMessages({  # Suppress package startup noise
  library(dplyr)  # Utility for distinct counts
})

source("R/utils_log.R")  # Logging helpers
source("R/io.R")  # IO helpers for writing JSON

build_summary <- function(df) {  # Assemble scalar summary metrics
  summary <- list(  # Construct summary list
    total_projects = nrow(df),  # Count of projects
    total_contractors = dplyr::n_distinct(df$Contractor, na.rm = TRUE),  # Distinct contractors count
    total_provinces = dplyr::n_distinct(df$Province, na.rm = TRUE),  # Distinct provinces count
    global_avg_delay = if (all(is.na(df$CompletionDelayDays))) NA_real_ else mean(df$CompletionDelayDays, na.rm = TRUE),  # Mean delay
    total_savings = sum(df$CostSavings, na.rm = TRUE)  # Aggregate savings
  )
  log_info("summary computed projects=%d", summary$total_projects)  # Log summary generation
  summary  # Return summary list
}

write_summary <- function(df, path) {  # Persist summary JSON to disk
  summary <- build_summary(df)  # Build summary list
  write_summary_json(summary, path)  # Write JSON using IO helper
}
