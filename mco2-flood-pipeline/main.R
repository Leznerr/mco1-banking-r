suppressPackageStartupMessages({  # Suppress package startup messages for clean CLI output
  library(dplyr)  # Required for pipeline operations sourced downstream
})

source("R/utils_log.R")  # Structured logging helpers
source("R/utils_cli.R")  # CLI parsing utilities
source("R/io.R")  # Output writers and directory helpers
source("R/ingest.R")  # Ingestion stage
source("R/validate.R")  # Validation stage
source("R/clean.R")  # Cleaning stage
source("R/derive.R")  # Derivation stage
source("R/report1.R")  # Report 1 generation
source("R/report2.R")  # Report 2 generation
source("R/report3.R")  # Report 3 generation
source("R/summary.R")  # Summary generation

run_pipeline <- function(args) {  # Execute pipeline end-to-end
  ensure_outdir(args$outdir)  # Ensure output directory exists
  data_raw <- with_log_context(list(stage = "ingest"), ingest_csv(args$input))  # Ingest raw CSV
  with_log_context(list(stage = "validate"), validate_schema(data_raw))  # Validate schema
  data_clean <- with_log_context(list(stage = "clean"), clean_data(data_raw))  # Clean dataset
  data_derived <- with_log_context(list(stage = "derive"), derive_fields(data_clean))  # Compute derived fields
  data_filtered <- with_log_context(list(stage = "filter"), filter_years(data_derived))  # Filter for allowed years
  with_log_context(list(stage = "filter"), assert_year_filter(data_filtered))  # Ensure filter compliance
  report1 <- with_log_context(list(stage = "report1"), report_regional_efficiency(data_filtered))  # Build report 1
  report2 <- with_log_context(list(stage = "report2"), report_top_contractors(data_filtered))  # Build report 2
  report3 <- with_log_context(list(stage = "report3"), report_overrun_trends(data_filtered))  # Build report 3
  summary <- with_log_context(list(stage = "summary"), build_summary(data_filtered))  # Build summary metrics
  write_report_csv(report1, file.path(args$outdir, "report1_regional_efficiency.csv"))  # Persist report 1
  write_report_csv(report2, file.path(args$outdir, "report2_top_contractors.csv"))  # Persist report 2
  write_report_csv(report3, file.path(args$outdir, "report3_overruns_trend.csv"))  # Persist report 3
  write_summary_json(summary, file.path(args$outdir, "summary.json"))  # Persist summary JSON
  invisible(NULL)  # Return nothing
}

main <- function() {  # CLI entry point
  args <- parse_cli_args()  # Parse command line flags
  validate_cli_paths(args)  # Validate provided paths
  tryCatch(  # Execute pipeline with error handling
    run_pipeline(args),  # Run main pipeline
    error = function(e) {  # Handle runtime errors
      message_fallback <- function(msg) {  # Helper to emit fallback message
        message(msg)  # Use base message when logger unavailable
      }
      tryCatch(  # Attempt structured logging first
        log_error("pipeline failed: %s", conditionMessage(e)),  # Log error via logger
        error = function(...) message_fallback(sprintf("pipeline failed: %s", conditionMessage(e)))  # Fallback to message()
      )
      quit(status = 1)  # Exit with failure status
    }
  )
}

if (identical(environment(), globalenv())) {  # Execute only when run as script
  main()  # Invoke entry point
}
