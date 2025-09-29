# Dataset schema validations and invariants

source("R/utils_log.R")  # Logging helpers for consistent diagnostics

required_columns <- c(  # Enumerate mandatory dataset columns
  "Region", "MainIsland", "Province", "FundingYear", "TypeOfWork", "StartDate",
  "ActualCompletionDate", "ApprovedBudgetForContract", "ContractCost", "Contractor",
  "Latitude", "Longitude"
)

validate_schema <- function(df) {  # Validate schema presence and basic typing
  if (!is.data.frame(df)) {  # Ensure input is a data frame
    stop("validate_schema expects a data frame")  # Abort on incorrect type
  }
  if (nrow(df) == 0) {  # Require at least one row
    stop("Dataset must contain at least one row")  # Report empty dataset issue
  }
  dup <- names(df)[duplicated(names(df))]  # Detect duplicate column headers
  if (length(dup) > 0) {  # When duplicates found
    stop(sprintf("Duplicate columns detected: %s", paste(dup, collapse = ", ")))  # Provide explicit error
  }
  missing_cols <- setdiff(required_columns, names(df))  # Determine missing required columns
  if (length(missing_cols) > 0) {  # If any columns absent
    stop(sprintf("Missing required columns: %s", paste(missing_cols, collapse = ", ")))  # Abort with details
  }
  numeric_candidate <- suppressWarnings(as.numeric(df$FundingYear))  # Attempt numeric coercion of FundingYear
  invalid_numeric <- !is.na(df$FundingYear) & is.na(numeric_candidate)  # Flag non-convertible values
  if (any(invalid_numeric)) {  # When conversion fails
    stop("FundingYear contains non-numeric values")  # Enforce integer coercibility
  }
  fractional <- !is.na(numeric_candidate) & abs(numeric_candidate - round(numeric_candidate)) > 1e-6  # Identify fractional years
  if (any(fractional)) {  # Reject fractional funding years
    stop("FundingYear must be whole numbers")  # Communicate invariant violation
  }
  log_info("schema validated rows=%d", nrow(df))  # Emit validation log
  invisible(df)  # Return input invisibly for chaining
}

assert_year_filter <- function(df, allowed_years = 2021:2023) {  # Ensure dataset funding years fall within allowed window
  years <- unique(df$FundingYear)  # Collect unique years present
  invalid <- setdiff(stats::na.omit(years), allowed_years)  # Determine out-of-range values ignoring NA
  if (length(invalid) > 0) {  # If invalid years found
    stop(sprintf("FundingYear outside allowed range: %s", paste(sort(invalid), collapse = ", ")))  # Abort with diff summary
  }
  invisible(df)  # Return dataset invisibly
}
