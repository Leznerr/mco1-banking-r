# Numeric safety helpers and tabular formatting utilities

suppressPackageStartupMessages({  # Silence package startup noise
  library(dplyr)  # Data manipulation verbs
})

safe_mean <- function(x) {  # Mean that returns NA when all inputs missing
  if (all(is.na(x))) {  # Detect fully missing vector
    return(NA_real_)  # Preserve NA semantics
  }
  mean(x, na.rm = TRUE)  # Compute mean ignoring NAs
}

safe_median <- function(x) {  # Median with all-NA guard
  if (all(is.na(x))) {  # Detect absence of values
    return(NA_real_)  # Return NA when appropriate
  }
  stats::median(x, na.rm = TRUE)  # Use stats median with NA removal
}

safe_sum <- function(x) {  # Sum with NA guard
  if (all(is.na(x))) {  # If everything missing
    return(NA_real_)  # Return NA rather than zero
  }
  sum(x, na.rm = TRUE)  # Standard NA-removed sum
}

minmax_0_100 <- function(x) {  # Clamp helper ensuring range within [0, 100]
  ifelse(is.na(x), NA_real_, pmax(0, pmin(100, x)))  # Bound values while preserving NA
}

format_number <- function(x) {  # Internal formatter for numeric vectors
  x <- round(x, 2)  # Round to two decimals per requirements
  formatted <- ifelse(is.na(x), "", formatC(x, format = "f", digits = 2, big.mark = ","))  # Format with commas and decimals
  formatted  # Return formatted strings
}

format_dataframe <- function(df, exclude = NULL, exclude_regex = NULL) {  # Format data frames for presentation
  df <- as.data.frame(df, stringsAsFactors = FALSE)  # Ensure base data frame for predictable mutation
  default_exclude <- c("FundingYear", "Year", "N", "NProjects")  # Default identifier columns to leave numeric
  exclude <- unique(c(default_exclude, exclude %||% character()))  # Combine provided and default exclusions
  numeric_cols <- vapply(df, is.numeric, logical(1))  # Detect numeric columns
  name_vec <- names(df)  # Cache column names for indexing
  if (!is.null(exclude_regex)) {  # Apply regex-based exclusions when provided
    regex_mask <- grepl(exclude_regex, name_vec)  # Determine matching columns
  } else {
    regex_mask <- rep(FALSE, length(name_vec))  # Default to no regex exclusion
  }
  for (col_name in name_vec[numeric_cols]) {  # Iterate over numeric columns
    idx <- match(col_name, name_vec)  # Locate column index
    if (col_name %in% exclude || regex_mask[[idx]]) {  # Skip excluded columns
      next  # Continue to next column
    }
    df[[col_name]] <- format_number(df[[col_name]])  # Apply formatting to eligible numeric columns
  }
  df  # Return formatted data frame
}

`%||%` <- function(lhs, rhs) {  # Null-coalescing helper
  if (is.null(lhs)) rhs else lhs  # Return right-hand side when left is NULL
}
