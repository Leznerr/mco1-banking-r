# Data cleaning transforms for standardized downstream analytics

suppressPackageStartupMessages({  # Suppress library startup output
  library(dplyr)  # Data manipulation verbs
  library(stringr)  # Text normalization helpers
  library(lubridate)  # Flexible date parsing
})

source("R/utils_log.R")  # Import logging utilities

parse_dates <- function(x) {  # Robustly parse heterogeneous date strings
  if (inherits(x, "Date")) {  # If already Date class
    return(x)  # Return as-is
  }
  if (inherits(x, "POSIXt")) {  # Convert POSIXt objects
    return(as.Date(x))  # Coerce to Date
  }
  orders <- c("Y-m-d", "m/d/Y", "d/m/Y", "Ymd", "d-b-Y", "d-B-Y", "B d, Y")  # Supported date formats
  parsed <- suppressWarnings(lubridate::parse_date_time(x, orders = orders, tz = "UTC"))  # Parse using lubridate
  as.Date(parsed)  # Return as Date (NA when parsing fails)
}

parse_money <- function(x) {  # Convert currency strings to numeric
  if (is.numeric(x)) {  # Allow numeric inputs to pass through
    return(as.numeric(x))  # Ensure numeric type
  }
  cleaned <- gsub("[^0-9\.-]", "", x)  # Remove non-numeric characters except signs and decimals
  as.numeric(cleaned)  # Convert to numeric with NA for invalid entries
}

clean_text_title <- function(x) {  # Title-case normalization with whitespace squish
  x <- stringr::str_squish(x)  # Compress internal whitespace
  stringr::str_to_title(x)  # Convert to title case respecting locales
}

clean_text_squish <- function(x) {  # Squish-only normalization
  stringr::str_squish(x)  # Remove redundant whitespace without case change
}

coerce_latlon <- function(x, min_val, max_val) {  # Convert lat/lon values and invalidate out-of-range
  numeric <- suppressWarnings(as.numeric(x))  # Attempt numeric conversion
  invalid <- !is.na(numeric) & (numeric < min_val | numeric > max_val)  # Identify out-of-range values
  numeric[invalid] <- NA_real_  # Set invalid positions to NA
  numeric  # Return sanitized numeric vector
}

clean_data <- function(df) {  # Main cleaning routine
  original_names <- names(df)  # Preserve original column ordering
  before_pair_na <- sum(is.na(df$Latitude) & is.na(df$Longitude))  # Capture NA pairs before cleaning
  df <- df %>% mutate(  # Apply vectorized transformations
    StartDate = parse_dates(StartDate),  # Parse start dates into Date objects
    ActualCompletionDate = parse_dates(ActualCompletionDate),  # Parse completion dates
    ApprovedBudgetForContract = parse_money(ApprovedBudgetForContract),  # Normalize budget currency
    ContractCost = parse_money(ContractCost),  # Normalize contract cost currency
    FundingYear = suppressWarnings(as.integer(round(as.numeric(FundingYear)))),  # Coerce funding year to integer
    Latitude = coerce_latlon(Latitude, -90, 90),  # Clean latitude values
    Longitude = coerce_latlon(Longitude, -180, 180),  # Clean longitude values
    Region = clean_text_title(Region),  # Normalize region names
    MainIsland = clean_text_title(MainIsland),  # Normalize island names
    Province = clean_text_title(Province),  # Normalize province names
    Contractor = clean_text_title(Contractor),  # Normalize contractor names
    TypeOfWork = clean_text_squish(TypeOfWork)  # Normalize work type strings without case change
  )
  geo_stats <- df %>%  # Derive province mean coordinates for imputation
    filter(!is.na(Province) & !is.na(Latitude) & !is.na(Longitude)) %>%  # Restrict to rows with complete coordinates
    group_by(Province) %>%  # Group by province for averaging
    summarise(lat_mean = mean(Latitude), lon_mean = mean(Longitude), .groups = "drop")  # Compute means per province
  needs_impute <- which(is.na(df$Latitude) & is.na(df$Longitude) & !is.na(df$Province))  # Identify rows eligible for imputation
  if (length(needs_impute) > 0) {  # Proceed when there are eligible rows
    province_idx <- match(df$Province[needs_impute], geo_stats$Province)  # Match provinces to computed means
    valid_idx <- which(!is.na(province_idx))  # Retain rows with available means
    rows_to_impute <- needs_impute[valid_idx]  # Actual row indices for imputation
    matched_means <- province_idx[valid_idx]  # Matching mean rows indices
    df$Latitude[rows_to_impute] <- geo_stats$lat_mean[matched_means]  # Impute latitude from province mean
    df$Longitude[rows_to_impute] <- geo_stats$lon_mean[matched_means]  # Impute longitude from province mean
  }
  after_pair_na <- sum(is.na(df$Latitude) & is.na(df$Longitude))  # Count NA pairs after imputation
  log_info("cleaning applied imputed_pairs=%d", before_pair_na - after_pair_na)  # Log number of pairs imputed
  df <- df[, original_names, drop = FALSE]  # Restore original column ordering explicitly
  df  # Return cleaned data frame
}
