# tests/test_clean_derive.R
# Purpose: lock in the geo-cleaning and conservative imputation rules applied by
# clean_derive(). The scenarios below ensure we only fill coordinates when both
# latitude and longitude are absent, never when a single coordinate is present.

source("R/clean_derive.R")

# Helper for tolerant numeric comparisons (mirrors other test files)
expect_near <- function(x, y, tol = 1e-6) {
  stopifnot(isTRUE(all.equal(x, y, tolerance = tol, check.attributes = FALSE)))
}

# -----------------------------------------------------------------------------
# Synthetic raw data mimicking branch records with mixed coordinate completeness
# -----------------------------------------------------------------------------
raw_df <- data.frame(
  BranchCode = c("BR-001", "BR-002", "BR-003", "BR-004", "BR-999"),
  Latitude   = c("14.5500", "",        "",        "14.6300", ""),
  Longitude  = c("120.9800", "",        "121.0200", "",       ""),
  stringsAsFactors = FALSE
)

# Reference lookup table holding authoritative coordinates for known branches
geo_lookup <- data.frame(
  BranchCode = c("BR-001", "BR-002", "BR-003", "BR-004"),
  Latitude   = c(14.5500, 14.6100, 14.6200, 14.6300),
  Longitude  = c(120.9800, 121.0100, 121.0200, 121.0300),
  stringsAsFactors = FALSE
)

clean_df <- clean_derive(raw_df, geo_lookup)

# Row 1: already complete → untouched and not marked as imputed
expect_near(clean_df$Latitude[1], 14.55)
expect_near(clean_df$Longitude[1], 120.98)
stopifnot(identical(clean_df$GeoImputed[1], FALSE))

# Row 2: both coordinates blank → imputed from lookup and flagged
expect_near(clean_df$Latitude[2], 14.61)
expect_near(clean_df$Longitude[2], 121.01)
stopifnot(identical(clean_df$GeoImputed[2], TRUE))

# Row 3: single coordinate present (Longitude only) → keep provided value, no imputation
stopifnot(is.na(clean_df$Latitude[3]))
expect_near(clean_df$Longitude[3], 121.02)
stopifnot(identical(clean_df$GeoImputed[3], FALSE))

# Row 4: single coordinate present (Latitude only) → retain latitude, no imputation
expect_near(clean_df$Latitude[4], 14.63)
stopifnot(is.na(clean_df$Longitude[4]))
stopifnot(identical(clean_df$GeoImputed[4], FALSE))

# Row 5: no lookup entry → remain NA with no imputation flag
stopifnot(is.na(clean_df$Latitude[5]))
stopifnot(is.na(clean_df$Longitude[5]))
stopifnot(identical(clean_df$GeoImputed[5], FALSE))

cat("✅ clean_derive geo-imputation tests passed\n")
