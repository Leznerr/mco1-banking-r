# R/clean_derive.R
# Purpose:
# - Clean raw branch/location records and conservatively derive missing geographic
#   coordinates using a reference lookup table.
# - Only impute latitude/longitude when BOTH values are absent in the raw row and
#   a matching lookup record provides a complete pair.
#
# Inputs:
#   raw_df     : data.frame with at least columns `key` (default "BranchCode"),
#                `Latitude`, and `Longitude`. Latitude/Longitude may arrive as
#                character strings (possibly blank) or numerics.
#   geo_lookup : data.frame containing the same key column plus `Latitude` and
#                `Longitude` numerics/strings used for imputation.
#   key        : column name (string) used to join `raw_df` with `geo_lookup`.
#
# Returns:
#   A copy of `raw_df` where Latitude/Longitude are numeric columns and a
#   logical `GeoImputed` flag marks rows whose coordinates were filled from the
#   lookup table.
#
# Notes:
# - Imputation happens *only* when both coordinates are missing in the raw row.
#   Rows that already contain at least one coordinate remain untouched; this
#   conservative rule prevents us from overriding partially trusted data.
# - Blank strings are treated as missing values prior to numeric conversion.
# - The lookup itself may contain strings or numerics; they are sanitized before
#   joining. Missing lookup coordinates will not be imputed.

clean_derive <- function(raw_df, geo_lookup, key = "BranchCode") {
  stopifnot(is.data.frame(raw_df))                                                # Require tabular input
  required_cols <- c(key, "Latitude", "Longitude")
  stopifnot(all(required_cols %in% names(raw_df)))                                # Ensure mandatory columns present

  df <- raw_df                                                                    # Work on a copy to avoid side-effects

  # Normalize latitude/longitude in the raw data to numeric vectors
  lat_chr <- trimws(as.character(df$Latitude))
  lon_chr <- trimws(as.character(df$Longitude))
  lat_chr[lat_chr == ""] <- NA_character_                                        # Treat blanks as missing
  lon_chr[lon_chr == ""] <- NA_character_
  df$Latitude  <- suppressWarnings(as.numeric(lat_chr))                           # Convert to numeric (blank -> NA)
  df$Longitude <- suppressWarnings(as.numeric(lon_chr))

  df$GeoImputed <- FALSE                                                          # Track which rows were imputed

  if (missing(geo_lookup) || is.null(geo_lookup)) {                               # Nothing to impute without lookup
    return(df)
  }

  stopifnot(is.data.frame(geo_lookup))                                            # Lookup must be tabular as well
  stopifnot(all(required_cols %in% names(geo_lookup)))                            # Must contain matching key + coords

  # Sanitize lookup coordinates and key column
  lookup_key <- trimws(as.character(geo_lookup[[key]]))
  lookup_lat <- trimws(as.character(geo_lookup$Latitude))
  lookup_lon <- trimws(as.character(geo_lookup$Longitude))
  lookup_lat[lookup_lat == ""] <- NA_character_
  lookup_lon[lookup_lon == ""] <- NA_character_
  lookup_lat <- suppressWarnings(as.numeric(lookup_lat))
  lookup_lon <- suppressWarnings(as.numeric(lookup_lon))

  # Match each raw row to the lookup via the key column
  raw_key   <- trimws(as.character(df[[key]]))
  match_idx <- match(raw_key, lookup_key)                                         # NA when no lookup entry exists

  # Impute only when both raw coordinates are missing AND lookup has a full pair
  both_missing <- is.na(df$Latitude) & is.na(df$Longitude)
  has_match    <- !is.na(match_idx)
  lookup_full  <- has_match & !is.na(lookup_lat[match_idx]) & !is.na(lookup_lon[match_idx])
  to_impute    <- both_missing & lookup_full

  if (any(to_impute)) {
    df$Latitude[to_impute]  <- lookup_lat[match_idx[to_impute]]
    df$Longitude[to_impute] <- lookup_lon[match_idx[to_impute]]
    df$GeoImputed[to_impute] <- TRUE
  }

  df
}
