# R/interest.R                                                               
# Design rules (from spec):
# - Daily interest = End-of-Day Balance × (annual_rate / 365).               
# - Balances are stored and computed in PHP (double precision).               
# - Functions here are pure (no I/O); they return numeric values/data frames. 

# Bring in validation helpers
source("R/validation.R")                                                      # Provides assert_days() and other guards if needed.

# Domain constant for clarity and single-point edit (if ever needed)
DAYS_PER_YEAR <- 365                                                          # Gregorian simplification appropriate for this course spec.

# ─────────────────────────────────────────────────────────────────────────────
# Internal scalar validator for non-negative numeric values
# Purpose : ensure balances passed to interest functions are valid doubles.
# ─────────────────────────────────────────────────────────────────────────────
.assert_nonneg_scalar <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1 || !is.finite(x) || x < 0) {           # Must be a single finite numeric ≥ 0.
    stop(name, " must be a non-negative number.", call. = FALSE)              # Clear, user-facing message for faster debugging.
  }
  invisible(TRUE)                                                             # Return silently when the check passes.
}

# ─────────────────────────────────────────────────────────────────────────────
# Internal scalar validator for annual rate proportions (0 < r < 1)
# Purpose : enforce sensible annual rate inputs (e.g., 0.05 for 5%).
# ─────────────────────────────────────────────────────────────────────────────
.assert_rate_prop <- function(r) {
  if (!is.numeric(r) || length(r) != 1 || !is.finite(r) || r <= 0 || r >= 1) {# Proportion strictly between 0 and 1.
    stop("Annual rate must be a proportion in (0,1), e.g., 0.05 for 5%.", 
         call. = FALSE)
  }
  invisible(TRUE)
}

# ─────────────────────────────────────────────────────────────────────────────
# daily_interest(balance_php, annual_rate)
# Purpose : compute one day's interest on a given balance in PHP.
# Inputs  : balance_php (≥0), annual_rate in (0,1).
# Returns : numeric (double) interest for that single day (no rounding).
# Formula : interest = balance × (annual_rate / 365).
# ─────────────────────────────────────────────────────────────────────────────
daily_interest <- function(balance_php, annual_rate) {
  .assert_nonneg_scalar(balance_php, "Balance")                               # Validate canonical balance input.
  .assert_rate_prop(annual_rate)                                              # Validate annual rate proportion.
  r_daily <- annual_rate / DAYS_PER_YEAR                                      # Convert annual proportion to a daily rate.
  balance_php * r_daily                                                        # Return raw interest (double); caller formats if needed.
}

# ─────────────────────────────────────────────────────────────────────────────
# simulate_interest(start_balance_php, days, annual_rate)
# Purpose : produce an N-day compounded table: Day | Interest | Balance.
# Inputs  : start_balance_php (≥0), days (integer ≥1), annual_rate in (0,1).
# Returns : data.frame(day, interest, balance) without rounding.
# Method  : vectorized compounding to keep O(n) and numerically stable:
#           balances[d]  = start × (1 + r_daily)^d
#           interests[d] = balances[d] − balances[d−1] (with d=1 using start)
# ─────────────────────────────────────────────────────────────────────────────
simulate_interest <- function(start_balance_php, days, annual_rate) {
  .assert_nonneg_scalar(start_balance_php, "Starting balance")                # Non-negative starting balance.
  assert_days(days)                                                           # Enforce integer day count ≥ 1 (central validator).
  .assert_rate_prop(annual_rate)                                              # Annual rate sanity check.

  r_daily <- annual_rate / DAYS_PER_YEAR                                      # Daily proportional rate (e.g., 0.05/365).
  day <- seq_len(days)                                                        # Day index: 1..N (integer vector).

  # Vectorized compounding of end-of-day balances.
  balances <- start_balance_php * (1 + r_daily) ^ day                         # End-of-day balances for each day.

  # Per-day interest is the day-to-day increase in balance.
  # For day 1, interest is balances[1] - start; for others, use differences.
  interests <- c(balances[1] - start_balance_php, diff(balances))             # Length N; aligns 1:1 with 'day'.

  # Construct a tidy table; leave numeric values unrounded for accuracy.
  data.frame(
    day      = day,                                                           # Day number (1..N).
    interest = interests,                                                     # Raw per-day interest (double).
    balance  = balances,                                                      # End-of-day balance (double).
    row.names = NULL                                                          # Clean row names for nicer printing.
  )
}
