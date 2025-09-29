# R/rates.R                                                                 
# Design rules (from spec):                                                   
# - PHP is the base currency with fixed rate 1.00.                           
# - Other rates are stored as "PHP per 1 unit of FX" (e.g., USD = 52.00).    
# - Convert using: amount_dst = amount_src * (rate[src] / rate[dst]).         
# - No rounding here; printing/rounding happens in formatters/UI.              

source("R/validation.R")                                                       # Reuse centralized guards + normalization helpers.

# ─────────────────────────────────────────────────────────────────────────────
# set_rate(state, code, rate_php_per_unit)
# Purpose : record/overwrite a currency's rate in state$rates.
# Inputs  : state (list), code (string), rate_php_per_unit (numeric > 0).
# Returns : updated state (explicit functional flow).
# Notes   : PHP is immutable at 1.00; assert_rate enforces this.
# ─────────────────────────────────────────────────────────────────────────────
set_rate <- function(state, code, rate_php_per_unit) {
  code <- ensure_currency(code)                                                # Normalize + validate code (e.g., "usd" → "USD").
  assert_rate(code, rate_php_per_unit)                                         # Enforce positivity and PHP immutability.

  state$rates[code] <- rate_php_per_unit                                       # O(1) named assignment into the rates vector.
  return(state)                                                                # Return updated state for chaining in callers.
}

# ─────────────────────────────────────────────────────────────────────────────
# get_rate(state, code)
# Purpose : read the stored rate (numeric or NA_real_ if unset).
# Inputs  : state (list), code (string).
# Returns : double or NA_real_.
# ─────────────────────────────────────────────────────────────────────────────
get_rate <- function(state, code) {
  code <- ensure_currency(code)                                                # Normalize + validate before indexing.
  state$rates[[code]]                                                          # Return the scalar rate for that code.
}

# ─────────────────────────────────────────────────────────────────────────────
# convert_amount(state, amount, src, dst)
# Purpose : convert `amount` from currency `src` to currency `dst`.
# Inputs  : state (list), amount (numeric > 0), src/dst (strings).
# Returns : numeric (double) representing amount in `dst`.
# Spec    : amount_dst = amount_src * ( rate[src] / rate[dst] ).
# Errors  : if either src or dst rate is unset (NA), fail with guidance.
# ─────────────────────────────────────────────────────────────────────────────
convert_amount <- function(state, amount, src, dst) {
  assert_amount(amount)                                                        # Single, finite, positive numeric.
  src <- ensure_currency(src); dst <- ensure_currency(dst)                     # Normalize + validate codes (handles case/space).

  rs <- state$rates[[src]]                                                     # rs = PHP per 1 unit of source currency.
  rd <- state$rates[[dst]]                                                     # rd = PHP per 1 unit of destination currency.

  if (is.na(rs))                                                               # If source rate is missing…
    stop("Please record the rate for ", src, " first.", call. = FALSE)         # …instruct the user to record it.
  if (is.na(rd))                                                               # If destination rate is missing…
    stop("Please record the rate for ", dst, " first.", call. = FALSE)         # …same actionable message.

  amount * (rs / rd)                                                        
}
