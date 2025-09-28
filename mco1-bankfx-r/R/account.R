# R/account.R                                                                 
# Responsibilities:                                                          
# - Register the account holder's name (non-empty),                          
# - Deposit funds in PHP or a foreign currency (FX converted first),           
# - Withdraw funds in PHP or FX (convert first; block overdrafts).           
# Notes:                                                                       
# - All balances are stored in PHP only (double precision).                  
# - No rounding here; the UI prints 2 decimals via formatters.R.               
#   -> separation of concerns.

# Bring in validators and conversion utilities
source("R/validation.R")                                                       # assert_amount(), assert_name(), ensure_currency(), etc.
source("R/rates.R")                                                            # convert_amount() uses spec: amount * (rs/rd)

# ─────────────────────────────────────────────────────────────────────────────
# Internal helper: convert arbitrary currency to PHP (no rounding)
# Purpose : centralize the FX→PHP step used by deposit/withdraw.
# Returns : numeric PHP amount (double).
# ─────────────────────────────────────────────────────────────────────────────
.amount_in_php <- function(state, amount, currency) {
  assert_amount(amount)                                                        # Guard: single, finite, positive numeric.
  code <- ensure_currency(currency)                                            # Normalize/validate code (e.g., "usd" → "USD").
  if (code == "PHP") {                                                         # If already PHP…
    return(amount)                                                             # …no conversion needed.
  }
  convert_amount(state, amount, code, "PHP")                                   # Else apply spec conversion via rates.R.
}

# ─────────────────────────────────────────────────────────────────────────────
# register_account(state, name)
# Purpose : set the account holder's name; reset balance to 0.0 (fresh start).
# Inputs  : state (list), name (string; non-blank after trim).
# Returns : updated state.
# ─────────────────────────────────────────────────────────────────────────────
register_account <- function(state, name) {
  assert_name(name)                                                            # Guard: must be a single non-blank string.
  clean <- trimws(name)                                                        # Trim leading/trailing whitespace for neatness.
  state$account$name <- clean                                                  # Record the holder's name.
  state$account$balance_php <- 0.0                                             # Per spec/design: registration initializes/clears balance.
  return(state)                                                                # Functional style: return updated state.
}

# ─────────────────────────────────────────────────────────────────────────────
# deposit(state, amount, currency)
# Purpose : add funds to balance; convert FX to PHP first.
# Inputs  : amount (numeric > 0), currency (e.g., "PHP","USD",…).
# Returns : updated state with increased PHP balance.
# Errors  : raised by validators (bad amount/currency) or by convert_amount
#           if the required FX rate is unset.
# ─────────────────────────────────────────────────────────────────────────────
deposit <- function(state, amount, currency) {
  php <- .amount_in_php(state, amount, currency)                               # Convert to canonical PHP amount.
  state$account$balance_php <- state$account$balance_php + php                 # Add to running balance (double precision).
  return(state)                                                                # Return updated state for chaining.
}

# ─────────────────────────────────────────────────────────────────────────────
# withdraw(state, amount, currency)
# Purpose : remove funds from balance; convert FX to PHP first; forbid overdrafts.
# Inputs  : amount (numeric > 0), currency code.
# Returns : updated state with decreased PHP balance.
# Errors  : "Insufficient funds." if converted amount exceeds balance; plus
#           any validation or unset-rate errors from helpers.
# ─────────────────────────────────────────────────────────────────────────────
withdraw <- function(state, amount, currency) {
  php <- .amount_in_php(state, amount, currency)                               # Convert desired withdrawal to PHP.
  if (php > state$account$balance_php) {                                       # Overdraft guard: do not allow negative balance.
    stop("Insufficient funds.", call. = FALSE)                                 # Clear, user-facing error message.
  }
  state$account$balance_php <- state$account$balance_php - php                 # Subtract; remain in double precision.
  return(state)                                                                # Return updated state.
}

# ─────────────────────────────────────────────────────────────────────────────
# Optional convenience: get_balance_php(state)
# Purpose : read the current canonical balance (PHP).
# Returns : numeric (double).
# ─────────────────────────────────────────────────────────────────────────────
get_balance_php <- function(state) {
  state$account$balance_php                                                    # Simple accessor; handy in UI/tests.
}
