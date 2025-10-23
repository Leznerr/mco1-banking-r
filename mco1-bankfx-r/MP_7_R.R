#########################
# Last names: Assistant
# Language: R
# Paradigm(s): Procedural + Functional (vectorized)
#########################

#### ===== Begin: R/state.R =====
# R/state.R                                           # File path header (purely informational for readers/tools)
# Single source of truth for app data and constants.  # This module defines the central state object used by the app.
# Invariants:                                         # Document the rules your data must always satisfy (helps grading & reviews).
# - PHP is the base currency and is always 1.00 (immutable via UI).         # Simplifies conversions; PHP is the canonical unit.
# - Other rates are stored as "PHP per 1 unit of FX".                        # Consistent interpretation of each rate value.
# - Account balance is stored in PHP only (non-negative).                    # Avoids FX rounding drift; keeps math simple.
# - Annual interest rate is 5% (0.05); daily = annual/365.                   # Shared constant for the interest computations.

CURRENCY_CODES <- c("PHP", "USD", "JPY", "GBP", "EUR", "CNY")  # Vector of valid currency codes; used by validators and UI menus.

new_state <- function() {                                     # Factory function: returns a fresh, clean state object.
  list(                                                        # Build and return a named list (R's flexible "struct").
    account = list(                                            # Nested list: all account-related fields live here.
      name = NULL,                                             # Not registered yet; NULL clearly signals "no name set".
      balance_php = 0.0                                        # Canonical balance stored in PHP to prevent FX drift.
    ),
    rates = c(                                                 # Named numeric vector of FX rates.
      PHP = 1.00,     # base, fixed                            # PHP is the base currency and MUST be 1.00 by definition.
      USD = NA_real_,                                          # NA_real_ = "unset" (numeric NA); forces user to record a rate.
      JPY = NA_real_,                                          # Same for JPY—unset until recorded via "Record Exchange Rate".
      GBP = NA_real_,                                          # Same for GBP.
      EUR = NA_real_,                                          # Same for EUR.
      CNY = NA_real_                                           # Same for CNY.
    ),
    annual_rate = 0.05                                         # 5% annual interest stored as a proportion (used by interest.R).
  )                                                            # End of the state list.
}                                                              # End of new_state() definition.

# Optional invariant checker (handy during debugging/tests)    # Helper to assert that a given state obeys all invariants.
check_state <- function(state) {                               # Accepts a state object and throws if anything is invalid.
  stopifnot(                                                   # stopifnot(...) stops with an error if any condition is FALSE/NA.
    is.list(state),                                            # The state must be a list (our chosen container type).
    is.list(state$account),                                    # The 'account' field itself must be a list (name + balance).
    is.numeric(state$account$balance_php),                     # The account balance must be numeric (double).
    state$account$balance_php >= 0,                            # Balance must never be negative (strong correctness guarantee).
    is.numeric(state$rates["PHP"]),                            # The 'PHP' rate slot must be numeric…
    state$rates["PHP"] == 1.00,                                # …and must equal EXACTLY 1.00 (base-currency invariant).
    identical(names(state$rates), CURRENCY_CODES),             # Ensure the rate vector has EXACTLY these names and order.
    is.numeric(state$annual_rate),                             # Interest rate must be numeric…
    state$annual_rate > 0 && state$annual_rate < 1             # …and a sensible proportion (0 < rate < 1).
  )                                                            # End of all checks; if any fails, stopifnot throws.
  invisible(TRUE)                                              # Return TRUE (invisibly) so successful checks produce no output.
}                                                              # End of check_state() definition.
#### ===== End: R/state.R =====

#### ===== Begin: R/validation.R =====
# R/validation.R                                                      
# Goal: keep bad inputs out of the core math/logic (correctness + UX).
# File header: this module holds all input guards.
# Centralizing validation improves simplicity/readability.


# ────────────────────────────────────────────────────────────────────
# Helpers (tiny, pure functions used by the asserts)
# ────────────────────────────────────────────────────────────────────
.allowed_codes <- function() {                        # Get the canonical list of currency codes
    if (exists("CURRENCY_CODES", inherits = TRUE))    # …prefer the constant from state.R if it's already loaded,
        get("CURRENCY_CODES", inherits = TRUE)        # …so we never duplicate source of truth.
    else                                              # Fallback in case validation.R is sourced before state.R:
        c("PHP", "USD", "JPY", "GBP", "EUR", "CNY")   # …still lets this file work independently in tests.
}

.sanitize_currency <- function(code) {                                 # Normalize user input into a clean currency code string.
  if (is.null(code)) stop("Currency code is missing.", call. = FALSE)  # Null means nothing was provided → clear error.
  code <- as.character(code)                                           # Coerce factors/numbers to character safely.
  code <- trimws(code)                                                 # Remove stray spaces that users might type.
  toupper(code)                                                        # Enforce uppercase (PHP, USD, …) for consistency.
}


# ────────────────────────────────────────────────────────────────────
# Assertions (fail fast with clear messages; return invisibly on pass)
# ────────────────────────────────────────────────────────────────────
assert_currency <- function(code) {                                    # Validate that a currency code is one of the allowed set.
  code <- .sanitize_currency(code)                                     # Normalize first so " usd " becomes "USD".
  allowed <- .allowed_codes()                                          # Pull the canonical set.
  if (!(code %in% allowed))                                            # If not in the set…
    stop("Invalid currency code: ", code,                              
         ". Allowed: ", paste(allowed, collapse = ", "), ".",          # …show a helpful message listing valid options.
         call. = FALSE)
  invisible(TRUE)                                                      # Return silently when validation passes.
}

assert_rate <- function(code, rate) {                                  # Validate a proposed FX rate for a given currency.
  assert_currency(code)                                                # Currency must be valid first.
  code <- .sanitize_currency(code)                                     # Work with normalized code locally.
  if (code == "PHP")                                                   # Per spec: PHP is base and immutable at 1.00.
    stop("PHP rate is fixed to 1.00.", call. = FALSE)
  if (!is.numeric(rate) || length(rate) != 1 ||                        # Must be a single numeric value…
      !is.finite(rate) || rate <= 0)                                   # …finite and strictly positive.
    stop("Rate must be a positive number (numeric, finite, > 0).", 
         call. = FALSE)
  invisible(TRUE)                                                      # All good.
}

assert_amount <- function(amount) {                                    # Validate amounts for deposit/withdraw/convert.
  if (!is.numeric(amount) || length(amount) != 1 ||                    # Must be a single numeric value…
      !is.finite(amount) || amount <= 0)                               # …finite and strictly positive.
    stop("Amount must be a positive number (numeric, finite, > 0).", 
         call. = FALSE)
  invisible(TRUE)                                                      # OK.
}

assert_days <- function(days) {                                        # Validate number of days for interest simulation.
  if (!is.numeric(days)    || length(days) != 1 ||                     # Must be a single numeric value…
      !is.finite(days)     || days < 1      ||                         # …finite and >= 1…
      days != as.integer(days))                                        # …and an integer (no fractions of days).
    stop("Days must be an integer >= 1.", call. = FALSE)
  invisible(TRUE)                                                      # OK.
}


assert_name <- function(name) {                                        # Validate account holder name at registration.
  if (!is.character(name) || length(name) != 1 ||                      # Must be a single character string…
      !nzchar(trimws(name)))                                           # …and not blank (after trimming spaces).
    stop("Account name cannot be blank.", call. = FALSE)
  invisible(TRUE)                                                      # OK.
}



ensure_currency <- function(code) {                                    # Normalize + validate currency code in one helper.
  clean <- .sanitize_currency(code)                                    # Trim/uppercase for canonical form.
  assert_currency(clean)                                               # Reuse guard to enforce allowed set.
  clean                                                                # Return sanitized code for callers.
}


#### ===== End: R/validation.R =====

#### ===== Begin: R/rates.R =====
# R/rates.R                                                                 
# Design rules (from spec):                                                   
# - PHP is the base currency with fixed rate 1.00.                           
# - Other rates are stored as "PHP per 1 unit of FX" (e.g., USD = 52.00).    
# - Convert using: amount_dst = amount_src * (rate[src] / rate[dst]).         
# - No rounding here; printing/rounding happens in formatters/UI.              


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
#### ===== End: R/rates.R =====

#### ===== Begin: R/account.R =====
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
#### ===== End: R/account.R =====

#### ===== Begin: R/interest.R =====
# R/interest.R                                                               
# Design rules (from spec):
# - Daily interest = End-of-Day Balance × (annual_rate / 365).               
# - Balances are stored and computed in PHP (double precision).               
# - Functions here are pure (no I/O); they return numeric values/data frames. 

# Bring in validation helpers

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
#### ===== End: R/interest.R =====

#### ===== Begin: R/formatters.R =====
# R/formatters.R                                                              
# Purpose:                                                                     
# - Provide a consistent "money" formatter (2 decimals; no scientific notation)
# - Print neat section headings and horizontal rules in the console
# - Offer tiny column formatters for aligned tables (optional helpers)
# Policy:                                                                      
# - Do NOT perform business/math here—only convert numbers to display strings.  

# ─────────────────────────────────────────────────────────────────────────────
# money(x, digits = 2, na = "NA")
# Purpose : Convert numeric vector x to character with fixed decimals.
# Inputs  : x (numeric vector), digits (# of decimals), na (string for NA/Inf)
# Returns : character vector the same length as x.
# Notes   : No currency symbol is added; the UI concatenates " PHP"/" USD" etc.
# ─────────────────────────────────────────────────────────────────────────────
money <- function(x, digits = 2, na = "NA") {
  if (!is.numeric(x)) {                                                        # Guard: ensure x is numeric; fail early for clarity.
    stop("money(): 'x' must be numeric.", call. = FALSE)
  }
  # formatC produces fixed decimal strings, rounding as needed and never using
  # scientific notation (format = 'f'). This preserves a consistent screen UX.
  out <- formatC(x, format = "f", digits = digits)                             # Core conversion → character strings at fixed precision.
  out[!is.finite(x)] <- na                                                     # Replace NA/Inf/-Inf with a friendly placeholder string.
  out                                                                          # Return the character vector for UI printing.
}

# ─────────────────────────────────────────────────────────────────────────────
# heading(text, width = 30, char = "=")
# Purpose : Print a simple section heading with underlines for readability.
# Inputs  : text (string), width (underline length), char (underline character)
# Returns : (invisible) NULL after printing to console.
# ─────────────────────────────────────────────────────────────────────────────
heading <- function(text, width = 30, char = "=") {
  if (!is.character(text) || length(text) != 1) {                              # Guard: heading expects a single string.
    stop("heading(): 'text' must be a single character string.", call. = FALSE)
  }
  cat("\n")                                                                    # Blank line before header improves readability.
  cat(paste0(strrep(char, width), "\n"))                                       # Top rule (e.g., "==============================").
  cat(text, "\n", sep = "")                                                    # The actual heading text on its own line.
  cat(paste0(strrep(char, width), "\n\n"))                                     # Bottom rule + trailing blank line.
  invisible(NULL)                                                              # Explicit invisible return (console printer).
}

# ─────────────────────────────────────────────────────────────────────────────
# rule(width = 30, char = "-")
# Purpose : Print a horizontal rule (useful to separate sections).
# Inputs  : width (characters), char (the character to repeat)
# Returns : (invisible) NULL after printing to console.
# ─────────────────────────────────────────────────────────────────────────────
rule <- function(width = 30, char = "-") {
  cat(paste0(strrep(char, width), "\n"))                                       # Print the repeated character followed by newline.
  invisible(NULL)                                                              # Console helper; return value is not used.
}

# ─────────────────────────────────────────────────────────────────────────────
# Optional column helpers for aligned tables in plain console (ASCII only)
# These are NOT required by the spec but help produce neat interest tables.
# The UI may keep using sprintf() directly; these are available if you prefer.
# ─────────────────────────────────────────────────────────────────────────────

# pad_left(s, width)
# Purpose : Left-pad string s with spaces up to 'width' (right-align numbers).
pad_left <- function(s, width) {
  s <- as.character(s)                                                         # Ensure 's' is character for padding operations.
  n <- nchar(s, type = "width")                                                # Visual width in monospace console (ASCII-safe).
  p <- pmax(0, width - n)                                                      # Number of spaces to add (non-negative).
  paste0(strrep(" ", p), s)                                                    # Prepend spaces to right-align within the column.
}

# pad_right(s, width)
# Purpose : Right-pad string s with spaces up to 'width' (left-align text).
pad_right <- function(s, width) {
  s <- as.character(s)                                                         # Coerce to character to avoid recycling issues.
  n <- nchar(s, type = "width")                                                # Compute display width.
  p <- pmax(0, width - n)                                                      # Spaces to append to reach desired width.
  paste0(s, strrep(" ", p))                                                    # Append spaces to left-align within the column.
}

# fmt_num(x, width = 10, digits = 2, na = "NA")
# Purpose : Right-align a numeric column at fixed decimals (uses money()).
fmt_num <- function(x, width = 10, digits = 2, na = "NA") {
  pad_left(money(x, digits = digits, na = na), width)                          # Format as money → pad left to align numerically.
}

# fmt_text(s, width = 10, align = c("left", "right", "center"))
# Purpose : Align plain text within a fixed-width column.
fmt_text <- function(s, width = 10, align = c("left", "right", "center")) {
  align <- match.arg(align)                                                    # Accept only one of the three supported alignments.
  s <- as.character(s)                                                         # Coerce to character vector.
  if (align == "left")   return(pad_right(s, width))                           # Left alignment: pad on the right.
  if (align == "right")  return(pad_left(s,  width))                           # Right alignment: pad on the left.
  # Center alignment: split padding on both sides (favor left when odd).
  n <- nchar(s, type = "width")                                                # Compute current width of strings.
  total_pad <- pmax(0, width - n)                                              # Total padding required for each element.
  left_pad  <- floor(total_pad / 2)                                            # Left side spaces.
  right_pad <- total_pad - left_pad                                            # Right side spaces (complements left).
  paste0(strrep(" ", left_pad), s, strrep(" ", right_pad))                     # Surround s with spaces to center-align.
}
#### ===== End: R/formatters.R =====

#### ===== Begin: R/ui_screens.R =====
# R/ui_screens.R                                                            
# Purpose:                                                                   
# - Prompt user input with exact labels per spec
# - Call domain functions (account, rates, interest, conversion)
# - Print friendly results (2-decimal money via formatters.R)

# Bring in helpers and domain logic (idempotent if already sourced upstream)

# Canonical currency labels (per spec screenshots) keyed by code for reuse.
.currency_labels <- c(
  PHP = "Philippine Peso (PHP)",
  USD = "United States Dollar (USD)",
  JPY = "Japanese Yen (JPY)",
  GBP = "British Pound Sterling (GBP)",
  EUR = "Euro (EUR)",
  CNY = "Chinese Yuan Renminni (CNY)"
)

.print_currency_menu <- function(title = NULL) {                               # Shared printer for numbered currency menus.
  if (!is.null(title)) cat(title, "\n", sep = "")                            # Optional section label (e.g., Source/Exchange).
  idx <- seq_along(.currency_labels)                                           # Numeric indices 1..6.
  for (i in idx) {                                                             # Print one line per currency exactly as spec.
    cat(sprintf("[%d] %s\n", i, .currency_labels[i]))
  }
}

.coerce_currency_selection <- function(input) {                                # Accept bracketed numbers or codes.
  raw <- trimws(input)                                                         # Remove stray whitespace around the input.
  raw <- gsub("[\t\r\n]", "", raw, perl = TRUE)                            # Strip control characters defensively.
  raw <- gsub("[", "", raw, fixed = TRUE)                                   # Allow entries like "[2]" from the spec transcript.
  raw <- gsub("]", "", raw, fixed = TRUE)                                   # Remove closing bracket separately.
  if (!nzchar(raw)) stop("Currency selection cannot be blank.", call. = FALSE)

  # Try numeric index first (per spec prompt using option numbers).
  idx <- suppressWarnings(as.integer(raw))
  if (!is.na(idx) && idx >= 1 && idx <= length(.currency_labels)) {
    return(names(.currency_labels)[idx])
  }

  # Fallback: treat as currency code and normalize via ensure_currency().
  return(ensure_currency(raw))
}

.account_label <- function(state) {                                            # Friendly display for account holder name.
  if (is.null(state$account$name)) "(not registered)" else state$account$name
}

# ────────────────────────────────────────────────────────────────────────────
# Small local I/O helpers (parse/validate console input at the UI edge)
# ────────────────────────────────────────────────────────────────────────────

.read_line <- function(prompt) {                                             # Read a single line of raw text from stdin.
  ans <- prompted_read(prompt)
  if (is.na(ans)) "" else ans
}

.read_number <- function(prompt) {                                           # Read a numeric (double) from stdin with parse check.
  txt <- .read_line(prompt)                                                  # Get raw text from user.
  if (!length(txt) || identical(txt, ""))                                   # Detect EOF / no data conditions.
    stop("No input received (EOF).", call. = FALSE)
  val <- suppressWarnings(as.numeric(txt))                                   # Attempt to parse as numeric; suppress NA warnings.
  if (is.na(val)) stop("Please enter a valid number.", call. = FALSE)        # Fail fast if not a number (let main tryCatch handle).
  val                                                                         # Return the parsed double.
}

.read_integer <- function(prompt) {                                          # Read an integer from stdin with a strict check.
  txt <- .read_line(prompt)                                                  # Get raw text from user.
  if (!length(txt) || identical(txt, ""))                                   # Detect EOF / no data conditions.
    stop("No input received (EOF).", call. = FALSE)
  val <- suppressWarnings(as.integer(txt))                                   # Attempt integer coercion (drops decimals).
  if (is.na(val) || as.character(val) != trimws(txt))                        # Reject non-integers like "2.5" or non-digits.
    stop("Please enter a valid integer.", call. = FALSE)                     # Clear, actionable error message.
  val                                                                         # Return the parsed integer.
}

# ────────────────────────────────────────────────────────────────────────────
# Screen: Register Account Name
# Spec text: heading "Register Account Name" then prompt "Account Name: "
# ────────────────────────────────────────────────────────────────────────────
screen_register <- function(state) {
  cat("\nRegister Account Name\n")                                           # Exact heading per spec screenshot.
  name <- .read_line("Account Name: ")                                       # Exact prompt per spec.
  state <- register_account(state, name)                                     # Delegate validation & mutation to domain logic.
  cat("Registered: ", state$account$name, "\n", sep = "")                    # Confirm stored (trimmed) name to the user.
  state                                                                      # Return updated state to the router.
}

# ────────────────────────────────────────────────────────────────────────────
# Screen: Record Exchange Rate
# Spec text: show currency choices; disallow changing PHP (base=1.00)
# Prompts: "Select Foreign Currency:" then "Exchange Rate:" (per PDF transcript)
# ────────────────────────────────────────────────────────────────────────────
screen_record_rates <- function(state) {
  cat("\nRecord Exchange Rate\n")                                            # Heading exactly as shown in the PDF.
  .print_currency_menu()                                                     # Display numbered currency list.
  choice <- .read_line("Select Foreign Currency: ")                          # Spec prompt (expects number or code).
  code <- .coerce_currency_selection(choice)                                 # Allow numeric selection or code entry.
  if (code == "PHP") {                                                       # PHP base is immutable by spec.
    cat("PHP is fixed at 1.00.\n")                                           # Inform user; no state change.
    return(state)                                                            # Early exit: nothing to set.
  }
  rate <- .read_number("Exchange Rate: ")                                    # Prompt label per spec screenshot.
  state <- set_rate(state, code, rate)                                       # Delegate validation & write to domain logic.
  cat("Saved: 1 ", code, " = ", money(rate), " PHP\n", sep = "")             # Confirm persistence with 2-decimal echo.
  state                                                                      # Return updated state.
}

# ────────────────────────────────────────────────────────────────────────────
# Screen: Currency Exchange (no state mutation; pure computation & print)
# Prompts: "Source Currency:", "Target Currency:", "Amount:"
# Output : "Exchange Amount: <amount> <DST>"
# ────────────────────────────────────────────────────────────────────────────
screen_currency_exchange <- function(state) {
  cat("\nForeign Currency Exchange\n")                                       # Heading per spec output page.
  .print_currency_menu("Source Currency Option:")                            # List options before selecting source.
  src <- .coerce_currency_selection(.read_line("Source Currency: "))         # Accept numeric or code input.
  amt <- .read_number("Source Amount: ")                                     # Amount in source currency.
  .print_currency_menu("Exchanged Currency Options:")                        # List options before selecting destination.
  dst <- .coerce_currency_selection(.read_line("Exchange Currency: "))       # Accept numeric or code input.
  out <- convert_amount(state, amt, src, dst)                                # Apply spec conversion formula.
  cat("Exchange Amount: ", money(out), "\n", sep = "")                      # Display converted amount (2 decimals via money).
  state                                                                      # State unchanged but returned for API uniformity.
}

# ────────────────────────────────────────────────────────────────────────────
# Screen: Deposit Amount
# Prompts: "Currency:", "Deposit Amount:" with account/balance context lines
# Output : updated balance in PHP (2 decimals)
# ────────────────────────────────────────────────────────────────────────────
screen_deposit <- function(state) {
  cat("\nDeposit Amount\n")                                                  # Heading matches PDF output.
  cat("Account Name: ", .account_label(state), "\n", sep = "")               # Echo current account holder (if any).
  cat("Current Balance: ", money(get_balance_php(state)), "\n", sep = "")     # Show balance before deposit (PHP).
  cur <- .coerce_currency_selection(.read_line("Currency: "))                # Accept numeric menu choice or code.
  amt <- .read_number("Deposit Amount: ")                                    # Amount to deposit.
  state <- deposit(state, amt, cur)                                          # Domain logic: handles conversion & validation.
  cat("Updated Balance: ", money(get_balance_php(state)), " PHP\n", sep = "")# Show canonical balance after operation.
  state                                                                      # Return updated state.
}

# ────────────────────────────────────────────────────────────────────────────
# Screen: Withdraw Amount
# Prompts: "Currency:", "Withdraw Amount:" with account/balance context lines
# Output : updated balance in PHP (2 decimals); errors on overdraft
# ────────────────────────────────────────────────────────────────────────────
screen_withdraw <- function(state) {
  cat("\nWithdraw Amount\n")                                                 # Heading matches PDF output.
  cat("Account Name: ", .account_label(state), "\n", sep = "")               # Echo current account holder (if any).
  cat("Current Balance: ", money(get_balance_php(state)), "\n", sep = "")     # Show balance before withdrawal (PHP).
  cur <- .coerce_currency_selection(.read_line("Currency: "))               # Accept numeric menu choice or code.
  amt <- .read_number("Withdraw Amount: ")                                   # Amount to withdraw.
  state <- withdraw(state, amt, cur)                                         # Domain logic: FX conversion + overdraft guard.
  cat("Updated Balance: ", money(get_balance_php(state)), " PHP\n", sep = "")# Confirm new canonical balance.
  state                                                                      # Return updated state.
}

# ────────────────────────────────────────────────────────────────────────────
# Screen: Show Interest Amount
# Prompt : "Total Number of Days:"
# Output : ASCII table "Day | Interest | Balance" with 2-decimal formatting
# ────────────────────────────────────────────────────────────────────────────
screen_show_interest <- function(state) {
  cat("\nShow Interest Computation\n")                                       # Section header per spec.
  cat("Account Name: ", .account_label(state), "\n", sep = "")               # Provide context consistent with PDF output.
  cat("Current Balance: ", money(get_balance_php(state)), "\n", sep = "")     # Show current balance before simulation.
  cat("Currency: PHP\n")                                                     # Balance is always maintained in PHP.
  cat("Interest Rate: ", money(state$annual_rate * 100, digits = 0), "%\n", sep = "") # Display 5% rate per spec.
  days <- .read_integer("Total Number of Days: ")                            # Integer day count (≥1 enforced downstream).
  cat("Total Number of Days: ", days, " days\n", sep = "")                   # Echo input like the PDF transcript.
  tbl <- simulate_interest(                                                  # Generate compounded table from current balance.
    start_balance_php = get_balance_php(state),                              # Use canonical PHP balance as the principal.
    days = days,                                                             # Horizon provided by the user.
    annual_rate = state$annual_rate                                          # Global annual rate from state.
  )

  # Print table exactly as the PDF layout (pipes separating columns).
  cat("Day | Interest | Balance |\n")
  for (i in seq_len(nrow(tbl))) {
    cat(
      sprintf(
        "%d | %s | %s |\n",
        tbl$day[i],
        money(tbl$interest[i]),
        money(tbl$balance[i])
      )
    )
  }
  state                                                                      # State unchanged; function returns state for uniform API.
}
#### ===== End: R/ui_screens.R =====

#### ===== Begin: app/main.R =====

# app/main.R                                                                    # File: entry point / router (UI navigation only).
# Responsibilities:                                                             # Keep business logic in R/*.R modules.
# - Print the exact Main Menu per spec.                                         # Consistency matters for screenshots/rubric.
# - Dispatch to thin UI screens (R/ui_screens.R).                               # Separation of concerns → simpler tests.
# - Catch and report errors without crashing.                                  

# ──────────────────────────────────────────────────────────────────────────────
# Load dependencies (explicit, readable order)
# ──────────────────────────────────────────────────────────────────────────────

# ──────────────────────────────────────────────────────────────────────────────
# Helpers (router concerns only—no business logic here)
# ──────────────────────────────────────────────────────────────────────────────

print_main_menu <- function() {                                                 # Prints the menu exactly as the spec shows.
  cat("\nMain Menu\n")                                                        # Title line.
  cat("Select Transaction:\n")                                                 # Subtitle per PDF output.
  cat("[1] Register Account Name\n")
  cat("[2] Deposit Amount\n")
  cat("[3] Withdraw Amount\n")
  cat("[4] Currency Exchange\n")
  cat("[5] Record Exchange Rates\n")
  cat("[6] Show Interest Computation\n")
  cat("[0] Exit\n")
}

prompted_read <- function(prompt) {
  # Always show the prompt before waiting
  cat(prompt); flush.console()

  # If the caller wants to force stdin (for piped tests/CI), honor it.
  force_stdin <- identical(Sys.getenv("RS_USE_STDIN", "0"), "1")

  # Order of sources: on Windows prefer CON (interactive keyboard), else stdin.
  sources <- if (.Platform$OS.type == "windows" && !force_stdin)
               c("CON", "stdin")
             else
               c("stdin", "CON")

  for (src in sources) {
    # Some devices may fail to open; try in order
    con <- try(file(src, open = "r"), silent = TRUE)
    if (inherits(con, "try-error")) next
    on.exit(try(close(con), silent = TRUE), add = TRUE)
    ln <- readLines(con, n = 1, warn = FALSE)
    if (length(ln) > 0) return(trimws(ln))
  }

  # If both sources failed or EOF, report NA so callers can decide to exit
  NA_character_
}

read_choice <- function() {                                                     # Reads one line from stdin for the menu pick.
  prompted_read("Select option: ")                                              # Delegate to helper for both modes.
}

ask_back_to_main_menu <- function() {                                           # Asks whether to loop back to the menu.
  ans <- prompted_read("\nBack to the Main Menu (Y/N): ")                      # Exact wording per spec prompt.
  if (is.na(ans)) return(NA)                                                    # Propagate NA when no input is available.
  toupper(ans) == "Y"                                                           # Normalize to uppercase and compare.
}

safe_call <- function(screen_fn, state) {                                       # Wrapper: run a screen and trap errors.
  tryCatch(                                                                     # Prevents the whole app from crashing on bad input.
    screen_fn(state),                                                           # Execute the screen (returns updated state).
    error = function(e) {                                                       # On any error…
      cat("Error: ", conditionMessage(e), "\n", sep = "")                       # …print a concise, user-friendly message,
      state                                                                     # …and keep the previous state unchanged.
    }
  )
}

# ──────────────────────────────────────────────────────────────────────────────
# Main router
# ──────────────────────────────────────────────────────────────────────────────
main_menu <- function() {                                                       # Public entry point for the CLI app.
  state <- new_state()                                                          # Start from a clean state (PHP=1.00, others NA).

  repeat {                                                                      # Event loop until the user chooses to exit.
    print_main_menu()                                                           # Show the menu.
    choice <- read_choice()                                                     # Read the user's selection as a string.
    if (is.na(choice)) {                                                        # Handle non-interactive execution without input.
      cat("\nNo interactive input detected; exiting.\n")
      break                                                                     # Exit loop gracefully.
    }

    if (choice == "0") {                                                        # Spec: '0' means Exit immediately.
      cat("Goodbye!\n")                                                         # Friendly goodbye message.
      break                                                                     # Break the loop to end the program.
    }

    # Dispatch to the appropriate screen. Each screen returns an updated state.
    if      (choice == "1") state <- safe_call(screen_register,         state)  # 1) Register Account Name
    else if (choice == "2") state <- safe_call(screen_deposit,           state) # 2) Deposit Amount
    else if (choice == "3") state <- safe_call(screen_withdraw,          state) # 3) Withdraw Amount
    else if (choice == "4") state <- safe_call(screen_currency_exchange, state) # 4) Currency Exchange
    else if (choice == "5") state <- safe_call(screen_record_rates,      state) # 5) Record Exchange Rates
    else if (choice == "6") state <- safe_call(screen_show_interest,     state) # 6) Show Interest Amount
    else                          cat("Invalid choice.\n")                       # Any other input → helpful message.

    back_to_menu <- ask_back_to_main_menu()                                     # After a screen, ask whether to return to menu.
    if (is.na(back_to_menu)) {                                                  # Handle batch execution without further input.
      cat("\nNo interactive input detected; exiting.\n")
      break
    }

    if (!back_to_menu) {                                                        # After a screen, ask whether to return to menu.
      cat("Goodbye!\n")                                                         # If not, exit gracefully.
      break                                                                     # Leave the loop.
    }
  }
}

#### ===== End: app/main.R =====

# ──────────────────────────────────────────────────────────────────────────────
# Auto-run when executed via `Rscript app/main.R`
# (When sourced from tests or RStudio, nothing runs automatically.)
# ──────────────────────────────────────────────────────────────────────────────
if (sys.nframe() == 0) {                                                        # True when this file is the top-level script.
  main_menu()                                                                   # Start the app automatically.
}
