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


