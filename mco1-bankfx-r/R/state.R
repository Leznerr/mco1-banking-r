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
