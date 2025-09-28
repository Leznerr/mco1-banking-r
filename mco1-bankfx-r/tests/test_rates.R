# tests/test_rates.R
# Acceptance & property-style tests for R/rates.R (exchange rates + conversion).
# Goal: lock correctness early (happy paths + guard rails) before building account/interest logic.

# ─────────────────────────────────────────────────────────────────────────────
# Load modules under test
# ─────────────────────────────────────────────────────────────────────────────
source("R/state.R")          # brings in new_state() and currency constants
source("R/validation.R")     # brings in assert_*() and ensure_currency()
source("R/rates.R")          # brings in set_rate(), get_rate(), convert_amount()

# ─────────────────────────────────────────────────────────────────────────────
# Tiny test helpers (base R only; no external packages)
# ─────────────────────────────────────────────────────────────────────────────
expect_error <- function(expr) {                          # Asserts that evaluating 'expr' throws an error
  ok <- tryCatch({ force(expr); FALSE }, error = function(e) TRUE)
  stopifnot(ok)                                           # Fails the test if no error occurred
}

expect_near <- function(x, y, tol = 1e-6) {               # Asserts |x - y| ≤ tol (floating-point tolerant equality)
  stopifnot(is.finite(x), is.finite(y), abs(x - y) <= tol)
}

# ─────────────────────────────────────────────────────────────────────────────
# Fresh state (canonical base: PHP=1.00, others unset)
# ─────────────────────────────────────────────────────────────────────────────
s <- new_state()                                          # Start from a clean state each time
check_state(s)                                            # Optional invariant check (silent when all good)

# ─────────────────────────────────────────────────────────────────────────────
# Happy path: set a rate and convert per spec
# ─────────────────────────────────────────────────────────────────────────────
s <- set_rate(s, "usd", 52.00)                            # Lowercase input; ensure_currency() normalizes to "USD"
stopifnot(get_rate(s, "USD") == 52.00)                    # Verify that the rate was recorded correctly

# Spec example: 1000 USD → PHP should be 52000 when USD=52.00 (amount * (rs / rd))
expect_near(convert_amount(s, 1000, "USD", "PHP"), 52000, tol = 1e-6)

# Identity conversions (when a rate is set): X → X returns the same amount
expect_near(convert_amount(s, 777, "PHP", "PHP"), 777, tol = 1e-12)
expect_near(convert_amount(s, 250, "USD", "USD"), 250, tol = 1e-12)

# ─────────────────────────────────────────────────────────────────────────────
# Multi-currency setup: record several rates (PHP per 1 FX unit)
# ─────────────────────────────────────────────────────────────────────────────
s <- set_rate(s, "JPY", 0.37)                             # 1 JPY = 0.37 PHP (example)
s <- set_rate(s, "GBP", 67.00)                            # 1 GBP = 67.00 PHP
s <- set_rate(s, "EUR", 59.00)                            # 1 EUR = 59.00 PHP
s <- set_rate(s, "CNY", 7.30)                             # 1 CNY = 7.30 PHP

# Direct formula checks against the state’s stored rates:
# USD → JPY = amount * (rate["USD"] / rate["JPY"])
expect_near(
  convert_amount(s, 100, "USD", "JPY"),
  100 * (get_rate(s, "USD") / get_rate(s, "JPY")),
  tol = 1e-6
)

# EUR → GBP
expect_near(
  convert_amount(s, 42, "EUR", "GBP"),
  42 * (get_rate(s, "EUR") / get_rate(s, "GBP")),
  tol = 1e-6
)

# JPY → CNY
expect_near(
  convert_amount(s, 5000, "JPY", "CNY"),
  5000 * (get_rate(s, "JPY") / get_rate(s, "CNY")),
  tol = 1e-6
)

# ─────────────────────────────────────────────────────────────────────────────
# 3) Round-trip property: src → dst → src returns the original (within tol)
# ─────────────────────────────────────────────────────────────────────────────
round_trip <- function(amount, a, b, tol = 1e-9) {        # Helper: convert to 'b' and back, then compare
  x <- convert_amount(s, amount, a, b)
  y <- convert_amount(s, x,      b, a)
  expect_near(y, amount, tol = tol)
}

round_trip(12345, "PHP", "USD")                           # PHP → USD → PHP
round_trip(888.88, "GBP", "EUR")                          # GBP → EUR → GBP
round_trip(1e6,   "CNY", "JPY")                           # Large amount round-trip
round_trip(0.01,  "EUR", "USD")                           # Small amount round-trip

# ─────────────────────────────────────────────────────────────────────────────
# 4) Guard rails: enforce immutability, positivity, and validity
# ─────────────────────────────────────────────────────────────────────────────
expect_error(set_rate(s, "PHP", 1.23))                    # PHP base rate is immutable by spec
expect_error(set_rate(s, "USD", -5))                      # Negative rate not allowed
expect_error(set_rate(s, "USD", 0))                       # Zero rate not allowed
expect_error(set_rate(s, "USD", NA_real_))                # NA rate not allowed
expect_error(set_rate(s, "AAA", 99))                      # Invalid currency code rejected

expect_error(convert_amount(s, 0,        "USD", "PHP"))   # Amount must be > 0
expect_error(convert_amount(s, -1,       "USD", "PHP"))   # Negative amount not allowed
expect_error(convert_amount(s, NA_real_, "USD", "PHP"))   # NA amount not allowed

# Unset-rate behavior: create a fresh state with only USD set, then attempt USD→EUR
s2 <- new_state()                                         # Fresh state: all but PHP unset
s2 <- set_rate(s2, "USD", 52.00)                          # Only USD is set in this fresh state
expect_error(convert_amount(s2, 10, "USD", "EUR"))        # EUR unset → must error with guidance
expect_error(convert_amount(s2, 10, "EUR", "PHP"))        # EUR unset even as source → must error

# ─────────────────────────────────────────────────────────────────────────────
# 5) Randomized sanity checks (deterministic via set.seed)
#    We verify formula consistency across random amounts.
# ─────────────────────────────────────────────────────────────────────────────
set.seed(2025)                                            # Deterministic sequence for reproducible tests
amounts <- c(runif(5, 0.01, 1e6))                         # Random positive amounts in a wide range
pairs <- list(                                            # Some diverse currency pairs to exercise code paths
  c("USD","PHP"), c("EUR","GBP"), c("JPY","USD"),
  c("CNY","EUR"), c("GBP","JPY")
)

for (amt in amounts) {                                    # For each random amount…
  for (p in pairs) {                                      # …and each src/dst pair…
    src <- p[1]; dst <- p[2]
    # Expected per spec: amt * (rate[src] / rate[dst])
    expected <- amt * (get_rate(s, src) / get_rate(s, dst))
    actual   <- convert_amount(s, amt, src, dst)
    expect_near(actual, expected, tol = 1e-6)             # Must match within tolerance
  }
}

# ─────────────────────────────────────────────────────────────────────────────
# 6) Edge values: very large/small magnitudes should remain finite
# ─────────────────────────────────────────────────────────────────────────────
big_amt   <- 1e12                                          # Large amount
small_amt <- 1e-6                                          # Very small amount
stopifnot(is.finite(convert_amount(s, big_amt,   "USD", "EUR")))
stopifnot(is.finite(convert_amount(s, small_amt, "EUR", "USD")))

# ─────────────────────────────────────────────────────────────────────────────
# Done
# ─────────────────────────────────────────────────────────────────────────────
cat("✅ rates tests passed\n")                             # Success banner when all assertions hold
