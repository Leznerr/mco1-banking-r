# tests/test_account.R
# Acceptance tests for account registration and money movements (deposit/withdraw).
# Purpose: lock in correctness for account flows before wiring UI and interest.

# ─────────────────────────────────────────────────────────────────────────────
# Load modules under test (state, validators, rates, account)
# ─────────────────────────────────────────────────────────────────────────────
source("R/state.R")          # Provides new_state(), check_state(), constants
source("R/validation.R")     # Provides assert_*() and ensure_currency()
source("R/rates.R")          # Provides set_rate(), get_rate(), convert_amount()
source("R/account.R")        # Provides register_account(), deposit(), withdraw(), get_balance_php()

# ─────────────────────────────────────────────────────────────────────────────
# Lightweight test helpers (base R only)
# ─────────────────────────────────────────────────────────────────────────────
expect_error <- function(expr) {                          # Assert that evaluating 'expr' throws an error
  ok <- tryCatch({ force(expr); FALSE }, error = function(e) TRUE)  # TRUE if error was thrown, else FALSE
  stopifnot(ok)                                           # Fail the test when no error occurs
}

expect_near <- function(x, y, tol = 1e-6) {               # Floating-point tolerant equality
  stopifnot(is.finite(x), is.finite(y), abs(x - y) <= tol) # Require finite values and |x−y| ≤ tol
}

# ─────────────────────────────────────────────────────────────────────────────
# Fresh canonical state
# ─────────────────────────────────────────────────────────────────────────────
s <- new_state()                                          # Start from a clean state (PHP=1.00; other rates unset)
check_state(s)                                            # Optional invariant check (silent when all constraints hold)

# ─────────────────────────────────────────────────────────────────────────────
# Registration behavior: name set, balance reset to 0.0
# ─────────────────────────────────────────────────────────────────────────────
s$account$balance_php <- 123.45                           # Pre-set a nonzero balance to verify "reset on register"
s <- register_account(s, "Renzel")                        # Register with a valid non-blank name
stopifnot(identical(s$account$name, "Renzel"))            # Name stored exactly as trimmed
expect_near(get_balance_php(s), 0.0)                      # Registration initializes balance to zero

# Invalid names should be rejected (validation of integrity/UX)
expect_error(register_account(s, ""))                     # Empty string is not allowed
expect_error(register_account(s, "   "))                  # Blank after trim is not allowed
expect_error(register_account(s, NA_character_))          # NA name is not allowed

# ─────────────────────────────────────────────────────────────────────────────
# Deposit in PHP (canonical currency)
# ─────────────────────────────────────────────────────────────────────────────
s <- deposit(s, 500, "PHP")                               # Deposit a positive PHP amount
expect_near(get_balance_php(s), 500)                      # Balance increases exactly by the deposit amount

# Guards for deposit amount and currency validity
expect_error(deposit(s, 0,  "PHP"))                       # Zero amount is invalid
expect_error(deposit(s, -1, "PHP"))                       # Negative amount is invalid
expect_error(deposit(s, 10, "AAA"))                       # Invalid currency code is rejected

# ─────────────────────────────────────────────────────────────────────────────
# Deposit in FX (requires rate); verify conversion path and correctness
# ─────────────────────────────────────────────────────────────────────────────
expect_error(deposit(s, 100, "EUR"))                      # EUR rate unset -> must error with guidance
s <- set_rate(s, "USD", 52.00)                            # Record USD rate (PHP per 1 USD)
prev <- get_balance_php(s)                                # Save balance before FX deposit
s <- deposit(s, 100, "USD")                               # Deposit 100 USD -> +5200 PHP (via conversion)
expect_near(get_balance_php(s), prev + 5200)              # Balance reflects conversion to PHP

# ─────────────────────────────────────────────────────────────────────────────
# Withdraw in PHP; then in FX; enforce overdraft guard
# ─────────────────────────────────────────────────────────────────────────────
s <- withdraw(s, 700, "PHP")                              # Withdraw a PHP amount
expect_near(get_balance_php(s), prev + 5200 - 700)        # Balance decreases accordingly

s <- set_rate(s, "EUR", 59.00)                            # Record EUR rate for FX withdrawal
prev <- get_balance_php(s)                                # Save balance before FX withdrawal
s <- withdraw(s, 1, "EUR")                                # Withdraw 1 EUR -> −59.00 PHP
expect_near(get_balance_php(s), prev - 59.00)             # Balance reflects EUR→PHP conversion

bal <- get_balance_php(s)                                 # Current balance snapshot
expect_error(withdraw(s, bal + 0.01, "PHP"))              # Overdraft: withdrawing more than balance must error

# Guards for withdraw amount and currency validity
expect_error(withdraw(s, 0,  "PHP"))                      # Zero amount invalid
expect_error(withdraw(s, -5, "PHP"))                      # Negative amount invalid
expect_error(withdraw(s, 10, "AAA"))                      # Invalid currency code rejected

# ─────────────────────────────────────────────────────────────────────────────
# Round-trip property: deposit X in FX and withdraw X in same FX → net zero
# ─────────────────────────────────────────────────────────────────────────────
prev <- get_balance_php(s)                                # Baseline balance
s <- deposit(s, 200, "USD")                               # + (200 * 52) PHP
s <- withdraw(s, 200, "USD")                              # − (200 * 52) PHP
expect_near(get_balance_php(s), prev, tol = 1e-9)         # Net effect is ~0 within tight tolerance

# ─────────────────────────────────────────────────────────────────────────────
# Associativity sanity: deposit(a) + deposit(b) == deposit(a+b) in PHP
# ─────────────────────────────────────────────────────────────────────────────
s <- register_account(s, "Renzel")                        # Reset account to zero (and keep name)
s <- set_rate(s, "USD", 52.00)                            # Ensure USD rate exists for any later FX ops

s1 <- s                                                   # Branch A for sequential deposits
s1 <- deposit(s1, 100, "PHP")                             # First PHP deposit
s1 <- deposit(s1, 50,  "PHP")                             # Second PHP deposit
d1 <- get_balance_php(s1)                                 # Resulting balance

s2 <- s                                                   # Branch B for combined deposit
s2 <- deposit(s2, 150, "PHP")                             # Single combined deposit
d2 <- get_balance_php(s2)                                 # Resulting balance

expect_near(d1, d2)                                       # Associativity holds in canonical currency

# ─────────────────────────────────────────────────────────────────────────────
# Mixed FX deposits vs single combined PHP deposit (commutativity of conversion)
# ─────────────────────────────────────────────────────────────────────────────
s <- register_account(s, "R")                             # Reset to zero again
s <- set_rate(s, "USD", 52.00)                            # Record USD rate
s <- set_rate(s, "JPY", 0.37)                             # Record JPY rate

sA <- s                                                   # Branch A: deposit in FX separately
sA <- deposit(sA, 100,  "USD")                            # + 100 * 52 PHP
sA <- deposit(sA, 1000, "JPY")                            # + 1000 * 0.37 PHP

sB <- s                                                   # Branch B: deposit the combined PHP value once
combined_php <- 100 * 52.00 + 1000 * 0.37                 # Compute expected PHP effect of the two FX deposits
sB <- deposit(sB, combined_php, "PHP")                    # Single PHP deposit with combined value

expect_near(get_balance_php(sA), combined_php)            # Branch A balance equals computed expectation
expect_near(get_balance_php(sA), get_balance_php(sB))     # Branch A and B balances match

# ─────────────────────────────────────────────────────────────────────────────
# Unset-rate behavior for withdrawals (fresh state with only USD set)
# ─────────────────────────────────────────────────────────────────────────────
s3 <- new_state()                                         # Start from scratch
s3 <- register_account(s3, "X")                           # Register name; zero balance
s3 <- set_rate(s3, "USD", 52.00)                          # Only USD rate recorded
expect_error(withdraw(s3, 1, "EUR"))                      # EUR rate unset → FX withdrawal must error
expect_error(deposit(s3,  1, "EUR"))                      # EUR deposit must also error without rate

# ─────────────────────────────────────────────────────────────────────────────
# Done
# ─────────────────────────────────────────────────────────────────────────────
cat("account tests passed\n")                           # Success banner when all assertions hold
