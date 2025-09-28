# tests/test_interest.R
# Acceptance & property-style tests for R/interest.R (daily interest + N-day compounding).
# Objective: verify numerical correctness, vectorized compounding behavior, input validation,
#            and table shape (Day | Interest | Balance) without any UI rounding effects.

# ─────────────────────────────────────────────────────────────────────────────
# Load modules under test
# ─────────────────────────────────────────────────────────────────────────────
source("R/validation.R")       # Brings assert_days() and shared validation semantics
source("R/interest.R")         # Brings daily_interest() and simulate_interest()

# ─────────────────────────────────────────────────────────────────────────────
# Minimal test helpers (base R only)
# ─────────────────────────────────────────────────────────────────────────────
expect_error <- function(expr) {                               # Helper: asserts that evaluating 'expr' throws an error
  ok <- tryCatch({ force(expr); FALSE }, error = function(e) TRUE)  # ok=TRUE iff an error was thrown
  stopifnot(ok)                                                # Fail the test if no error occurred
}

expect_near <- function(x, y, tol = 1e-8) {                    # Helper: floating-point tolerant equality
  stopifnot(is.finite(x), is.finite(y), abs(x - y) <= tol)     # Require both finite and within tolerance
}

# ─────────────────────────────────────────────────────────────────────────────
# 1) daily_interest(): scalar correctness and validation
# ─────────────────────────────────────────────────────────────────────────────
bal <- 1000.00                                                 # Canonical test balance in PHP
r   <- 0.05                                                    # Annual interest proportion (5%)
exp_daily <- bal * r / 365                                     # Expected daily interest per spec formula

i1 <- daily_interest(bal, r)                                   # Compute daily interest using the function under test
expect_near(i1, exp_daily, tol = 1e-12)                        # Must match exact formula to tight tolerance

i0 <- daily_interest(0, r)                                     # Zero balance should earn exactly zero interest
expect_near(i0, 0.0, tol = 0.0)                                # No tolerance needed for exact zero

# Validation: negative balance must be rejected
expect_error(daily_interest(-1, r))                            # Guard: balance must be non-negative

# Validation: annual rate must be a proper proportion in (0,1)
expect_error(daily_interest(bal, 0))                           # r=0 not allowed by our validator
expect_error(daily_interest(bal, 1))                           # r=1 not allowed (upper bound is open)
expect_error(daily_interest(bal, NA_real_))                    # NA rate must be rejected

# ─────────────────────────────────────────────────────────────────────────────
# 2) simulate_interest(): table shape, compounding math, and monotonicity
# ─────────────────────────────────────────────────────────────────────────────
days <- 30                                                     # Horizon for the compounding table
tbl  <- simulate_interest(start_balance_php = bal,             # Generate the N-day interest table
                          days = days,
                          annual_rate = r)

stopifnot(is.data.frame(tbl))                                  # The result must be a data.frame
stopifnot(identical(names(tbl), c("day","interest","balance")))# Column names must be exactly Day|Interest|Balance
stopifnot(nrow(tbl) == days)                                   # There must be 'days' rows

stopifnot(is.integer(tbl$day))                                 # 'day' column is integer-typed
stopifnot(identical(tbl$day, seq_len(days)))                   # 'day' column must be 1..N in order

stopifnot(is.numeric(tbl$interest))                            # 'interest' column numeric (not pre-rounded or character)
stopifnot(is.numeric(tbl$balance))                             # 'balance' column numeric (not pre-rounded or character)

stopifnot(tbl$balance[1] > bal)                                # Day-1 end balance must exceed starting balance when r>0
stopifnot(all(diff(tbl$balance) >= 0))                         # Balances must be non-decreasing across days

# Reconstruct balances and interests via the closed-form vectorized formula
r_daily <- r / 365                                             # Daily proportional rate used by the spec
balances_exp <- bal * (1 + r_daily) ^ (1:days)                 # Expected end-of-day balances for each day
interests_exp <- c(balances_exp[1] - bal, diff(balances_exp))  # Expected per-day interest (first increment, then diffs)

# Compare computed vs expected vectors elementwise
stopifnot(length(tbl$balance)  == length(balances_exp))        # Sanity: aligned lengths
stopifnot(length(tbl$interest) == length(interests_exp))       # Sanity: aligned lengths
stopifnot(all(abs(tbl$balance  - balances_exp)  <= 1e-10))     # Balances match within tight numeric tolerance
stopifnot(all(abs(tbl$interest - interests_exp) <= 1e-10))     # Interests match within tight numeric tolerance

# Interest additivity property: sum of daily interests equals total balance gain
expect_near(sum(tbl$interest), tail(tbl$balance, 1) - bal, tol = 1e-10)

# ─────────────────────────────────────────────────────────────────────────────
# 3) Edge cases: days=1, start=0, large horizon
# ─────────────────────────────────────────────────────────────────────────────
t1 <- simulate_interest(bal, 1, r)                             # Single day table
stopifnot(nrow(t1) == 1)                                       # Exactly one row
expect_near(t1$interest[1], exp_daily, tol = 1e-12)            # Day-1 interest equals daily_interest()
expect_near(t1$balance[1], bal * (1 + r_daily), 1e-12)         # Day-1 balance matches closed form

t0 <- simulate_interest(0, 10, r)                              # Zero starting balance
stopifnot(all(t0$interest == 0))                               # No interest accrues when starting from zero
stopifnot(all(t0$balance  == 0))                               # Balance remains zero

t365 <- simulate_interest(bal, 365, r)                         # One full year of daily compounding
expect_near(t365$balance[365], bal * (1 + r_daily)^365, 1e-8)  # End balance matches closed form for 365 days

# ─────────────────────────────────────────────────────────────────────────────
# 4) Input validation in simulate_interest(): days and rate
# ─────────────────────────────────────────────────────────────────────────────
expect_error(simulate_interest(bal, 0,   r))                   # days must be >= 1
expect_error(simulate_interest(bal, -5,  r))                   # negative days not allowed
expect_error(simulate_interest(bal, 2.5, r))                   # non-integer days not allowed
expect_error(simulate_interest(bal, 10,  0))                   # rate must be in (0,1)
expect_error(simulate_interest(bal, 10,  1))                   # upper bound open
expect_error(simulate_interest(bal, 10,  NA_real_))            # NA rate not allowed
expect_error(simulate_interest(-1, 10,  r))                    # negative starting balance rejected

# ─────────────────────────────────────────────────────────────────────────────
# 5) Numerical sanity: very small/large amounts remain finite and reasonable
# ─────────────────────────────────────────────────────────────────────────────
tiny  <- simulate_interest(1e-6,   5, r)                       # Very small starting balance
huge  <- simulate_interest(1e12,  10, r)                       # Very large starting balance
stopifnot(all(is.finite(tiny$balance)), all(is.finite(tiny$interest)))   # No NA/Inf in small-scale case
stopifnot(all(is.finite(huge$balance)), all(is.finite(huge$interest)))   # No NA/Inf in large-scale case

# ─────────────────────────────────────────────────────────────────────────────
# Done
# ─────────────────────────────────────────────────────────────────────────────
cat("✅ interest tests passed\n")                               # Success banner when all assertions hold
