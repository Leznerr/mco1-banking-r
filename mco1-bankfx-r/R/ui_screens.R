# R/ui_screens.R                                                            
# Purpose:                                                                   
# - Prompt user input with exact labels per spec
# - Call domain functions (account, rates, interest, conversion)
# - Print friendly results (2-decimal money via formatters.R)

# Bring in helpers and domain logic (idempotent if already sourced upstream)
source("R/formatters.R")                                                     # money(), heading(), rule(), etc.
source("R/account.R")                                                        # register_account(), deposit(), withdraw(), get_balance_php()
source("R/rates.R")                                                          # set_rate(), get_rate(), convert_amount()
source("R/interest.R")                                                       # daily_interest(), simulate_interest()

# ────────────────────────────────────────────────────────────────────────────
# Small local I/O helpers (parse/validate console input at the UI edge)
# ────────────────────────────────────────────────────────────────────────────

.read_line <- function(prompt) {                                             # Read a single line of raw text from stdin.
  cat(prompt)                                                                # Show the prompt without newline suppression.
  ln <- readLines(stdin(), 1)                                                # Read exactly one line from the console.
  ln                                                                          # Return as character (may be empty string).
}

.read_number <- function(prompt) {                                           # Read a numeric (double) from stdin with parse check.
  txt <- .read_line(prompt)                                                  # Get raw text from user.
  val <- suppressWarnings(as.numeric(txt))                                   # Attempt to parse as numeric; suppress NA warnings.
  if (is.na(val)) stop("Please enter a valid number.", call. = FALSE)        # Fail fast if not a number (let main tryCatch handle).
  val                                                                         # Return the parsed double.
}

.read_integer <- function(prompt) {                                          # Read an integer from stdin with a strict check.
  txt <- .read_line(prompt)                                                  # Get raw text from user.
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
  heading("Register Account Name")                                           # Visual section separator for readability.
  name <- .read_line("Account Name: ")                                       # Exact prompt per spec.
  state <- register_account(state, name)                                     # Delegate validation & mutation to domain logic.
  cat("Registered: ", state$account$name, "\n", sep = "")                    # Confirm stored (trimmed) name to the user.
  state                                                                      # Return updated state to the router.
}

# ────────────────────────────────────────────────────────────────────────────
# Screen: Record Exchange Rate
# Spec text: show currency choices; disallow changing PHP (base=1.00)
# Prompts: "Select currency code:" then "Enter rate (PHP per 1 CODE): "
# ────────────────────────────────────────────────────────────────────────────
screen_record_rates <- function(state) {
  heading("Record Exchange Rate")                                            # Section header matches deliverable screenshot.
  cat("[1] PHP  [2] USD  [3] JPY  [4] GBP  [5] EUR  [6] CNY\n")              # Display the allowed set (informational).
  code <- toupper(.read_line("Select currency code: "))                      # Read code; normalize case for UX.
  if (code == "PHP") {                                                       # PHP base is immutable by spec.
    cat("PHP is fixed at 1.00.\n")                                           # Inform user; no state change.
    return(state)                                                            # Early exit: nothing to set.
  }
  rate <- .read_number(paste0("Enter rate (PHP per 1 ", code, "): "))        # Ask for "PHP per 1 CODE"; parse as numeric.
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
  heading("Currency Exchange")                                               # Header matches spec.
  src <- toupper(.read_line("Source Currency: "))                            # Source code (case-insensitive input).
  dst <- toupper(.read_line("Target Currency: "))                            # Destination code.
  amt <- .read_number("Amount: ")                                            # Amount to convert (double).
  out <- convert_amount(state, amt, src, dst)                                # Use spec formula inside domain logic.
  cat("Exchange Amount: ", money(out), " ", dst, "\n", sep = "")             # Print 2-decimal result with target code.
  state                                                                      # State unchanged but returned for API uniformity.
}

# ────────────────────────────────────────────────────────────────────────────
# Screen: Deposit Amount
# Prompts: "Currency (PHP/USD/JPY/GBP/EUR/CNY):", "Deposit Amount:"
# Output : updated balance in PHP (2 decimals)
# ────────────────────────────────────────────────────────────────────────────
screen_deposit <- function(state) {
  heading("Deposit Amount")                                                  # Section header for screenshot parity.
  cur <- toupper(.read_line("Currency (PHP/USD/JPY/GBP/EUR/CNY): "))         # Currency to deposit in.
  amt <- .read_number("Deposit Amount: ")                                    # Amount to deposit.
  state <- deposit(state, amt, cur)                                          # Domain logic: handles conversion & validation.
  cat("Updated Balance: ", money(get_balance_php(state)), " PHP\n", sep = "")# Show canonical balance after operation.
  state                                                                      # Return updated state.
}

# ────────────────────────────────────────────────────────────────────────────
# Screen: Withdraw Amount
# Prompts: "Currency (PHP/USD/JPY/GBP/EUR/CNY):", "Withdraw Amount:"
# Output : updated balance in PHP (2 decimals); errors on overdraft
# ────────────────────────────────────────────────────────────────────────────
screen_withdraw <- function(state) {
  heading("Withdraw Amount")                                                 # Section header.
  cur <- toupper(.read_line("Currency (PHP/USD/JPY/GBP/EUR/CNY): "))         # Currency to withdraw in.
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
  heading("Show Interest Amount")                                            # Section header per spec.
  days <- .read_integer("Total Number of Days: ")                            # Integer day count (≥1 enforced downstream).
  tbl <- simulate_interest(                                                  # Generate compounded table from current balance.
    start_balance_php = get_balance_php(state),                              # Use canonical PHP balance as the principal.
    days = days,                                                             # Horizon provided by the user.
    annual_rate = state$annual_rate                                          # Global annual rate from state.
  )

  # Print an aligned ASCII table using money() for 2-decimal display.
  cat(sprintf("%-5s | %-10s | %-12s\n", "Day", "Interest", "Balance"))       # Column headers with fixed widths.
  for (i in seq_len(nrow(tbl))) {                                            # Iterate rows to print formatted values.
    cat(sprintf("%-5d | %-10s | %-12s\n",                                   # Fixed-width row with money()-formatted numbers.
                tbl$day[i],
                money(tbl$interest[i]),
                money(tbl$balance[i])))
  }
  state                                                                      # State unchanged; function returns state for uniform API.
}
