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
  if (interactive()) {                                                       # readline() keeps cursor position in consoles.
    return(readline(prompt = prompt))
  }

  cat(prompt)                                                                # Show the prompt without newline suppression.
  flush.console()                                                            # Ensure prompt appears before waiting.
  ln <- tryCatch(                                                            # Safely attempt to read one line.
    readLines(stdin(), 1, warn = FALSE),
    error = function(e) character(0)
  )
  if (length(ln) == 0) "" else ln                                            # Gracefully handle EOF in batch mode.
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
