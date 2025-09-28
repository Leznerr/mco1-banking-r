# app/main.R                                                                    # File: entry point / router (UI navigation only).
# Responsibilities:                                                             # Keep business logic in R/*.R modules.
# - Print the exact Main Menu per spec.                                         # Consistency matters for screenshots/rubric.
# - Dispatch to thin UI screens (R/ui_screens.R).                               # Separation of concerns → simpler tests.
# - Catch and report errors without crashing.                                  

# ──────────────────────────────────────────────────────────────────────────────
# Load dependencies (explicit, readable order)
# ──────────────────────────────────────────────────────────────────────────────
source("R/state.R")                                                             # new_state(), constants (currency codes, rate).
source("R/formatters.R")                                                        # money(), heading() used in UI.
source("R/validation.R")                                                        # assert_*(), ensure_currency() for guards.
source("R/rates.R")                                                             # set_rate(), get_rate(), convert_amount().
source("R/account.R")                                                           # register_account(), deposit(), withdraw().
source("R/interest.R")                                                          # daily_interest(), simulate_interest().
source("R/ui_screens.R")                                                        # screen_* functions (thin I/O wrappers).

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

read_choice <- function() {                                                     # Reads one line from stdin for the menu pick.
  cat("Select option: ")                                                        # Prompt text must match the spec.
  trimws(readLines(stdin(), n = 1))                                             # Return trimmed string (e.g., "1", "2", "0").
}

ask_back_to_main_menu <- function() {                                           # Asks whether to loop back to the menu.
  cat("\nBack to the Main Menu (Y/N): ")                                        # Exact wording per spec (note "the").
  ans <- toupper(trimws(readLines(stdin(), n = 1)))                              # Normalize to uppercase.
  ans == "Y"                                                                     # TRUE to loop, FALSE to exit.
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

    if (!ask_back_to_main_menu()) {                                             # After a screen, ask whether to return to menu.
      cat("Goodbye!\n")                                                         # If not, exit gracefully.
      break                                                                     # Leave the loop.
    }
  }
}

# ──────────────────────────────────────────────────────────────────────────────
# Auto-run when executed via `Rscript app/main.R`
# (When sourced from tests or RStudio, nothing runs automatically.)
# ──────────────────────────────────────────────────────────────────────────────
if (sys.nframe() == 0) {                                                        # True when this file is the top-level script.
  main_menu()                                                                   # Start the app automatically.
}
