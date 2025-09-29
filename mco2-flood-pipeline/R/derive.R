# Derived metrics and filtering utilities

suppressPackageStartupMessages({  # Suppress library startup output
  library(dplyr)  # Data manipulation verbs
})

source("R/utils_log.R")  # Logging utilities

derive_fields <- function(df) {  # Compute derived analytical columns
  df <- df %>% mutate(  # Add derived fields without dropping columns
    CostSavings = ApprovedBudgetForContract - ContractCost,  # Savings as budget minus cost
    CompletionDelayDays = as.numeric(ActualCompletionDate - StartDate)  # Difference in days allowing negatives
  )
  overruns <- sum(df$CostSavings < 0, na.rm = TRUE)  # Count negative savings as overruns
  na_delays <- sum(is.na(df$CompletionDelayDays))  # Count missing delay calculations
  log_info("derivations complete overruns=%d na_delays=%d", overruns, na_delays)  # Emit derivation summary log
  df  # Return augmented data frame
}

filter_years <- function(df, years = 2021:2023) {  # Keep only rows whose FundingYear is within years
  filtered <- df %>% filter(!is.na(FundingYear) & FundingYear %in% years)  # Drop rows outside allowed years
  dropped <- nrow(df) - nrow(filtered)  # Compute number of rows removed
  log_info("year filter applied dropped=%d", dropped)  # Log filter impact
  filtered  # Return filtered data frame
}
