# Annual Project Type Cost Overrun Trends report

suppressPackageStartupMessages({  # Silence package startup output
  library(dplyr)  # Data manipulation verbs
})

source("R/utils_format.R")  # Safe stats helpers
source("R/utils_log.R")  # Logging utilities

compute_overrun_rate <- function(values) {  # Helper computing percentage of overruns
  indicator <- values < 0  # Identify cost overruns
  if (all(is.na(indicator))) {  # If all values missing
    return(NA_real_)  # Return NA to denote lack of data
  }
  mean(indicator, na.rm = TRUE) * 100  # Convert to percentage
}

report_overrun_trends <- function(df) {  # Generate annual type overrun trends
  aggregated <- df %>%  # Aggregate metrics per year and type
    group_by(FundingYear, TypeOfWork, .drop = FALSE) %>%  # Group for summarisation
    summarise(
      N = dplyr::n(),  # Count projects per grouping
      AvgSavings = safe_mean(CostSavings),  # Average savings (can be negative)
      OverrunRate = compute_overrun_rate(CostSavings),  # Percentage of overruns
      .groups = "drop"  # Drop grouping
    )
  baseline <- aggregated %>% filter(FundingYear == 2021) %>% select(TypeOfWork, AvgSavings_2021 = AvgSavings)  # Extract 2021 baseline
  report <- aggregated %>%  # Join baseline to compute YoY trends
    left_join(baseline, by = "TypeOfWork") %>%  # Combine baseline data
    mutate(
      YoY_vs_2021 = ifelse(
        FundingYear == 2021 | is.na(AvgSavings_2021) | AvgSavings_2021 == 0,  # Guard baseline edge cases
        NA_real_,  # No YoY comparison for baseline or missing data
        100 * (AvgSavings - AvgSavings_2021) / abs(AvgSavings_2021)  # Percentage change vs 2021
      )
    ) %>%
    select(FundingYear, TypeOfWork, N, AvgSavings, OverrunRate, YoY_vs_2021) %>%  # Ensure column order
    arrange(FundingYear, desc(AvgSavings))  # Sort per spec
  log_info("report3 generated rows=%d", nrow(report))  # Log row count
  report  # Return assembled report
}
