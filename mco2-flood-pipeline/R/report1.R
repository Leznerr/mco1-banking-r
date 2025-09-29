# Regional Flood Mitigation Efficiency report generation

suppressPackageStartupMessages({  # Suppress package startup chatter
  library(dplyr)  # Grouping and summarisation
})

source("R/utils_format.R")  # Bring in safe stats and normalization
source("R/utils_log.R")  # Logging utilities

compute_delay_rate <- function(delays) {  # Helper to compute rate of delays over 30 days
  indicator <- delays > 30  # Logical vector marking >30 day delays
  if (all(is.na(indicator))) {  # If no valid values
    return(NA_real_)  # Return NA to represent absence of data
  }
  mean(indicator, na.rm = TRUE) * 100  # Convert to percentage rate
}

report_regional_efficiency <- function(df) {  # Produce aggregated efficiency report
  report <- df %>%  # Start summarisation chain
    group_by(Region, MainIsland, .drop = FALSE) %>%  # Group by geographic levels
    summarise(  # Aggregate metrics
      TotalApprovedBudget = sum(ApprovedBudgetForContract, na.rm = TRUE),  # Sum budgets
      MedianSavings = safe_median(CostSavings),  # Median of savings
      AvgDelay = safe_mean(CompletionDelayDays),  # Average delay in days
      Delay30Rate = compute_delay_rate(CompletionDelayDays),  # Percentage of projects delayed over 30 days
      .groups = "drop"  # Ungroup after summarise
    ) %>%
    mutate(  # Derive efficiency score per spec
      EfficiencyScore = minmax_0_100((MedianSavings / pmax(AvgDelay, 0.5)) * 100)  # Clamp computed efficiency to [0, 100]
    ) %>%
    arrange(desc(EfficiencyScore), Region, MainIsland)  # Sort per specification
  log_info("report1 generated rows=%d", nrow(report))  # Log the number of report rows
  report  # Return assembled report
}
