# Top Contractors Performance Ranking report

suppressPackageStartupMessages({  # Silence package startup messages
  library(dplyr)  # Data aggregation verbs
})

source("R/utils_format.R")  # Import safe stats helpers
source("R/utils_log.R")  # Logging utilities

report_top_contractors <- function(df) {  # Build contractor performance ranking
  aggregated <- df %>%  # Aggregate metrics per contractor
    group_by(Contractor, .drop = FALSE) %>%  # Group by contractor name
    summarise(  # Compute aggregation values
      NProjects = dplyr::n(),  # Count projects per contractor
      TotalCost = sum(ContractCost, na.rm = TRUE),  # Sum of contract costs
      AvgDelay = safe_mean(CompletionDelayDays),  # Average completion delay
      TotalSavings = sum(CostSavings, na.rm = TRUE),  # Total savings accumulated
      .groups = "drop"  # Drop grouping after summarise
    ) %>%
    filter(NProjects >= 5)  # Apply eligibility threshold
  top15 <- aggregated %>% arrange(desc(TotalCost)) %>% head(15)  # Pre-select top 15 by total cost
  report <- top15 %>%  # Compute reliability metrics and risk flag
    mutate(
      ReliabilityIndex = pmin(100, (1 - (AvgDelay / 90)) * (TotalSavings / pmax(TotalCost, 1)) * 100),  # Compute capped reliability
      ReliabilityIndex = ifelse(is.na(ReliabilityIndex), NA_real_, pmax(0, ReliabilityIndex)),  # Ensure non-negative values
      RiskFlag = ifelse(ReliabilityIndex < 50, "High Risk", "OK")  # Assign risk flag threshold
    ) %>%
    arrange(desc(ReliabilityIndex), desc(TotalCost), Contractor)  # Sort per specification
  log_info("report2 generated rows=%d", nrow(report))  # Emit log summary
  report  # Return final report
}
