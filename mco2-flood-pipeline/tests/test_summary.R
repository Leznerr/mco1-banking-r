source("R/summary.R")  # Load summary module

library(testthat)  # Testing utilities

sample_df <- data.frame(  # Construct dataset for summary stats
  Province = c("A", "B", "A"),
  Contractor = c("X", "Y", "X"),
  CompletionDelayDays = c(10, NA, 20),
  CostSavings = c(100, -50, 25),
  stringsAsFactors = FALSE
)

summary <- build_summary(sample_df)  # Generate summary

test_that("summary contains scalar metrics", {
  expect_named(summary, c("total_projects", "total_contractors", "total_provinces", "global_avg_delay", "total_savings"))  # Check keys
})

test_that("summary values computed correctly", {
  expect_equal(summary$total_projects, 3)  # Row count
  expect_equal(summary$total_contractors, 2)  # Distinct contractors
  expect_equal(summary$total_provinces, 2)  # Distinct provinces
  expect_equal(summary$global_avg_delay, mean(c(10, 20), na.rm = TRUE))  # Average delay ignoring NA
  expect_equal(summary$total_savings, 75)  # Sum of savings
})
