source("R/report1.R")  # Load report module

library(testthat)  # Testing utilities

sample_df <- data.frame(  # Construct dataset for report calculations
  Region = c("Region A", "Region A", "Region B"),
  MainIsland = c("Luzon", "Luzon", "Visayas"),
  ApprovedBudgetForContract = c(100, 200, 150),
  CostSavings = c(10, -20, 30),
  CompletionDelayDays = c(20, 40, 10),
  stringsAsFactors = FALSE
)

report <- report_regional_efficiency(sample_df)  # Generate report

test_that("report columns and order", {
  expect_equal(names(report), c("Region", "MainIsland", "TotalApprovedBudget", "MedianSavings", "AvgDelay", "Delay30Rate", "EfficiencyScore"))  # Validate schema
})

test_that("efficiency score bounded", {
  expect_true(all(report$EfficiencyScore >= 0 | is.na(report$EfficiencyScore)))  # Lower bound check
  expect_true(all(report$EfficiencyScore <= 100 | is.na(report$EfficiencyScore)))  # Upper bound check
})

test_that("delay rate computed", {
  group_a <- report[report$Region == "Region A", ]
  expect_equal(group_a$Delay30Rate, 50)  # One of two projects delayed >30 days
})
