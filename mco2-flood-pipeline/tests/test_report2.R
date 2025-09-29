source("R/report2.R")  # Load report module

library(testthat)  # Testing utilities

sample_df <- data.frame(  # Construct dataset with multiple contractors
  Contractor = c(rep("Alpha", 6), rep("Beta", 6), rep("Gamma", 4)),
  ContractCost = c(rep(100, 6), rep(80, 6), rep(50, 4)),
  CompletionDelayDays = c(rep(10, 6), rep(120, 6), rep(5, 4)),
  CostSavings = c(rep(20, 6), rep(-10, 6), rep(5, 4)),
  stringsAsFactors = FALSE
)

report <- report_top_contractors(sample_df)  # Generate contractor ranking

test_that("eligible contractors meet minimum project count", {
  expect_true(all(report$NProjects >= 5))  # Ensure eligibility filter applied
})

test_that("report capped to top 15", {
  expect_lte(nrow(report), 15)  # Verify maximum row count
})

test_that("reliability index bounded", {
  expect_true(all(report$ReliabilityIndex <= 100 | is.na(report$ReliabilityIndex)))  # Upper bound check
  expect_true(all(report$ReliabilityIndex >= 0 | is.na(report$ReliabilityIndex)))  # Lower bound check
})

test_that("risk flag identifies low reliability", {
  beta_row <- report[report$Contractor == "Beta", ]
  expect_equal(beta_row$RiskFlag, "High Risk")  # Beta should be flagged as high risk
})

test_that("ineligible contractors removed", {
  expect_false("Gamma" %in% report$Contractor)  # Contractor with <5 projects excluded
})
