source("R/report3.R")  # Load report module

library(testthat)  # Testing utilities

sample_df <- data.frame(  # Construct dataset for trends report
  FundingYear = c(2021, 2021, 2022, 2022, 2022, 2023),
  TypeOfWork = c("Levee", "Levee", "Levee", "Channel", "Channel", "Levee"),
  CostSavings = c(10, 20, 30, -5, 15, 5),
  stringsAsFactors = FALSE
)

report <- report_overrun_trends(sample_df)  # Generate report

test_that("report schema matches specification", {
  expect_equal(names(report), c("FundingYear", "TypeOfWork", "N", "AvgSavings", "OverrunRate", "YoY_vs_2021"))  # Validate columns
})

test_that("baseline rows have NA YoY", {
  expect_true(all(is.na(report$YoY_vs_2021[report$FundingYear == 2021])))  # 2021 rows should be NA
})

test_that("missing baseline yields NA", {
  channel_rows <- report[report$TypeOfWork == "Channel", ]
  expect_true(all(is.na(channel_rows$YoY_vs_2021)))  # No 2021 baseline for Channel
})

test_that("YoY computed when baseline available", {
  levee_2022 <- report[report$TypeOfWork == "Levee" & report$FundingYear == 2022, ]
  expect_equal(round(levee_2022$YoY_vs_2021, 2), 100)  # Baseline 15 -> (30-15)/15*100 = 100
})

test_that("report sorted by year then AvgSavings desc", {
  years <- report$FundingYear
  expect_true(all(diff(years) >= 0))  # FundingYear ascending
  by_year <- split(report, report$FundingYear)
  for (chunk in by_year) {
    expect_true(all(diff(chunk$AvgSavings) <= 0))  # AvgSavings descending within year
  }
})
