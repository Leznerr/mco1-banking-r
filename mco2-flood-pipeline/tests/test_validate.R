source("R/validate.R")  # Load validation utilities

library(testthat)  # Ensure testthat functions available

base_df <- data.frame(  # Construct minimal valid dataset
  Region = "Region I",
  MainIsland = "Luzon",
  Province = "Ilocos Norte",
  FundingYear = "2021",
  TypeOfWork = "Floodwall",
  StartDate = "2021-01-01",
  ActualCompletionDate = "2021-06-01",
  ApprovedBudgetForContract = 1,
  ContractCost = 1,
  Contractor = "ABC",
  Latitude = 15,
  Longitude = 120,
  stringsAsFactors = FALSE
)

test_that("validate_schema errors when columns missing", {
  df <- base_df
  df$Province <- NULL  # Remove required column
  expect_error(validate_schema(df), "Missing required columns")  # Expect missing column error
})

test_that("validate_schema rejects fractional funding years", {
  df <- base_df
  df$FundingYear <- "2021.5"  # Fractional year value
  expect_error(validate_schema(df), "whole numbers")  # Expect fractional rejection
})

test_that("assert_year_filter flags invalid years", {
  df <- base_df
  df$FundingYear <- 2024  # Year outside allowed set
  expect_error(assert_year_filter(df), "outside allowed range")  # Expect range error
})

test_that("validation passes for clean data", {
  expect_no_error(validate_schema(base_df))  # Should validate successfully
})
