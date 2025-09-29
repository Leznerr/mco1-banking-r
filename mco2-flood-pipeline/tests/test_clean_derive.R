source("R/clean.R")  # Load cleaning utilities
source("R/derive.R")  # Load derivation utilities

library(testthat)  # Provide testing helpers

raw_df <- data.frame(  # Construct raw dataset for cleaning tests
  Region = c("region ii", "region ii", "region ii"),
  MainIsland = c("luzon", "luzon", "luzon"),
  Province = c("Isabela", "Isabela", "Isabela"),
  FundingYear = c("2021", "2022", "2023"),
  TypeOfWork = c(" Flood Control ", "Flood Control", "Flood Control"),
  StartDate = c("2021-01-05", "02/10/2022", "2023-03-01"),
  ActualCompletionDate = c("2021-02-05", "2022-05-12", "2023-02-20"),
  ApprovedBudgetForContract = c("Php 1,000", "Php 2,000", "Php 3,000"),
  ContractCost = c("Php 800", "Php 2,500", "Php 2,000"),
  Contractor = c("acme corp", "acme corp", "acme corp"),
  Latitude = c("17.0", "", "17.5"),
  Longitude = c("121.8", "121.9", ""),
  stringsAsFactors = FALSE
)

cleaned <- clean_data(raw_df)  # Apply cleaning transformations

test_that("dates are parsed to Date", {
  expect_s3_class(cleaned$StartDate, "Date")  # StartDate should be Date
  expect_s3_class(cleaned$ActualCompletionDate, "Date")  # Completion date should be Date
})

test_that("money fields become numeric", {
  expect_type(cleaned$ApprovedBudgetForContract, "double")  # Budget numeric
  expect_type(cleaned$ContractCost, "double")  # Cost numeric
})

test_that("funding year coerces to integer", {
  expect_true(is.integer(cleaned$FundingYear))  # Funding year integer type
})

test_that("geo imputation only when both coordinates missing", {
  expect_equal(cleaned$Latitude[2], cleaned$Latitude[1])  # Second row should impute latitude from mean
  expect_equal(cleaned$Longitude[2], cleaned$Longitude[1])  # Second row should impute longitude from mean
})

test_that("single-coordinate gaps are not imputed", {
  expect_equal(cleaned$Latitude[3], 17.5)  # Latitude remains provided value
  expect_true(is.na(cleaned$Longitude[3]))  # Longitude stays NA because partner coordinate present
})

derived <- derive_fields(cleaned)  # Apply derivations

test_that("derived fields computed correctly", {
  expect_equal(derived$CostSavings[1], 200)  # Savings difference
  expect_equal(derived$CompletionDelayDays[1], as.numeric(as.Date("2021-02-05") - as.Date("2021-01-05")))  # Delay in days
})
