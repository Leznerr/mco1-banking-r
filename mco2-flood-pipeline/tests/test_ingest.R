source("R/ingest.R")  # Load ingestion module

library(testthat)  # Ensure testthat is available within file

fixture_path <- "sample-data/tiny_fixture.csv"  # Path to sample dataset

test_that("ingest errors on missing file", {
  expect_error(ingest_csv("nonexistent.csv"), "does not exist")  # Missing file guard
})

test_that("ingest rejects duplicate headers", {
  tmp <- tempfile(fileext = ".csv")  # Temporary CSV path
  writeLines(c("A,A", "1,2"), tmp)  # Duplicate header content
  expect_error(ingest_csv(tmp), "Duplicate column headers")  # Expect duplicate header error
})

test_that("ingest attaches parse problems attribute", {
  df <- ingest_csv(fixture_path)  # Ingest sample dataset
  probs <- attr(df, "ingest_problems")  # Retrieve attached problems
  expect_s3_class(probs, "tbl_df")  # Problems stored as tibble
  expect_true(all(c("row", "col", "expected", "actual", "file") %in% names(probs)))  # Standard readr problems structure
})

test_that("ingest rejects empty files", {
  tmp <- tempfile(fileext = ".csv")  # Create temporary file
  file.create(tmp)  # Empty file creation
  expect_error(ingest_csv(tmp), "Input file is empty")  # Expect empty file guard
})
