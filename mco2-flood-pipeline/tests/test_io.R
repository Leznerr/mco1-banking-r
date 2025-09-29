source("R/io.R")  # Load IO helpers under test

library(testthat)

test_that("write_report_csv refreshes existing outputs", {
  tmpdir <- tempfile("io-test-")
  dir.create(tmpdir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmpdir, recursive = TRUE, force = TRUE))
  path <- file.path(tmpdir, "report.csv")

  initial_df <- data.frame(Value = 1, stringsAsFactors = FALSE)
  write_report_csv(initial_df, path)
  initial_contents <- readLines(path)
  expect_true(any(grepl("1.00", initial_contents, fixed = TRUE)))

  updated_df <- data.frame(Value = 99, stringsAsFactors = FALSE)
  write_report_csv(updated_df, path)
  updated_contents <- readLines(path)

  expect_false(identical(initial_contents, updated_contents))
  expect_true(any(grepl("99.00", updated_contents, fixed = TRUE)))
})

test_that("write_summary_json refreshes existing outputs", {
  tmpdir <- tempfile("io-test-")
  dir.create(tmpdir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmpdir, recursive = TRUE, force = TRUE))
  path <- file.path(tmpdir, "summary.json")

  write_summary_json(list(value = 1), path)
  initial_payload <- jsonlite::read_json(path, simplifyVector = TRUE)
  expect_equal(initial_payload$value, 1)

  write_summary_json(list(value = 42), path)
  refreshed_payload <- jsonlite::read_json(path, simplifyVector = TRUE)

  expect_equal(refreshed_payload$value, 42)
})
