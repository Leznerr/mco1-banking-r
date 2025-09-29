# Command line interface utilities for the pipeline

suppressPackageStartupMessages({  # Suppress startup noise from optparse
  library(optparse)  # Provide CLI option parsing
})

normalize_path <- function(path) {  # Normalize paths to absolute form
  normalizePath(path, winslash = "/", mustWork = FALSE)  # Use base normalizePath for cross-platform consistency
}

parse_cli_args <- function() {  # Construct and parse the command line options
  option_list <- list(  # Define the supported CLI flags
    make_option(c("-i", "--input"), type = "character", help = "Input CSV file", metavar = "FILE"),  # Input dataset flag
    make_option(c("-o", "--outdir"), type = "character", default = "outputs", help = "Output directory [default %default]", metavar = "DIR")  # Output directory flag
  )
  parser <- OptionParser(option_list = option_list, usage = "Rscript main.R --input <file> --outdir <dir>")  # Build parser with usage banner
  args <- parse_args(parser)  # Parse actual command line arguments
  if (is.null(args$input) || !nzchar(args$input)) {  # Ensure input flag provided
    stop("--input is required")  # Fail fast when missing
  }
  args$input <- normalize_path(args$input)  # Normalize input path for downstream use
  args$outdir <- normalize_path(args$outdir)  # Normalize output directory path
  args  # Return parsed arguments
}

validate_cli_paths <- function(args) {  # Validate provided CLI paths
  if (!file.exists(args$input)) {  # Ensure input file exists on disk
    stop(sprintf("Input file not found: %s", args$input))  # Raise descriptive error
  }
  invisible(args)  # Return arguments invisibly for chaining
}
