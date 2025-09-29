# MCO2 Flood Control Data Analysis Pipeline

This repository contains a production-ready R implementation of the MCO2 Flood Control Data Analysis Pipeline. It ingests Department of Public Works and Highways (DPWH) flood control project data, validates and cleans it, derives analytics, and generates standardized reports and summary outputs.

## Installation

Install the required R packages from CRAN:

```sh
R -q -e "install.packages(c('optparse','readr','dplyr','stringr','lubridate','jsonlite','tibble','testthat'), repos='https://cloud.r-project.org')"
```

## Usage

Run the pipeline against an input CSV:

```sh
Rscript main.R --input dpwh_flood_control_projects.csv --outdir outputs
```

## Testing

Execute the automated test suite:

```sh
R -q -e "testthat::test_dir('tests')"
```

## Project Structure

The project is organized into clearly delineated modules:

- `main.R`: orchestrates the CLI workflow end-to-end.
- `R/`: ingestion, validation, cleaning, derivation, reporting, logging, formatting, and I/O utilities.
- `tests/`: unit and integration tests using `testthat`.
- `sample-data/`: sample datasets for local testing.
- `outputs/`: generated artifacts.

## Troubleshooting

- **Command not found**: Ensure Rscript is on your `PATH` (on Windows, add the R `bin` directory to the Environment Variables panel).
- **CSV header mismatches**: Confirm the input file contains the expected headers exactly (`Region`, `MainIsland`, `Province`, `FundingYear`, `TypeOfWork`, `StartDate`, `ActualCompletionDate`, `ApprovedBudgetForContract`, `ContractCost`, `Contractor`, `Latitude`, `Longitude`).
