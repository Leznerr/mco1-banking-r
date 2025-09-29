.PHONY: install run test clean

install:
R -q -e "install.packages(c('optparse','readr','dplyr','stringr','lubridate','jsonlite','tibble','testthat'), repos='https://cloud.r-project.org')"

run:
Rscript main.R --input dpwh_flood_control_projects.csv --outdir outputs

test:
R -q -e "testthat::test_dir('tests')"

clean:
rm -rf outputs/*
