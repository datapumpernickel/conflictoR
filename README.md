
<!-- README.md is generated from README.Rmd. Please edit that file -->

# conflictoR

<!-- badges: start -->

![CRAN Status](https://img.shields.io/badge/CRAN-Unpublished-red.svg)
![Project
Status](https://img.shields.io/badge/status-under--construction-orange.svg)
![R Version](https://img.shields.io/badge/R-%3E%3D%204.0.0-blue.svg)
![License](https://img.shields.io/badge/license-MIT-green.svg) ![Build
Status](https://img.shields.io/github/actions/workflow/status/datapumpernickel/conflictoR/r.yml)
![Last
Commit](https://img.shields.io/github/last-commit/datapumpernickel/conflictoR)
![GitHub Repo
stars](https://img.shields.io/github/stars/datapumpernickel/conflictoR?style=social)

<!-- badges: end -->

The goal of conflictoR is to provide a user-friendly R interface for
accessing and analyzing conflict data from the Uppsala Conflict Data
Program (UCDP) API. With conflictoR, you can dynamically fetch, filter,
and analyze conflict datasets directly from the source, streamlining
research and data analysis workflows related to global conflicts. \##
Installation

You can install the development version of conflictoR from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("datapumpernickel/conflictoR")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(conflictoR)

# Fetch UCDP GED events with specific filters
response_ged <- cl_get_data(
  resource = "gedevents",
  version = "24.1",
  country = c(90, 91, 92), # Guatemala, Honduras, El Salvador
  start_date = "2000-01-01",
  end_date = "2007-10-12",
  type_of_violence = c(1, 3)
)

# Print the response
print(response_ged)
```