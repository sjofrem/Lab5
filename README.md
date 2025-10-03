
# Lab5

<!-- badges: start -->
[![R-CMD-check](https://github.com/sjofrem/Lab5/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sjofrem/Lab5/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of Lab5 is to be a R wrapper for the Kolada API.
Kolada is an open data service from Sweden that provides public statistics about municipalities and regions.
The API lets you query things like:
Population data
Employment rates
Education levels
Municipality info

## Installation

You can install the development version of Lab5 like so:

``` r
# install.packages("devtools")
devtools::install_github("sjofrem/Lab5")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(Lab5)

# Fetch data about a specific municipality (e.g., Stockholm)
mun <- get_municipality("0180")
print(mun)

# Retrieve information about a KPI (e.g., unemployment rate)
kpi <- get_kpi("N12345")
print_kpi(kpi)

# Get actual data values for the KPI and municipality
data <- get_kpi_data("N12345", "0180")
head(data)

```

