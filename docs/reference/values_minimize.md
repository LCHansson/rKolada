# Simplify a Kolada values table

Simplify a Kolada values table, i.e as created by
[`get_values()`](https://lchansson.github.io/rKolada/reference/get_values.md),
by removing columns that contain monotonous data, i.e. that contain only
one value for all observations.

## Usage

``` r
values_minimize(values_df)
```

## Arguments

- values_df:

  A Kolada value table, as created by
  [`get_values()`](https://lchansson.github.io/rKolada/reference/get_values.md).

## Value

A Kolada values table

## Examples

``` r
# Download values for all available years of a given KPI for
# Malmö municipality (code 1280)
if (kolada_available()) {
vals <- get_values(kpi = "N45933", municipality = "1280", simplify = TRUE)
# (Returns a table with 5 rows and 8 columns)

# Remove columns with no information to differentiate between rows
values_minimize(vals)
# (Returns a table with 5 rows and 4 columns)
}
#> # A tibble: 9 × 4
#>   value kpi     year municipality
#>   <dbl> <chr>  <int> <chr>       
#> 1  24.8 N45933  2016 Malmö       
#> 2  26.7 N45933  2017 Malmö       
#> 3  38.0 N45933  2018 Malmö       
#> 4  35.6 N45933  2019 Malmö       
#> 5  30.3 N45933  2020 Malmö       
#> 6  21.2 N45933  2021 Malmö       
#> 7  24.2 N45933  2022 Malmö       
#> 8  43.6 N45933  2023 Malmö       
#> 9  34.5 N45933  2024 Malmö       
```
