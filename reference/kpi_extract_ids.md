# Extract a vector of KPI ID strings from a Kolada KPI metadata table

This function is primarily intended as a convenient way to pass a
(filtered) Kolada KPI metadata table to
[`get_values`](https://lchansson.github.io/rKolada/reference/get_values.md).

## Usage

``` r
kpi_extract_ids(kpi_df)
```

## Arguments

- kpi_df:

  A Kolada KPI metadata table, e.g. as created by
  [`get_kpi`](https://lchansson.github.io/rKolada/reference/get_kpi.md).

## Value

A vector of KPI IDs.

## Examples

``` r
if (kolada_available()) {
# Download Kolada data for the first KPI in the list matching the term "Grundskola" (primary school)
# for the years 2010-2019
# (omit the parameter "max_results" to actually download all data)
kpi_filter <- get_kpi(max_results = 500) %>%
  kpi_search("Grundskola")

# Only keep the top row
kpi_filter <- kpi_filter[1, ]

# Only download 100 observations
# (omit the parameter "max_results" to actually download all data)
kld_data <- get_values(
  kpi = kpi_extract_ids(kpi_filter),
  period = 2010:2019,
  max_results = 100
)
}
```
