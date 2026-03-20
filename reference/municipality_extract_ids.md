# Extract a vector of municipality ID strings from a Kolada municipality table

This function is primarily intended as a convenient way to pass a
(filtered) Kolada municipality metadata table to
[`get_values()`](https://lchansson.github.io/rKolada/reference/get_values.md).

## Usage

``` r
municipality_extract_ids(munic_df)
```

## Arguments

- munic_df:

  A Kolada Municipality metadata table, as created by e.g.
  `get_municipality`.

## Examples

``` r
if (kolada_available()) {
# Download Kolada data for all municipalities of type "L"
# (regions and national total) for KPI "N45933"
munic_filter <- get_municipality() |>
  municipality_search("L", column = "type")

kld_data <- get_values(
  kpi = "N45933",
  municipality = municipality_extract_ids(munic_filter),
  period = 2022
)
}
```
