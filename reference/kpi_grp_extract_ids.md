# Extract KPI ID strings from a Kolada KPI Group metadata table

This function is primarily intended as a convenient way to pass a
(filtered) Kolada KPI Group metadata table to
[`get_values`](https://lchansson.github.io/rKolada/reference/get_values.md).
All IDs of the KPIs contained in each group in the table are extracted.

## Usage

``` r
kpi_grp_extract_ids(kpi_grp_df)
```

## Arguments

- kpi_grp_df:

  A Kolada KPI Group metadata table, as created by e.g.
  `get_kpi_groups`.

## Value

A vector of KPI IDs.
