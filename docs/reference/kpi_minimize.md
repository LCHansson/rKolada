# Simplify a KPI table

Remove all columns from a Kolada KPI metadata table that are monotonous
across the table, i.e. columns that contain only one single value. Also
remove undocumented columns, i.e. columns that contain unintelligible
and undocumented information.

## Usage

``` r
kpi_minimize(kpi_df, remove_monotonous_data = TRUE)
```

## Arguments

- kpi_df:

  A Kolada KPI metadata table, e.g. as created by
  [`get_kpi()`](https://lchansson.github.io/rKolada/reference/get_kpi.md).

- remove_monotonous_data:

  Remove columns from the KPI table which contain exactly the same
  information for all entries in the table?

## Value

A Kolada KPI metadata table
