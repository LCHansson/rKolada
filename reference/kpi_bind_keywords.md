# Add keyword columns to a Kolada KPI table

Identify `n` keywords describing the KPI and add them as new columns.
Keywords are inferred from the `title` field of the table.

## Usage

``` r
kpi_bind_keywords(kpi_df, n = 2, form = c("wide", "long"))
```

## Arguments

- kpi_df:

  A Kolada KPI metadata table, e.g. as created by
  [`get_kpi()`](https://lchansson.github.io/rKolada/reference/get_kpi.md).

- n:

  How many keyword columns should be added?

- form:

  Can be either "wide" (default) or "long". Whether to return keywords
  as separate columns ("wide") or as separate rows, duplicating all
  other data ("long").

## Value

A Kolada KPI metadata table

## Examples

``` r
if (kolada_available()) {
kpi_df <- get_kpi(id = c("N45933", "U28563")) |>
  kpi_bind_keywords(n = 3)
}
```
