# Describe the KPIs in a Kolada KPI Group metadata table

Print a human-readable description of each row of a KPI Group metadata
table, including member KPIs (up to a maximum number of rows). Can be
printed either directly to the R console or used to populate a R
markdown document, which can be useful for documentation purposes.

## Usage

``` r
kpi_grp_describe(
  kpi_grp_df,
  max_n = 5,
  format = "inline",
  heading_level = 2,
  sub_heading_level = heading_level + 1
)
```

## Arguments

- kpi_grp_df:

  A Kolada KPI Group metadata table, as created by e.g.
  `get_kpi_groups`.

- max_n:

  The maximum number of KPI groups to describe.

- format:

  Output format. Can be one of "inline" or "md" (markdown).

- heading_level:

  The top heading level output format is "md".

- sub_heading_level:

  The sub heading level output format is "md".

## Value

Returns the object passed to the function, invisibly, to be re-used in a
pipe.
