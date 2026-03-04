# Extract municipality ID strings from a Kolada municipality group table

This function is primarily intended as a convenient way to pass a
(filtered) Kolada municipality group metadata table to
[`get_values`](https://lchansson.github.io/rKolada/reference/get_values.md).
All IDs of the municipalities contained in each group in the table are
extracted.

## Usage

``` r
municipality_grp_extract_ids(munic_grp_df)
```

## Arguments

- munic_grp_df:

  A Kolada municipality group table, as created by e.g.
  `get_municipality_groups`.

## Value

A vector of Municipality IDs.
