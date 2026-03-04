# Search a Kolada Municipality Group metadata table for group names

Search a Kolada Municipality Group metadata table. Only keep rows that
contain the search query. Searches group titles and group IDs. Note that
this function does not search for individual municipalities contained
within municipality groups! To search for KPIs within a KPI group, see
examples below for an example using `municipality_grp_unnest`.

## Usage

``` r
municipality_grp_search(munic_grp_df, query)
```

## Arguments

- munic_grp_df:

  A Kolada Municipality Group metadata table, as created by e.g.
  `get_municipality_groups`.

- query:

  A search term or a vector of search terms to filter by. Case
  insensitive.

## Value

A Kolada Municipality Group metadata table
