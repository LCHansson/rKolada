# Search a Kolada KPI Group metadata table for group names

Search a Kolada KPI Group metadata table. Only keep rows that contain
the search query. Searches group titles and group IDs. Note that this
function does not search for individual KPIs contained within KPI
groups! To search for KPIs within a KPI group, see examples below for an
example using `kpi_grp_unnest`.

## Usage

``` r
kpi_grp_search(kpi_grp_df, query)
```

## Arguments

- kpi_grp_df:

  A Kolada KPI Group metadata table, as created by e.g.
  `get_kpi_groups`.

- query:

  A search term or a vector of search terms to filter by. Case
  insensitive.

## Value

A Kolada KPI Group metadata table

## Examples

``` r
if (kolada_available()) {
kpi_grp_df <- get_kpi_groups()

# Which KPI groups match the keyword "ekonomi" (economy)?
kpi_grp_df |> kpi_grp_search("ekonomi")

# Which KPI groups contain KPIs matching the keyword "arbete" (work/labour)?
kpi_grp_df |>
  kpi_grp_unnest() |>
  kpi_search("arbete") |>
  dplyr::count(group_title, sort = TRUE)
}
#> # A tibble: 29 × 2
#>    group_title                                 n
#>    <chr>                                   <int>
#>  1 Medborgarundersökningen - Kommun            8
#>  2 Elevenkät gymnasieelever år 2               7
#>  3 Elevenkät årskurs 5                         7
#>  4 Elevenkät årskurs 8                         7
#>  5 Enhet elevenkät årskurs 5                   7
#>  6 Enhet elevenkät årskurs 8                   7
#>  7 Enhet, elevenkät gymnasieelever år 2        7
#>  8 Pedagogisk personal enkät - grundskola      7
#>  9 Pedagogisk personal enkät gymnasieskola     7
#> 10 Personal - Region                           6
#> # ℹ 19 more rows
```
