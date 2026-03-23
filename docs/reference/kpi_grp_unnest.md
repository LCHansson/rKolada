# Create a KPI table from a Kolada KPI Group metadata table

KPI groups are a convenient way to discover sets of KPIs that can be
used to highlight different aspects of a policy area. A practical
workflow for discovering such sets can be to search through KPI Group
metadata using
[`kpi_grp_search()`](https://lchansson.github.io/rKolada/reference/kpi_grp_search.md)
to search for keywords and
[`kpi_grp_describe()`](https://lchansson.github.io/rKolada/reference/kpi_grp_describe.md)
to inspect contents of KPI groups. Once you have created a KPI group
table that has been narrowed down to the group/s you are looking for,
`kpi_grp_unnest()` is used to create a KPI metadata table for further
processing.

## Usage

``` r
kpi_grp_unnest(kpi_grp_df)
```

## Arguments

- kpi_grp_df:

  A Kolada KPI Group metadata table, as created by e.g.
  `get_kpi_groups`.

## Value

A Kolada KPI metadata table

## Examples

``` r
if (kolada_available()) {
# Download KPI Group metadata
kpi_grp_df <- get_kpi_groups()

# Create a KPI metadata table from KPI groups matching the term
# "utbidning" (education)
kpi_grp_df |>
  kpi_grp_search("utbildning") |>
  kpi_grp_unnest()
}
#> # A tibble: 12 × 4
#>    id     title                                             group_id group_title
#>    <chr>  <chr>                                             <chr>    <chr>      
#>  1 N00905 Mediannettoinkomst, kr/inv 20+                    G2KPI11… Utbildning…
#>  2 N00906 Sammanräknad förvärvsinkomst i åldern 20-64 år (… G2KPI11… Utbildning…
#>  3 N00926 Företagskonkurser, antal/1000 inv, 16-64 år       G2KPI11… Utbildning…
#>  4 N00999 Nystartade företag, antal/1000 inv, 16-64 år      G2KPI11… Utbildning…
#>  5 N01006 Företagskonkurser, antal anställda, antal/1000 i… G2KPI11… Utbildning…
#>  6 N01982 Invånare 25-64 år med eftergymnasial utbildning,… G2KPI11… Utbildning…
#>  7 N01984 Invånare 25-64 år med förgymnasial utbildning, a… G2KPI11… Utbildning…
#>  8 N02243 Sysselsatta invånare 20-65 år, andel (%)          G2KPI11… Utbildning…
#>  9 N03932 Arbetslöshet 16-65 år, årsmedelvärde, andel (%) … G2KPI11… Utbildning…
#> 10 N31807 Invånare som någon gång under året erhållit ekon… G2KPI11… Utbildning…
#> 11 N60802 Elever i år 9 som är behöriga till yrkesprogram,… G2KPI11… Utbildning…
#> 12 N60960 Gymnasieelever med examen eller studiebevis inom… G2KPI11… Utbildning…
```
