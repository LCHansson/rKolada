# Search for Kolada KPIs using a Kolada KPI table

Search a Kolada KPI metadata table. Only keep rows that contain the
search query. Matches against all columns or columns named with the
`column` parameter. For more precise matching, please use
[dplyr::filter](https://dplyr.tidyverse.org/reference/filter.html).

## Usage

``` r
kpi_search(kpi_df, query, column = NULL)
```

## Arguments

- kpi_df:

  A Kolada KPI metadata table, e.g. as created by
  [`get_kpi()`](https://lchansson.github.io/rKolada/reference/get_kpi.md).

- query:

  A search term or a vector of search terms to filter by. Case
  insensitive.

- column:

  (Optional) A string or character vector with the names of columns in
  which to search for `query`.

## Value

A Kolada KPI metadata table

## Examples

``` r
if (kolada_available()) {
# Search for a single search term in a KPI table
kpis <- get_kpi(id = c("N11002", "N11003", "N11004", "N11005"))
kpi_filter <- kpi_search(kpis, "kostnad")

# Add keywords to a KPI table and search for multiple terms among
# the keywords
kpi_filter <- get_kpi(id = c("N11002", "N11003", "N11004", "N11005")) |>
  kpi_bind_keywords(n = 3) |>
  kpi_search(
    query = c("nettokostnad", "öppen"),
    column = c("keyword_1", "keyword_2", "keyword_3")
  )
}
```
