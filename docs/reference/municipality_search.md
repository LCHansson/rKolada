# Search a Kolada municipality metadata table

Search a Kolada municipality metadata table. Only keep rows that contain
the search query. Note that some a quer might be both the name, or part
of a name, of a municipality and part of the name of a region. Thus, a
search might return rows for both municipalities and regions. To avoid
this you can use
[`dplyr::filter`](https://dplyr.tidyverse.org/reference/filter.html) to
filter the `type` column to keep only "K" (municipalities) or "L"
(regions) rows. See also examples below for an alternative approach
avoiding any direct calls to `filter`.

## Usage

``` r
municipality_search(munic_df, query, column = NULL)
```

## Arguments

- munic_df:

  A Kolada Municipality metadata table, as created by e.g.
  `get_municipality`.

- query:

  A search term or a vector of search terms to filter by. Case
  insensitive.

- column:

  (Optional) A string or character vector with the names of columns in
  which to search for `query`.

## Value

A Kolada Municipality metadata table

## Examples

``` r
if (kolada_available()) {
# Search for a single search term in a municipality table
munic_df <- get_municipality()
municipality_search(munic_df, "Arboga")

# Only keep columns with type == "K" (keep municipalities, drop regions)
munic_filter <- get_municipality(cache = TRUE) %>%
  municipality_search("K", column = "type")
}
```
