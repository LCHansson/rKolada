# Compose a query to fetch metadata from the Kolada API. Its use is mainly

Mainly used as a supporting function for
[`get_values()`](https://lchansson.github.io/rKolada/reference/get_values.md)
but can also be used to create a working URL to paste in your web
browser.

## Usage

``` r
compose_data_query(
  kpi = NULL,
  municipality = NULL,
  period = NULL,
  ou = NULL,
  unit_type = "municipality",
  from_date = NULL,
  page = NA,
  per_page = NA,
  version = "v3"
)
```

## Arguments

- kpi:

  What kpis should be fetched? Can be a single name or a vector of
  names.

- municipality:

  For which municipalities should data be fetched? Can be a single name
  or a vector of names.

- period:

  For what years should data be fetched? Can be one or more four-digit
  integers or character strings.

- ou:

  (Optional) for what Operating Units should data be fetched? Only
  available for certain KPIs. Only used if `unit_type` is set to `"ou"`.

- unit_type:

  One of `"municipality"` or `"ou"`. Whether to fetch data for
  Municipalities or Organizational Units. Units. Defaults to
  `"municipality"`.

- from_date:

  (Optional) Only return data updated after this date. Format:
  `"YYYY-MM-DD"`.

- page:

  What page to fetch. Used mainly in large queries. Fetches a page using
  the value of `"per_page"` as pagination delimiter.

- per_page:

  Number of results per page.

- version:

  Version of the API. Defaults to `"v3"`.

## Value

A string containing a URL to the Kolada REST API.
