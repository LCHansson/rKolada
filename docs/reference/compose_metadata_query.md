# Compose a query to fetch metadata from the Kolada API.

Mainly used as a supporting function for
[`get_metadata()`](https://lchansson.github.io/rKolada/reference/get_metadata.md)
but can also be used to create a working URL to paste in your web
browser.

## Usage

``` r
compose_metadata_query(
  entity = "kpi",
  title = NULL,
  id = NULL,
  municipality = NULL,
  region_type = NULL,
  page = NA,
  per_page = NA,
  version = "v3"
)
```

## Arguments

- entity:

  Any allowed metadata entity. Check
  [`allowed_entities()`](https://lchansson.github.io/rKolada/reference/allowed_entities.md)
  to see an updated list.

- title:

  A free-form search term or the exact title of any entry in the current
  entity. Case insensitive.

- id:

  The ID of any entry in the current entity.

- municipality:

  If entity is `"ou"`, the municipality parameter can be added to narrow
  the search.

- region_type:

  (Optional) Filter municipalities by region type. Only used when
  `entity` is `"municipality"`. Common values: `"K"` (municipality),
  `"L"` (region).

- page:

  What page to fetch. Used mainly in large queries. Fetches a page using
  the value of `"per_page"` as pagination delimiter.

- per_page:

  Number of results per page.

- version:

  Version of the API. Defaults to `"v3"`.

## Value

A string containing a URL to the Kolada REST API.
