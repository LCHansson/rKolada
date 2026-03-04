# Download metadata from the Kolada API

This is a generalized function for downloading metadata from the Kolada
API. The function parameters closely mask the names specified in the
original API. For further information about the Kolada API
specification, please see the [official documentation on
GitHub](https://github.com/Hypergene/kolada).

## Usage

``` r
get_metadata(
  entity = "kpi",
  title = NULL,
  id = NULL,
  municipality = NULL,
  region_type = NULL,
  max_results = NULL,
  cache = FALSE,
  cache_location = tempdir,
  verbose = FALSE
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

- max_results:

  (Optional) Specify the maximum number of results returned by the
  query.

- cache:

  Logical. If TRUE, downloaded data are stored to the local disk in the
  place specified by `cache_location`. If data is already present on the
  local disk, this data is returned instead of downloading data from the
  API.

- cache_location:

  Where to store and search for cached data. Can be a path to a
  directory or the name of any function that returns the path to a
  directory when called, like
  [`getwd`](https://rdrr.io/r/base/getwd.html). Defaults to
  [`tempdir`](https://rdrr.io/r/base/tempfile.html).

- verbose:

  Whether to print the call to the Kolada API as a message to the R
  console.

## Value

Returns a tibble with metadata for the specified entity. In rKolada
terminology, a table returned by e.g. `entity = "kpi"` is referred to as
a `kpi_df` and can be passed to functions starting with "kpi" such as
[`kpi_bind_keywords`](https://lchansson.github.io/rKolada/reference/kpi_bind_keywords.md).

## See also

[`get_kpi`](https://lchansson.github.io/rKolada/reference/get_kpi.md),
[`get_kpi_groups`](https://lchansson.github.io/rKolada/reference/get_kpi.md),
[`get_municipality`](https://lchansson.github.io/rKolada/reference/get_kpi.md),
[`get_municipality_groups`](https://lchansson.github.io/rKolada/reference/get_kpi.md),
[`get_ou`](https://lchansson.github.io/rKolada/reference/get_kpi.md)
