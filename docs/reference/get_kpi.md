# Download metadata for a specific entity from the Kolada API

There are five different types of metadata entities in the Kolada
database: "kpi", "kpi_groups", "municipality", "municipality_groups",
and "ou". For every entity there is a corresponding function
`get_ENTITY` which retrieves a table with the metadata for that entity.
The `get_ENTITY` functions are thin wrappers around
[`get_metadata`](https://lchansson.github.io/rKolada/reference/get_metadata.md).

## Usage

``` r
get_kpi(
  id = NULL,
  max_results = NULL,
  cache = FALSE,
  cache_location = tempdir,
  verbose = FALSE
)

get_kpi_groups(
  id = NULL,
  cache = FALSE,
  max_results = NULL,
  cache_location = tempdir,
  verbose = FALSE
)

get_ou(
  id = NULL,
  municipality = NULL,
  max_results = NULL,
  cache = FALSE,
  cache_location = tempdir,
  verbose = FALSE
)

get_municipality(
  id = NULL,
  region_type = NULL,
  cache = FALSE,
  max_results = NULL,
  cache_location = tempdir,
  verbose = FALSE
)

get_municipality_groups(
  id = NULL,
  cache = FALSE,
  max_results = NULL,
  cache_location = tempdir,
  verbose = FALSE
)
```

## Arguments

- id:

  (Optional) One or several KPI IDs

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

- municipality:

  (Optional) A string or vector of strings containing municipality
  codes. If getting OU data, you can use this parameter to narrow the
  search.

- region_type:

  (Optional) Filter municipalities by region type. Common values: `"K"`
  (municipality), `"L"` (region).

## Value

Returns a tibble with metadata for the specified entity. In rKolada
terminology, a table returned by e.g. `get_kpi` is referred to as a
`kpi_df` and can be passed to functions starting with "kpi" such as
[`kpi_bind_keywords`](https://lchansson.github.io/rKolada/reference/kpi_bind_keywords.md).

## Examples

``` r
# Download KPI table and store a cache copy of the results in a temporary folder
# (to actually download all available data, don't specify max_results)
if (kolada_available()) {
kpi_df <- get_kpi(cache = TRUE, max_results = 100)
}
```
