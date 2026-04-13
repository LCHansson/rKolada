# Get data from Kolada

Download a table of data from Kolada. Data is selected based on three
metadata dimensions: KPI (ID), municipality (ID) and period (years). You
must supply arguments for at least two of these three dimensions. If a
dimension is omitted, all available data for that dimension will be
downloaded.

## Usage

``` r
get_values(
  kpi = NULL,
  municipality = NULL,
  period = NULL,
  ou = NULL,
  unit_type = "municipality",
  max_results = NULL,
  from_date = NULL,
  keep_deleted = FALSE,
  simplify = TRUE,
  cache = FALSE,
  cache_location = NULL,
  verbose = FALSE
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
  available for certain KPIs.

- unit_type:

  One of `"municipality"` or `"ou"`. Whether to fetch data for
  Municipalities or Organizational Units.

- max_results:

  (Optional) Specify the maximum number of results returned by the
  query.

- from_date:

  (Optional) Only return data updated after this date. Format:
  `"YYYY-MM-DD"`.

- keep_deleted:

  Logical. If `FALSE` (default), rows where `isdeleted` is `TRUE` are
  removed from the result.

- simplify:

  Whether to make results more human readable.

- cache:

  Logical. If `TRUE` and `cache_location` points at a SQLite file (or an
  `nxt_handle` from the nordstatExtras package), values are cached at
  cell granularity in that database. Unlike the per-entity `.rds` caches
  used by
  [`get_kpi()`](https://lchansson.github.io/rKolada/reference/get_kpi.md)
  and friends, this path supports concurrent multi-process reads/writes
  and cross-query cell reuse. Requires the `nordstatExtras` package to
  be installed.

- cache_location:

  Either a path to a `.sqlite` file or an `nxt_handle` returned by
  [`nordstatExtras::nxt_open()`](https://rdrr.io/pkg/nordstatExtras/man/nxt_open.html).
  Ignored unless `cache = TRUE`.

- verbose:

  Whether to print the call to the Kolada API as a message to the R
  console.

## Value

A tibble containing Kolada values and metadata.

## Examples

``` r
# Download data for KPIs for Gross Regional Product ("bruttoregionprodukt" in Swedish)
# for three municipalities
if (kolada_available()) {

# If you already know the ID numbers you are looking for,
# you can use these directly as argments.
# Otherwise, use the get_\*() functions in combination with \*_search()
# functions to find good parameter values.
grp_data <- get_values(
  kpi = c("N03700", "N03701"),
  municipality = c("0180", "1480", "1280")
)

# To download OU data instead of Municipality data, set the parameter
# "unit_type" to "ou".
ou_data <- get_values(
 kpi = "N15033",
 ou = "V15E144001101",
 unit_type = "ou"
)
}
```
