# Changelog

## rKolada 0.3.0

### Breaking changes

- **KPI field rename**: The `auspices` column returned by
  [`get_kpi()`](https://lchansson.github.io/rKolada/reference/get_kpi.md)
  is now named `auspice` (matching the v3 API schema).
- **`ou_publication_date` removed**: This field is no longer returned by
  the v3 API and has been removed from
  [`kpi_describe()`](https://lchansson.github.io/rKolada/reference/kpi_describe.md)
  output.
- **`is_divided_by_gender`**: Now returns a logical (`TRUE`/`FALSE`)
  instead of integer (`0`/`1`).
- **`remove_undocumented_columns` removed**:
  [`kpi_minimize()`](https://lchansson.github.io/rKolada/reference/kpi_minimize.md)
  no longer accepts the `remove_undocumented_columns` parameter as the
  API has been updated with proper documentation for all data
  properties.

### New features

- **Kolada API v3**: All API calls now use `https://api.kolada.se/v3/`
  instead of `http://api.kolada.se/v2/`. The v2 API is being shut down
  on March 31, 2026.
- [`get_municipality()`](https://lchansson.github.io/rKolada/reference/get_kpi.md)
  gains a `region_type` parameter to filter by municipality type (e.g.,
  `"K"` for municipalities, `"L"` for regions).
- [`get_values()`](https://lchansson.github.io/rKolada/reference/get_values.md)
  gains a `from_date` parameter to fetch only data updated after a
  specific date.
- [`get_values()`](https://lchansson.github.io/rKolada/reference/get_values.md)
  gains a `keep_deleted` parameter (default `FALSE`). Rows marked as
  deleted in the v3 API are filtered out by default.
- Pagination now uses the v3 `next_url` field and supports up to 5000
  results per page (up from 2000).
- [`kolada_available()`](https://lchansson.github.io/rKolada/reference/kolada_available.md)
  is now more resilient to v3 response shape changes.

### Internal improvements

- **`httr` replaced by `httr2`**: The HTTP backend has been migrated
  from `httr` to `httr2`. This is an internal change with no user-facing
  API differences.
- **`urltools` removed**: No longer a dependency.
- Deprecated
  [`dplyr::filter_at()`](https://dplyr.tidyverse.org/reference/filter_all.html)
  and
  [`dplyr::select_if()`](https://dplyr.tidyverse.org/reference/select_all.html)
  replaced with modern
  [`dplyr::if_any()`](https://dplyr.tidyverse.org/reference/across.html)
  /
  [`dplyr::all_of()`](https://tidyselect.r-lib.org/reference/all_of.html)
  equivalents.
- New internal `entity_search()` helper eliminates code duplication
  across
  [`kpi_search()`](https://lchansson.github.io/rKolada/reference/kpi_search.md),
  [`municipality_search()`](https://lchansson.github.io/rKolada/reference/municipality_search.md),
  and
  [`ou_search()`](https://lchansson.github.io/rKolada/reference/ou_search.md).
- New internal `append_query_params()` helper replaces
  `urltools::param_set()`.
- Deprecated `.data$` pronoun usage in tidyselect contexts replaced with
  string column names.
- Comprehensive test suite added: 140+ tests covering query composers,
  utilities, search functions, KPI/municipality/group operations, mocked
  API responses, and live integration tests.
- Test infrastructure uses testthat edition 3.
- [`get_values()`](https://lchansson.github.io/rKolada/reference/get_values.md)
  and
  [`get_metadata()`](https://lchansson.github.io/rKolada/reference/get_metadata.md)
  now automatically chunk oversized path parameters into batches of 25
  to respect the Kolada v3 API limit. Previously, passing more than 25
  KPIs, municipalities, or periods would return an error. Chunking is
  transparent — results are combined into a single tibble.

### Minor fixes

- Fixes to vignettes: clearer language, correct minor errors.

## rKolada 0.2.3

CRAN release: 2024-05-14

- Add
  [`kolada_available()`](https://lchansson.github.io/rKolada/reference/kolada_available.md)
  for programming with the `rKolada` package
- Fix vignettes and examples so they don’t crash when the Kolada API is
  unavailable

## rKolada 0.2.2

- Correctly print URLs on `verbose = TRUE`
  ([\#6](https://github.com/lchansson/rKolada/issues/6))
- Check if the Kolada API is available using
  [`kolada_available()`](https://lchansson.github.io/rKolada/reference/kolada_available.md)

## rKolada 0.2.1

CRAN release: 2022-03-10

- data downloaders fail gracefully when using bad KPI names

## rKolada 0.2.0

CRAN release: 2021-03-22

- add support for paging. This implies that queries that produce a
  result with more than 5,000 rows are no longer truncated
- vignette polish
- graceful fail on no internet connection (and other HTTP problems)
- add `max_results` argument to all `get_` functions

## rKolada 0.1.3

CRAN release: 2020-06-07

- add option to download OU data to
  [`get_values()`](https://lchansson.github.io/rKolada/reference/get_values.md)
- improved some error messages
- add link to package website

## rKolada 0.1.2

CRAN release: 2020-05-12

- Fixes for CRAN comments

## rKolada 0.1.1

- Fixes for CRAN comments

## rKolada 0.1.0

- Initial release.
