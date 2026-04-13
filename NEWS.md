# rKolada 0.3.2

## New features

- **`values_legend()` gains `lang`, `omit_varname` and `omit_desc`
  arguments**, mirroring the same API in the sibling package `rTrafa`.
  `lang` (default `"SV"`, settable via `getOption("rKolada.lang", "SV")`)
  toggles the source prefix between `"Källa: Kolada"` and `"Source:
  Kolada"`. `omit_varname` drops the `"N01951: "` KPI code prefix;
  `omit_desc` shows only the codes.
- **`values_legend()`'s `kpi_df` argument is now optional.** When
  omitted, the legend falls back to a codes-only listing.
- **Source prefix in plot captions**: `values_legend()` now prepends a
  `"Källa: Kolada"` / `"Source: Kolada"` line to the caption by default.
  This is a minor change to the rendered caption for existing users.
- **Optional SQLite-backed caching via nordstatExtras.** `get_values()`
  and `get_metadata()` now accept `cache = TRUE` with a `.sqlite`
  `cache_location` for shared, multi-process cache backed by the
  [nordstatExtras](https://github.com/LoveHansson/nordstatExtras) package.
  Cell-level deduplication, cross-query freshness, and FTS5-powered
  typeahead search. Falls back to the existing `.rds` cache when
  nordstatExtras is not installed.

## Documentation

- Vignette plots now convert `year` to `Date` before plotting and use
  `scale_x_date(date_breaks = "…", date_labels = "%Y")`, so axis breaks
  land on whole years rather than on decimal years like `2020, 2022.5,
  2025`. This pattern is explained inline in both vignettes and is
  consistent across the sibling packages `pixieweb` and `rTrafa`.
- README and vignettes now cross-link to the sibling packages `pixieweb`
  (PX-Web APIs) and `rTrafa` (Trafa transport statistics).
- README now includes a section on enhanced caching with nordstatExtras.

# rKolada 0.3.1

## Breaking changes

- **`magrittr` pipe removed**: The `%>%` pipe is no longer re-exported. Use the native R pipe `|>` instead (available since R 4.1.0, which is already the minimum version for rKolada). If you rely on `%>%` in your own code, add `library(magrittr)` or switch to `|>`.

## New features

- **`kolada_cache_dir()`**: New exported function returning the persistent cache directory path (using `tools::R_user_dir()`).
- **`kolada_clear_cache()`**: New exported function for clearing cached files.
- **Lightweight `kolada_available()`**: Now performs a fast HTTP check (single request, 5s timeout) instead of downloading actual data. Much faster in examples and tests.
- **HTTP retry with backoff**: API requests now automatically retry on HTTP 429 (rate limiting) with exponential backoff, up to 3 retries.

## Documentation

- Improved quickstart vignette: explains the three-dimension data model, enriched metadata entity descriptions, clearer `simplify = TRUE` explanation, inline ggplot comments, and "K"-filter explanations.
- Improved introduction vignette: prominent Swedish-only note, inline ggplot comments, clearer groups/unnest explanation, and ggplot2 reference link.

## Internal improvements

- **`glue` replaced by `cli`**: All string interpolation now uses `cli` for user-facing messages and `paste0()` for URL construction. `glue` is no longer a dependency.
- **`magrittr` removed**: All internal code uses the native pipe `|>`. `magrittr` is no longer a dependency.
- **Structured messaging**: `message()`, `warning()`, and `stop()` replaced with `cli::cli_inform()`, `cli::cli_warn()`, and `cli::cli_abort()` throughout.
- **Roxygen2 markdown**: Documentation now uses markdown syntax (e.g., `[func()]` instead of `\code{\link{func}}`).
- **`kolada_get()` internal helper**: Centralised HTTP request logic with `tryCatch()`, timeout, and retry.
- Removed legacy `CRAN-RELEASE` file.
- Removed legacy `test_api.R` test stub.

# rKolada 0.3.0


## Breaking changes

- **KPI field rename**: The `auspices` column returned by `get_kpi()` is now named `auspice` (matching the v3 API schema).
- **`ou_publication_date` removed**: This field is no longer returned by the v3 API and has been removed from `kpi_describe()` output.
- **`is_divided_by_gender`**: Now returns a logical (`TRUE`/`FALSE`) instead of integer (`0`/`1`).
- **`remove_undocumented_columns` removed**: `kpi_minimize()` no longer accepts the `remove_undocumented_columns` parameter as the API has been updated with proper documentation for all data properties.

## New features

- **Kolada API v3**: All API calls now use `https://api.kolada.se/v3/` instead of `http://api.kolada.se/v2/`. The v2 API is being shut down on March 31, 2026.
- `get_municipality()` gains a `region_type` parameter to filter by municipality type (e.g., `"K"` for municipalities, `"L"` for regions).
- `get_values()` gains a `from_date` parameter to fetch only data updated after a specific date.
- `get_values()` gains a `keep_deleted` parameter (default `FALSE`). Rows marked as deleted in the v3 API are filtered out by default.
- Pagination now uses the v3 `next_url` field and supports up to 5000 results per page (up from 2000).
- `kolada_available()` is now more resilient to v3 response shape changes.

## Internal improvements

- **`httr` replaced by `httr2`**: The HTTP backend has been migrated from `httr` to `httr2`. This is an internal change with no user-facing API differences.
- **`urltools` removed**: No longer a dependency.
- Deprecated `dplyr::filter_at()` and `dplyr::select_if()` replaced with modern `dplyr::if_any()` / `dplyr::all_of()` equivalents.
- New internal `entity_search()` helper eliminates code duplication across `kpi_search()`, `municipality_search()`, and `ou_search()`.
- New internal `append_query_params()` helper replaces `urltools::param_set()`.
- Deprecated `.data$` pronoun usage in tidyselect contexts replaced with string column names.
- Comprehensive test suite added: 140+ tests covering query composers, utilities, search functions, KPI/municipality/group operations, mocked API responses, and live integration tests.
- Test infrastructure uses testthat edition 3.
- `get_values()` and `get_metadata()` now automatically chunk oversized path parameters into batches of 25 to respect the Kolada v3 API limit. Previously, passing more than 25 KPIs, municipalities, or periods would return an error. Chunking is transparent — results are combined into a single tibble.

## Minor fixes

- Fixes to vignettes: clearer language, correct minor errors.

# rKolada 0.2.3

- Add `kolada_available()` for programming with the `rKolada` package
- Fix vignettes and examples so they don't crash when the Kolada API is unavailable

# rKolada 0.2.2

- Correctly print URLs on `verbose = TRUE` (#6)
- Check if the Kolada API is available using `kolada_available()`

# rKolada 0.2.1

- data downloaders fail gracefully when using bad KPI names

# rKolada 0.2.0

- add support for paging. This implies that queries that produce a result with more than 5,000 rows are no longer truncated
- vignette polish
- graceful fail on no internet connection (and other HTTP problems)
- add `max_results` argument to all `get_` functions

# rKolada 0.1.3

- add option to download OU data to `get_values()`
- improved some error messages
- add link to package website

# rKolada 0.1.2

- Fixes for CRAN comments

# rKolada 0.1.1

- Fixes for CRAN comments

# rKolada 0.1.0

- Initial release.