## Resubmission
In this version I have:

* Migrated from Kolada API v2 to v3 (v2 is being shut down on March 31, 2026)
* Replaced `httr` with `httr2` and removed `urltools` dependency
* Replaced deprecated dplyr patterns (`filter_at`, `select_if`, `.data$` in tidyselect)
* Added comprehensive test suite (160+ tests, all offline-safe)
* Added new v3 API features: `region_type`, `from_date`, `keep_deleted` parameters
* `get_values()` and `get_metadata()` now automatically chunk oversized parameters to respect the v3 API's 25-element limit
* All examples and vignettes remain guarded by `kolada_available()` / `eval = FALSE`
* Removed `LazyData: TRUE` (no `data/` directory)
* Removed unused `httptest2` from Suggests

## Test environments
* GitHub Actions: macOS (latest, R-release)
* GitHub Actions: Windows (latest, R-release)
* GitHub Actions: Ubuntu (latest, R-release)
* GitHub Actions: Ubuntu (latest, R-devel)
* GitHub Actions: Ubuntu (latest, R-oldrel-1)

## R CMD check results
0 ERRORs | 0 WARNINGs | 0 NOTEs on all five platforms.

## Downstream dependencies
No downstream dependencies at this time.
