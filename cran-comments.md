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
* local Windows install, R 4.x
* GitHub Actions (ubuntu-latest, R-release)

## R CMD check results
There were no ERRORs or WARNINGs.

## Downstream dependencies
No downstream dependencies at this time.
