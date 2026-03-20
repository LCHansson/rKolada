## Resubmission

In this version I have:

* Migrated from Kolada API v2 to v3 (v2 is being shut down on March 31, 2026)
* Replaced `httr` with `httr2` and removed `urltools` dependency
* Replaced deprecated dplyr patterns (`filter_at`, `select_if`, `.data$` in tidyselect)
* Removed `magrittr` pipe re-export — uses native `|>` throughout
* Added comprehensive test suite (160+ tests, all offline-safe)
* Added new v3 API features: `region_type`, `from_date`, `keep_deleted` parameters
* `get_values()` and `get_metadata()` now automatically chunk oversized parameters to respect the v3 API's 25-element limit
* Improved vignettes with pedagogical explanations and inline code comments
* All examples and vignettes remain guarded by `kolada_available()` / `eval = FALSE`

## Test environments

- Local: Windows 11, R 4.5.2
- GitHub Actions: ubuntu-latest (R-release, R-devel, R-oldrel-1),
  macOS-latest (R-release), windows-latest (R-release)

## R CMD check results

0 ERRORs | 0 WARNINGs | 1 NOTE

The NOTE is the standard "CRAN incoming feasibility" note for updated packages.

## Downstream dependencies

No downstream dependencies at this time.
