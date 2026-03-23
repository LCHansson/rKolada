## Resubmission

In this version (0.3.1) I have:

* Removed `magrittr` pipe re-export — uses native `|>` throughout
* Added `kolada_cache_dir()` and `kolada_clear_cache()` for cache management
* Replaced `glue` with `cli` for all user-facing messages
* Made `kolada_available()` faster (single lightweight HTTP check)
* Added HTTP retry with exponential backoff for rate limiting
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
