## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

- Local: Windows 11, R 4.5.2
- GitHub Actions: ubuntu-latest (R-release, R-devel, R-oldrel-1),
  macOS-latest (R-release), windows-latest (R-release)

## Changes since 0.3.1

- `values_legend()` gains `lang`, `omit_varname`, `omit_desc` arguments
  and an optional `kpi_df` parameter.
- Optional SQLite-backed caching via nordstatExtras (in Suggests, available
  on GitHub at https://github.com/LoveHansson/nordstatExtras). All
  integration points use `requireNamespace("nordstatExtras", quietly = TRUE)`
  with graceful fallback to standard `.rds` file caching. No functionality
  is lost without it.

## Downstream dependencies

No downstream dependencies at this time.
