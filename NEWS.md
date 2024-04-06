# rKolada 0.2.3

- Add `kolada_available()` for programming with the `rKolada` package
- Fix vignettes and examples so they don't crash when the Kolada API is unavailable

# rKolada 0.2.2

- Correctly print URLs on `verbose = TRUE` (#6)
- Check if the Kolada API is availoable using `kolada_available()`

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