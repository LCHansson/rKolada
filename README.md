# rKolada

[`rKolada`](https://lchansson.github.io/rKolada/index.html) is an R package for *downloading*, *inspecting* and *processing* data from [Kolada](https://kolada.se/), an open database containing over 4,000 Key Performance Indicators for Swedish municipalities and regions.

To learn more about using `rKolada`, it is recommended you use the following resources in order:

## Getting started with rKolada

1.  To get up and running quickly with rKolada, please see the vignette [A quick start guide to rKolada](https://lchansson.github.io/rKolada/articles/a-quickstart-rkolada.html).
2.  For an introduction to rKolada and the design principles of functions included, please see [Introduction to rKolada](https://lchansson.github.io/rKolada/articles/introduction-to-rkolada.html).
3.  See the [Reference section of the package homepage](https://lchansson.github.io/rKolada/reference/index.html) to learn about the full set of functionality included with the package.

`rKolada` is open source licensed under the Affero Gnu Public License version 3. This means you are free to download the source, modify and redistribute it as you please, but any copies or modifications must retain the original license. Please see the file LICENSE.md for further information.

## Installation

rKolada is on CRAN. To install it, run the following code in R:

``` r
install.packages("rKolada")
```

To install the latest development version from GitHub, use the `remotes` package:

``` r
library("remotes")
remotes::install_github("LCHansson/rKolada")
```

## Enhanced caching with nordstatExtras

For multi-user web applications or workflows that benefit from a shared,
persistent cache, rKolada integrates with the
[nordstatExtras](https://github.com/LoveHansson/nordstatExtras) package.
When installed, `get_values()`, `get_kpi()`, and other functions can
write to a shared SQLite file instead of per-session `.rds` files:

```r
# install.packages("devtools")
devtools::install_github("LoveHansson/nordstatExtras")

library(nordstatExtras)
handle <- nxt_open("cache.sqlite")

# Data and metadata are cached in the same SQLite file
kpis <- get_kpi(cache = TRUE, cache_location = handle)
vals <- get_values(
  kpi = "N03700", municipality = c("0180", "1480"),
  cache = TRUE, cache_location = handle
)

nxt_close(handle)
```

Features include cell-level deduplication across overlapping queries,
cross-query freshness propagation, and FTS5-powered typeahead search
via `nxt_search()`. See the
[nordstatExtras README](https://github.com/LoveHansson/nordstatExtras)
for details.

## Contributing

You are welcome to contribute to the further development of the rKolada package in any of the following ways:

-   Open an [issue](https://github.com/LCHansson/rKolada/issues)
-   Clone this repo, make modifications and create a pull request
-   Spread the word!

## Related packages

`rKolada` is part of a family of R packages for Swedish and Nordic open statistics that share the same design philosophy — tibble-based, pipe-friendly, and offline-safe:

- [pixieweb](https://lchansson.github.io/pixieweb/) — R client for PX-Web APIs (Statistics Sweden, Statistics Norway, Statistics Finland, and more)
- [rTrafa](https://lchansson.github.io/rTrafa/) — R client for the [Trafa](https://api.trafa.se/) API of Swedish transport statistics

### Code of Conduct

Please note that the rKolada project is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.
