# rKolada

[![R build status](https://github.com/LCHansson/rKolada/workflows/R-CMD-check/badge.svg)](https://github.com/LCHansson/rKolada/actions)

[`rKolada`](https://lchansson.github.io/rKolada/index.html) is an R package for *downloading*, *inspecting* and *processing* data from [Kolada](https://kolada.se/), a Key Performance Indicator database for Swedish municipalities and regions. This vignette provides an overview of the methods included in the `rKolada` package and the design principles of the package API. To learn more about the specifics of functions and to see a full list of the functions included, please see the [Reference section of the package homepage](https://lchansson.github.io/rKolada/reference/index.html) or run `??rKolada`.

rKolada is open source licensed under the Affero Gnu Public License version 3. This means you are free to download the source, modify and redistribute it as you please, but any copies or modifications must retain the original license. Please see the file LICENSE.md for further information.


## Installation

rKolada is on CRAN. To install it, run the following code in R/RStudio:

```r
install.packages("rKolada")
```

To install the development version, use the `remotes` package:

```r
library("remotes")
remotes::install_github("LCHansson/rKolada")
```

## Getting started with rKolada

To get up and running quickly with rKolada, please see the vignette [A quick start guide to rKolada](https://lchansson.github.io/rKolada/articles/quickstart-rkolada.html).

For an introduction to rKolada and the design principles of functions included, please see [Introduction to rKolada](https://lchansson.github.io/rKolada/articles/introduction-to-rkolada.html).

## Contributing

You are welcome to contribute to the further development of the rKolada package in any of the following ways:

- Open an [issue](https://github.com/LCHansson/rKolada/issues)
- Clone this repo, make modifications and create a pull request

### Code of Conduct
  
Please note that the rKolada project is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.