# Check if the Kolada API is available

Performs a lightweight HTTP check to verify that the Kolada API is
reachable. This is primarily useful for guarding examples and tests.

## Usage

``` r
kolada_available()
```

## Value

`TRUE` if the API responds within 5 seconds, `FALSE` otherwise.
