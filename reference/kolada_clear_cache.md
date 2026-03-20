# Clear the rKolada cache

Removes all cached files from the rKolada cache directory.

## Usage

``` r
kolada_clear_cache(cache_dir = kolada_cache_dir())
```

## Arguments

- cache_dir:

  The cache directory to clear. Defaults to
  [`kolada_cache_dir()`](https://lchansson.github.io/rKolada/reference/kolada_cache_dir.md).

## Value

Invisibly returns the cache directory path.
