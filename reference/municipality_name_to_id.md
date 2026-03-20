# Convert a vector of municipality names to municipality ids

Given a vector of names of municipalities or regions, return a named
vector of municipality IDs/codes. Codes of municipalities and regions
follow the Swedish standard for municipality codes. The codes extracted
can be used e.g. to pass as a parameter to
[`get_values()`](https://lchansson.github.io/rKolada/reference/get_values.md).
This function is the inverse to
[`municipality_id_to_name()`](https://lchansson.github.io/rKolada/reference/municipality_id_to_name.md).

## Usage

``` r
municipality_name_to_id(munic_df, municipality, remove_na = FALSE)
```

## Arguments

- munic_df:

  A Kolada Municipality metadata table, as created by e.g.
  `get_municipality`.

- municipality:

  Name of one or several municipalities. Case insensitive. Allows
  repeats.

- remove_na:

  Should NA return values be removed?

## Value

A vector of Municipality IDs.

## See also

[`municipality_extract_ids()`](https://lchansson.github.io/rKolada/reference/municipality_extract_ids.md),
[`municipality_id_to_name()`](https://lchansson.github.io/rKolada/reference/municipality_id_to_name.md)

## Examples

``` r
if (kolada_available()) {
munic_df <- get_municipality()
munic_df |>
  municipality_name_to_id(c("Arboga", "Lund", "Stockholm", "Arboga"))
}
#>    Arboga      Lund Stockholm    Arboga 
#>    "1984"    "1281"    "0180"    "1984" 
```
