# Convert a vector of municipality ids to municipality names

Given a vector of municipality IDs/codes, return a named vector of names
of municipalities or regions. Codes of municipalities and regions follow
the Swedish standard for municipality codes. The codes extracted can be
used e.g. to pass as a parameter to
[`get_values`](https://lchansson.github.io/rKolada/reference/get_values.md).
This function is the inverse to
[`municipality_name_to_id`](https://lchansson.github.io/rKolada/reference/municipality_name_to_id.md).

## Usage

``` r
municipality_id_to_name(munic_df, id, remove_na = FALSE)
```

## Arguments

- munic_df:

  A Kolada Municipality metadata table, as created by e.g.
  `get_municipality`.

- id:

  ID ids of one or several municipalities. Allows repeats.

- remove_na:

  Should NA return values be removed?

## Value

A vector of Municipality names.

## See also

[`municipality_extract_ids`](https://lchansson.github.io/rKolada/reference/municipality_extract_ids.md),
[`municipality_name_to_id`](https://lchansson.github.io/rKolada/reference/municipality_name_to_id.md)

## Examples

``` r
if (kolada_available()) {
munic_df <- get_municipality()
municipality_id_to_name(munic_df, c("1280", "1281", "0180", "1280"))
}
#>        1280        1281        0180        1280 
#>     "Malmö"      "Lund" "Stockholm"     "Malmö" 
```
