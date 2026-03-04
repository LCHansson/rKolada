# Create a municipality table from a Kolada Municipality Group metadata table

Municipality groups are a convenient way to discover pre-rendered sets
of municipalities. A practical workflow for discovering such sets can be
to search through Municipality Group metadata using
[`municipality_grp_search`](https://lchansson.github.io/rKolada/reference/municipality_grp_search.md)
to search for keywords and
[`municipality_grp_describe`](https://lchansson.github.io/rKolada/reference/municipality_grp_describe.md)
to inspect contents of KPI groups. Once you have created a Municipality
Group metadata table that has been narrowed down to the group/s you are
looking for, `municipality_grp_unnest` is used to create a municipality
metadata table for further processing.

## Usage

``` r
municipality_grp_unnest(munic_grp_df)
```

## Arguments

- munic_grp_df:

  A Kolada Municipality Group metadata table, as created by e.g.
  `get_municipality_groups`.

## Value

A Kolada Municipality metadata table

## Examples

``` r
if (kolada_available()) {
# Download Municipality Group metadata
# (skip the parameter "max_results" to actually download all available data)
munic_grp_df <- get_municipality_groups(max_results = 100)

# Create a Municipality metadata table from municipality groups matching the
# term "Arboga"
munic_grp_df %>%
  municipality_grp_search("arboga") %>%
  municipality_grp_unnest()
}
#> # A tibble: 7 × 4
#>   id    title     group_id group_title                                       
#>   <chr> <chr>     <chr>    <chr>                                             
#> 1 0380  Uppsala   G175909  Liknande kommuner ekonomiskt bistånd, Arboga, 2023
#> 2 1472  Tibro     G175909  Liknande kommuner ekonomiskt bistånd, Arboga, 2023
#> 3 1473  Töreboda  G175909  Liknande kommuner ekonomiskt bistånd, Arboga, 2023
#> 4 1490  Borås     G175909  Liknande kommuner ekonomiskt bistånd, Arboga, 2023
#> 5 1499  Falköping G175909  Liknande kommuner ekonomiskt bistånd, Arboga, 2023
#> 6 2034  Orsa      G175909  Liknande kommuner ekonomiskt bistånd, Arboga, 2023
#> 7 2583  Haparanda G175909  Liknande kommuner ekonomiskt bistånd, Arboga, 2023
```
