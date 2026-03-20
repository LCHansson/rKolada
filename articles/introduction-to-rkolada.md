# Introduction to rKolada

`rKolada` is an R package for *downloading*, *inspecting* and
*processing* data from [Kolada](https://kolada.se/), a Key Performance
Indicator database for Swedish municipalities and regions. This vignette
provides an overview of the methods included in the `rKolada` package
and the design principles of the package API. To learn more about the
specifics of functions and to see a full list of the functions included,
please see the [Reference section of the package
homepage](https://lchansson.github.io/rKolada/reference/index.html) or
run `??rKolada`. For a quick introduction to the package see the
vignette [A quick start guide to
rKolada](https://lchansson.github.io/rKolada/articles/a-quickstart-rkolada.md).

> **Important:** All metadata and data labels in Kolada are in Swedish
> only. Function names and parameters are in English, but KPI titles,
> municipality names, and descriptions will be in Swedish.

The design of `rKolada` functions is inspired by the design and
functionality provided by several packages in the `tidyverse` family.
rKolada uses the base R pipe (`|>`) throughout. Some vignette examples
use `dplyr`, `tidyr`, and `ggplot2` for data wrangling and
visualisation:

``` r
install.packages("rKolada")
```

## Kolada, a Key Performance Indicator database for Swedish municipalities and regions

The Swedish Municipalities and Regions Database
[Kolada](https://kolada.se/) is a openly accessible, comprehensive
database containing over 4,000 Key Performance Indicators (KPIs) for a
vast number of aspects of municipal and regional organisations, politics
and economic life. The `rKolada` R package provides an interface to R
users to directly download, explore, and simplify metadata and data from
Kolada.

To get started with Kolada you might want to visit its homepage
(Swedish-only) or read through the [REST API documentation on
Github](https://github.com/Hypergene/kolada). However, you can also use
the `rKolada` package to explore data without prior knowledge of the
database.

``` r
library("rKolada")
```

### The data model

Data in Kolada are stored along three basic *dimensions*:

- A KPI ID
- A point in time (year)
- A municipality/region/ ID

When downloading data, the user needs to specify search parameters for
at least two of these three basic dimensions. (The Kolada API
documentation also specifies a fourth basic dimension: *gender*.
However, data for all genders is always automatically downloaded when
available.) The parameters can be a single, atomic value or a vector of
values.

Also, the Kolada database proves useful groupings of municipalities and
KPIs that can be used for further exploration, or to create unweighted
averages. Lastly, some KPIs are also available for *Organizational
units* (OUs) within municipalities, e.g. a school, an administrative
subdivision or an elderly home.

### Downloading data

If the user already has knowledge of the IDs of the KPIs and/or
municipalities they want to download, this can be done using the
function
[`get_values()`](https://lchansson.github.io/rKolada/reference/get_values.md).
For instance, if you want to download all values for the KPI `N00945`
(“Tillfälliga föräldrapenningdagar (VAB) som tas ut av män, andel av
antal dagar (%)”) for Sweden’s three most populous cities; Stockholm (id
`"0180"`), Gothenburg (Swedish: *Göteborg*; `"1480"`) and Malmö
(`"1280"`):

    #>    gender count status    value    kpi year municipality_id municipality
    #> 1       T     0              NA N00945 1996            0180    Stockholm
    #> 2       T     0              NA N00945 1996            1280        Malmö
    #> 3       T     0              NA N00945 1996            1480     Göteborg
    #> 4       T     0              NA N00945 1997            0180    Stockholm
    #> 5       T     0              NA N00945 1997            1280        Malmö
    #> 6       T     0              NA N00945 1997            1480     Göteborg
    #> 7       T     1        30.41209 N00945 1998            0180    Stockholm
    #> 8       T     1        26.69198 N00945 1998            1280        Malmö
    #> 9       T     1        30.45307 N00945 1998            1480     Göteborg
    #> 10      T     1        30.62354 N00945 1999            0180    Stockholm
    #> 11      T     1        26.84941 N00945 1999            1280        Malmö
    #> 12      T     1        32.00700 N00945 1999            1480     Göteborg
    #> 13      T     1        31.32960 N00945 2000            0180    Stockholm
    #> 14      T     1        28.36723 N00945 2000            1280        Malmö
    #> 15      T     1        33.23952 N00945 2000            1480     Göteborg
    #> 16      T     1        31.90901 N00945 2001            0180    Stockholm
    #> 17      T     1        29.14839 N00945 2001            1280        Malmö
    #> 18      T     1        34.34883 N00945 2001            1480     Göteborg
    #> 19      T     1        33.13784 N00945 2002            0180    Stockholm
    #> 20      T     1        30.18228 N00945 2002            1280        Malmö
    #> 21      T     1        35.15515 N00945 2002            1480     Göteborg
    #> 22      T     1        33.82520 N00945 2003            0180    Stockholm
    #> 23      T     1        29.88630 N00945 2003            1280        Malmö
    #> 24      T     1        35.55795 N00945 2003            1480     Göteborg
    #> 25      T     1        34.21939 N00945 2004            0180    Stockholm
    #> 26      T     1        31.21299 N00945 2004            1280        Malmö
    #> 27      T     1        35.51597 N00945 2004            1480     Göteborg
    #> 28      T     1        35.00610 N00945 2005            0180    Stockholm
    #> 29      T     1        32.26879 N00945 2005            1280        Malmö
    #> 30      T     1        36.71541 N00945 2005            1480     Göteborg
    #> 31      T     1        35.78205 N00945 2006            0180    Stockholm
    #> 32      T     1        33.89914 N00945 2006            1280        Malmö
    #> 33      T     1        37.20446 N00945 2006            1480     Göteborg
    #> 34      T     1        35.61412 N00945 2007            0180    Stockholm
    #> 35      T     1        32.27672 N00945 2007            1280        Malmö
    #> 36      T     1        36.03724 N00945 2007            1480     Göteborg
    #> 37      T     1        36.34371 N00945 2008            0180    Stockholm
    #> 38      T     1        32.99687 N00945 2008            1280        Malmö
    #> 39      T     1        36.04195 N00945 2008            1480     Göteborg
    #> 40      T     1        36.59765 N00945 2009            0180    Stockholm
    #> 41      T     1        33.88994 N00945 2009            1280        Malmö
    #> 42      T     1        35.93543 N00945 2009            1480     Göteborg
    #> 43      T     1        36.92186 N00945 2010            0180    Stockholm
    #> 44      T     1        33.82857 N00945 2010            1280        Malmö
    #> 45      T     1        36.39835 N00945 2010            1480     Göteborg
    #> 46      T     1        37.04921 N00945 2011            0180    Stockholm
    #> 47      T     1        33.20278 N00945 2011            1280        Malmö
    #> 48      T     1        37.00446 N00945 2011            1480     Göteborg
    #> 49      T     1        37.04136 N00945 2012            0180    Stockholm
    #> 50      T     1        34.31265 N00945 2012            1280        Malmö
    #> 51      T     1        36.87452 N00945 2012            1480     Göteborg
    #> 52      T     1        38.35227 N00945 2013            0180    Stockholm
    #> 53      T     1        34.82378 N00945 2013            1280        Malmö
    #> 54      T     1        37.95666 N00945 2013            1480     Göteborg
    #> 55      T     1        38.61051 N00945 2014            0180    Stockholm
    #> 56      T     1        35.30861 N00945 2014            1280        Malmö
    #> 57      T     1        38.12922 N00945 2014            1480     Göteborg
    #> 58      T     1        39.09000 N00945 2015            0180    Stockholm
    #> 59      T     1        35.78000 N00945 2015            1280        Malmö
    #> 60      T     1        38.19000 N00945 2015            1480     Göteborg
    #> 61      T     1        38.79194 N00945 2016            0180    Stockholm
    #> 62      T     1        36.26382 N00945 2016            1280        Malmö
    #> 63      T     1        38.60823 N00945 2016            1480     Göteborg
    #> 64      T     1        38.93364 N00945 2017            0180    Stockholm
    #> 65      T     1        35.68866 N00945 2017            1280        Malmö
    #> 66      T     1        38.86796 N00945 2017            1480     Göteborg
    #> 67      T     1        39.40000 N00945 2018            0180    Stockholm
    #> 68      T     1        36.50000 N00945 2018            1280        Malmö
    #> 69      T     1        39.10000 N00945 2018            1480     Göteborg
    #> 70      T     1        39.30000 N00945 2019            0180    Stockholm
    #> 71      T     1        35.40000 N00945 2019            1280        Malmö
    #> 72      T     1        39.10000 N00945 2019            1480     Göteborg
    #> 73      T     1        40.80000 N00945 2020            0180    Stockholm
    #> 74      T     1        37.00000 N00945 2020            1280        Malmö
    #> 75      T     1        39.50000 N00945 2020            1480     Göteborg
    #>    municipality_type
    #> 1                  K
    #> 2                  K
    #> 3                  K
    #> 4                  K
    #> 5                  K
    #> 6                  K
    #> 7                  K
    #> 8                  K
    #> 9                  K
    #> 10                 K
    #> 11                 K
    #> 12                 K
    #> 13                 K
    #> 14                 K
    #> 15                 K
    #> 16                 K
    #> 17                 K
    #> 18                 K
    #> 19                 K
    #> 20                 K
    #> 21                 K
    #> 22                 K
    #> 23                 K
    #> 24                 K
    #> 25                 K
    #> 26                 K
    #> 27                 K
    #> 28                 K
    #> 29                 K
    #> 30                 K
    #> 31                 K
    #> 32                 K
    #> 33                 K
    #> 34                 K
    #> 35                 K
    #> 36                 K
    #> 37                 K
    #> 38                 K
    #> 39                 K
    #> 40                 K
    #> 41                 K
    #> 42                 K
    #> 43                 K
    #> 44                 K
    #> 45                 K
    #> 46                 K
    #> 47                 K
    #> 48                 K
    #> 49                 K
    #> 50                 K
    #> 51                 K
    #> 52                 K
    #> 53                 K
    #> 54                 K
    #> 55                 K
    #> 56                 K
    #> 57                 K
    #> 58                 K
    #> 59                 K
    #> 60                 K
    #> 61                 K
    #> 62                 K
    #> 63                 K
    #> 64                 K
    #> 65                 K
    #> 66                 K
    #> 67                 K
    #> 68                 K
    #> 69                 K
    #> 70                 K
    #> 71                 K
    #> 72                 K
    #> 73                 K
    #> 74                 K
    #> 75                 K

``` r
n00945 <- get_values(
  kpi = "N00945",
  municipality = c("0180", "1480", "1280"),
  period = 1970:2020
)

n00945
```

In many cases, however, you will not know in advance exactly what KPIs
to be looking for, or you might not know the IDs of Sweden’s
municipalities.

### Downloading metadata: `get` functions

Kolada has five different kinds of metadata entities Each one of these
can be downloaded by using `rKolada`’s `get` functions. Each function
returns a `tibble` with all available data for the specified metadata
entity:

- KPIs:
  [`get_kpi()`](https://lchansson.github.io/rKolada/reference/get_kpi.md)
- KPI groups:
  [`get_kpi_groups()`](https://lchansson.github.io/rKolada/reference/get_kpi.md)
- Municipalities:
  [`get_municipality()`](https://lchansson.github.io/rKolada/reference/get_kpi.md)
- Municipality groups:
  [`get_municipality_groups()`](https://lchansson.github.io/rKolada/reference/get_kpi.md)
- Organizational Unit: :
  [`get_ou()`](https://lchansson.github.io/rKolada/reference/get_kpi.md)

Each function returns a `tibble` with all available data for the
specified metadata entity.

    #>        id                                               title
    #> 1  N00003                           Personalkostnader, kr/inv
    #> 2  N00005      Utjämningssystemet enl resultaträkning, kr/inv
    #> 3  N00009                    Intäkter kommunen totalt, kr/inv
    #> 4  N00011 Inkomstutjämning, bidrag/avgift, kr/inv 1 nov fg år
    #> 5  N00012            Kostnadsutjämning, bidrag/avgift, kr/inv
    #> 6  N00014                     Regleringsbidrag/avgift, kr/inv
    #> 7  N00016                  Utjämningssystemet enl SCB, kr/inv
    #> 8  N00018                             Införandebidrag, kr/inv
    #> 9  N00019                              Strukturbidrag, kr/inv
    #> 10 N00021                Intäkter egentlig verksamhet, kr/inv
    #>                                                                                                                                                       description
    #> 1                                                      Personalkostnader kommunen totalt, dividerat med antal invånare totalt 31/12. Avser egen regi. Källa: SCB.
    #> 2                                                                     Kommunalekonomisk utjämning kommun, dividerat med antal invånare totalt 31/12 . Källa: SCB.
    #> 3      Externa intäkter exklusive intäkter från försäljning till andra kommuner och regioner för kommunen totalt, dividerat med antal invånare 31/12. Källa: SCB.
    #> 4                                                                              Inkomstutjämning, bidrag/avgift, i kronor per invånare den 1/11 fg år. Källa: SCB.
    #> 5                                                                         Kostnadsutjämning, bidrag/avgift, i kronor per invånare den 1/11 nov fg år. Källa: SCB.
    #> 6                                                                                      Regleringsbidrag/avgift, i kronor per invånare den 1/11 fg år. Källa: SCB.
    #> 7                                                                                                                 Utjämningssystemet enl SCB, kr/inv. Källa: SCB.
    #> 8                                                                                              Införandebidrag, i kronor per invånare den 1/11 fg år. Källa: SCB.
    #> 9                                                                                               Strukturbidrag, i kronor per invånare den 1/11 fg år. Källa: SCB.
    #> 10 Externa intäkter exklusive intäkter från försäljning till andra kommuner och regioner för egentlig verksamhet, dividerat med antal invånare 31/12. Källa: SCB.
    #>    is_divided_by_gender municipality_type auspice         operating_area
    #> 1                 FALSE                 K       E Kommunen, övergripande
    #> 2                 FALSE                 K       X Kommunen, övergripande
    #> 3                 FALSE                 K    <NA> Kommunen, övergripande
    #> 4                 FALSE                 K    <NA> Kommunen, övergripande
    #> 5                 FALSE                 K    <NA> Kommunen, övergripande
    #> 6                 FALSE                 K       X Kommunen, övergripande
    #> 7                 FALSE                 K    <NA> Kommunen, övergripande
    #> 8                 FALSE                 K       X Kommunen, övergripande
    #> 9                 FALSE                 K       X Kommunen, övergripande
    #> 10                FALSE                 K    <NA> Kommunen, övergripande
    #>    perspective prel_publication_date publication_date publ_period has_ou_data
    #> 1     Resurser            2026-04-01       2026-08-28        2025       FALSE
    #> 2     Resurser            2026-04-01       2026-08-28        2025       FALSE
    #> 3     Resurser                  <NA>       2027-02-24        2026       FALSE
    #> 4     Resurser            2025-09-28       2026-04-15        2026       FALSE
    #> 5     Resurser            2025-09-28       2026-04-15        2026       FALSE
    #> 6     Resurser            2025-09-28       2026-04-15        2026       FALSE
    #> 7     Resurser            2025-09-28       2026-04-15        2026       FALSE
    #> 8     Resurser            2025-09-28       2026-04-15        2026       FALSE
    #> 9     Resurser            2025-09-28       2026-04-15        2026       FALSE
    #> 10    Resurser            2026-04-01       2026-08-28        2025       FALSE

``` r
# Download all KPI metadata as a tibble (kpi_df)
kpi_df <- get_kpi()

head(kpi_df, n = 10)
```

All `get` functions are thin wrappers around the more general function
[`get_metadata()`](https://lchansson.github.io/rKolada/reference/get_metadata.md).
If you are familiar with the terminology used in the Kolada API for
accessing metadata you might want to use this function instead.

### Exploring metadata

For each metadata type mentioned in the previous sections, `rKolada`
offers several convenience functions to help exploring and narrowing
down metadata tables. (If you are familiar with `dplyr` semantics, most
of these functions are basically wrappers around `dplyr`/`tidyr` code.)

Since each `get` function above returns a table for the selected entity,
a metadata table can be one of five different types. All metadata
convenience functions are prefixed to reflect which kind of metadata
table they operate on: `kpi`, `kpi_grp`, `municipality`,
`municipality_grp`, and `ou`.

All metadata convenience functions have been designed with piping in
mind, so their first argument is always a metadata tibble. Most of them
also return a tibble of the same type.

The most important family of metadata convenience functions is the
`search` family. Much like
[`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html)
they can be used to search for one or several search terms in the entire
table or in a subset of named columns:

``` r
# Search for KPIs with the term "skola" in their description or title
kpi_filter <- kpi_df |> kpi_search("skola", column = c("description", "title"))
kpi_filter
#> # A tibble: 846 × 12
#>    id     title       description is_divided_by_gender municipality_type auspice
#>    <chr>  <chr>       <chr>       <lgl>                <chr>             <chr>  
#>  1 N00022 Kostnadsut… "Kostnadsu… FALSE                K                 NA     
#>  2 N00023 Kostnadsut… "Kostnadsu… FALSE                K                 NA     
#>  3 N00026 Kostnadsut… "Kostnadsu… FALSE                K                 NA     
#>  4 N00097 Nettokostn… "Avvikelse… FALSE                K                 T      
#>  5 N00100 Strukturko… "Strukturk… FALSE                K                 T      
#>  6 N00108 Månadsavlö… "Antal ans… FALSE                K                 E      
#>  7 N00114 Årsarbetar… "Antal års… FALSE                K                 E      
#>  8 N00303 Tillgängli… "Kommunind… FALSE                K                 T      
#>  9 N00314 Tillgängli… "Kvinnors … FALSE                K                 T      
#> 10 N00325 Tillgängli… "Mäns Komm… FALSE                K                 T      
#> # ℹ 836 more rows
#> # ℹ 6 more variables: operating_area <chr>, perspective <chr>,
#> #   prel_publication_date <chr>, publication_date <chr>, publ_period <chr>,
#> #   has_ou_data <lgl>
```

    #> # A tibble: 3,630 × 3
    #>    id      title                                                 members      
    #>    <chr>   <chr>                                                 <list>       
    #>  1 G114418 Fyrkommunsnätverket (ovägt medel)                     <df [4 × 2]> 
    #>  2 G114419 SMS - Samhällsskydd mellersta Skaraborg (ovägt medel) <df [4 × 2]> 
    #>  3 G114798 4M -  Fyra Mälarstäder (ovägt medel)                  <df [4 × 2]> 
    #>  4 G114915 SmåKom (ovägt medel)                                  <df [68 × 2]>
    #>  5 G116237 Sjuhärad kommunalförbund (ovägt medel)                <df [9 × 2]> 
    #>  6 G116238 Skaraborgs kommunalförbund (ovägt medel)              <df [15 × 2]>
    #>  7 G122469 Region 10 (ovägt medel)                               <df [10 × 2]>
    #>  8 G122931 3KVH (ovägt medel)                                    <df [5 × 2]> 
    #>  9 G128518 Familjen Helsingborg (ovägt medel)                    <df [11 × 2]>
    #> 10 G128681 Stor-Malmö (ovägt medel)                              <df [12 × 2]>
    #> # ℹ 3,620 more rows

``` r
# Search for municipality groups containing the name "Arboga"
munic_g <- get_municipality_groups()
```

``` r
arboga_groups <- munic_g |> municipality_grp_search("Arboga")
arboga_groups
#> # A tibble: 12 × 3
#>    id      title                                              members     
#>    <chr>   <chr>                                              <list>      
#>  1 G175909 Liknande kommuner ekonomiskt bistånd, Arboga, 2023 <df [7 × 2]>
#>  2 G176199 Liknande kommuner socioekonomi, Arboga, 2024       <df [7 × 2]>
#>  3 G176489 Liknande kommuner äldreomsorg, Arboga, 2024        <df [7 × 2]>
#>  4 G216099 Liknande kommuner arbetsmarknad, Arboga, 2024      <df [7 × 2]>
#>  5 G219042 Liknande kommuner räddningstjänst, Arboga, 2024    <df [7 × 2]>
#>  6 G35869  Liknande kommuner grundskola, Arboga, 2024         <df [7 × 2]>
#>  7 G36161  Liknande kommuner gymnasieskola, Arboga, 2024      <df [7 × 2]>
#>  8 G36453  Liknande kommuner IFO, Arboga, 2024                <df [7 × 2]>
#>  9 G37329  Liknande kommuner, övergripande, Arboga, 2024      <df [7 × 2]>
#> 10 G39502  Liknande kommuner LSS, Arboga, 2024                <df [7 × 2]>
#> 11 G85463  Liknande kommuner fritidshem, Arboga, 2024         <df [7 × 2]>
#> 12 G85755  Liknande kommuner förskola, Arboga, 2024           <df [7 × 2]>
```

Another important family of exploration functions is the `describe`
family of functions. These functions take a metadata table and print a
human-readable summary of the most important facts about each row in the
table (up to a limit, specified by `max_n`). By default, output is
printed directly to the R console. But by specifying `format = "md"` you
can make the `describe` functions create markdown-ready output which can
be added directly to a R markdown file by setting the chunk option
`results='asis'`. The output then looks as follows:

``` r
kpi_filter |> kpi_describe(max_n = 2, format = "md", heading_level = 4)
```

#### N00022: Kostnadsutjämningsnetto förskola och skolbarnsomsorg, kr/inv 1 nov fg år

##### Description

Kostnadsutjämningsnetto förskola och skolbarnomsorg beräknas som
kommunens standardkostnad minus standardkostnaden för riket i tkr
dividerat med antal invånare totalt 1/11 nov fg år. Källa: SCB.

##### Metadata

- Has OU data: FALSE

- Divided by gender: FALSE

- Municipality type: K

- Operating area: Kommunen, övergripande

- Auspice: Unknown

- Publication date: 2026-04-15

- Publication period: 2026

##### Keywords

Unknown

#### N00023: Kostnadsutjämningsnetto grundskola, kr/inv 1 nov fg år

##### Description

Kostnadsutjämningsnetto grundskola beräknas som kommunens
standardkostnad minus standardkostnad för riket i tkr dividerat med
antal invånare totalt 1/11 fg år. Källa: SCB.

##### Metadata

- Has OU data: FALSE

- Divided by gender: FALSE

- Municipality type: K

- Operating area: Kommunen, övergripande

- Auspice: Unknown

- Publication date: 2026-04-15

- Publication period: 2026

##### Keywords

Unknown

### Extra functions for exploring KPI metadata

KPI metadata is considerably more complex than other types of metadata.
To further assist in exploring KPI metadata the function
[`kpi_bind_keywords()`](https://lchansson.github.io/rKolada/reference/kpi_bind_keywords.md)
can be used to tag data with keywords (these are inferred from the KPI
title) to classify KPIs and make them more searchable.

``` r
# Add keywords to a KPI table
kpis_with_keywords <- kpi_filter |> kpi_bind_keywords(n = 4)

# count keywords
kpis_with_keywords |>
  tidyr::pivot_longer(dplyr::starts_with("keyword"), values_to = "keyword") |>
  dplyr::count(keyword, sort = TRUE)
#> # A tibble: 383 × 2
#>    keyword            n
#>    <chr>          <int>
#>  1 elever           160
#>  2 grundskola       150
#>  3 pedagogisk       113
#>  4 gymnasieelever   106
#>  5 gymnasieskola    103
#>  6 förskola          91
#>  7 personal          91
#>  8 åk                91
#>  9 kommunal          71
#> 10 år                69
#> # ℹ 373 more rows
```

Some KPIs can be very similar-looking and it can sometimes be hard to
discern which of the KPIs to use. To make sifting through data easier,
[`kpi_minimize()`](https://lchansson.github.io/rKolada/reference/kpi_minimize.md)
can be used to remove all redundant columns from a KPI table. (In this
case, “redundant” means “containing no information that helps in
differentiating KPIs from one another”, i.e. columns containing only one
single value for all observations in the table):

``` r
# Top 10 rows of the table
kpi_filter |> dplyr::slice(1:10)
#> # A tibble: 10 × 12
#>    id     title       description is_divided_by_gender municipality_type auspice
#>    <chr>  <chr>       <chr>       <lgl>                <chr>             <chr>  
#>  1 N00022 Kostnadsut… "Kostnadsu… FALSE                K                 NA     
#>  2 N00023 Kostnadsut… "Kostnadsu… FALSE                K                 NA     
#>  3 N00026 Kostnadsut… "Kostnadsu… FALSE                K                 NA     
#>  4 N00097 Nettokostn… "Avvikelse… FALSE                K                 T      
#>  5 N00100 Strukturko… "Strukturk… FALSE                K                 T      
#>  6 N00108 Månadsavlö… "Antal ans… FALSE                K                 E      
#>  7 N00114 Årsarbetar… "Antal års… FALSE                K                 E      
#>  8 N00303 Tillgängli… "Kommunind… FALSE                K                 T      
#>  9 N00314 Tillgängli… "Kvinnors … FALSE                K                 T      
#> 10 N00325 Tillgängli… "Mäns Komm… FALSE                K                 T      
#> # ℹ 6 more variables: operating_area <chr>, perspective <chr>,
#> #   prel_publication_date <chr>, publication_date <chr>, publ_period <chr>,
#> #   has_ou_data <lgl>

# Top 10 rows of the table, with non-distinct data removed
kpi_filter |> dplyr::slice(1:10) |> kpi_minimize()
#> # A tibble: 10 × 9
#>    id     title                   description auspice operating_area perspective
#>    <chr>  <chr>                   <chr>       <chr>   <chr>          <chr>      
#>  1 N00022 Kostnadsutjämningsnett… "Kostnadsu… NA      Kommunen, öve… Resurser   
#>  2 N00023 Kostnadsutjämningsnett… "Kostnadsu… NA      Kommunen, öve… Resurser   
#>  3 N00026 Kostnadsutjämningsnett… "Kostnadsu… NA      Kommunen, öve… Resurser   
#>  4 N00097 Nettokostnadsavvikelse… "Avvikelse… T       Kommunen, öve… Resurser   
#>  5 N00100 Strukturkostnad kommun… "Strukturk… T       Kommunen, öve… Resurser   
#>  6 N00108 Månadsavlönad personal… "Antal ans… E       Förskoleverks… Resurser   
#>  7 N00114 Årsarbetare inom försk… "Antal års… E       Förskoleverks… Resurser   
#>  8 N00303 Tillgänglighet till tj… "Kommunind… T       Kommunen, öve… Kvalitet o…
#>  9 N00314 Tillgänglighet till tj… "Kvinnors … T       Kommunen, öve… Kvalitet o…
#> 10 N00325 Tillgänglighet till tj… "Mäns Komm… T       Kommunen, öve… Kvalitet o…
#> # ℹ 3 more variables: prel_publication_date <chr>, publication_date <chr>,
#> #   publ_period <chr>
```

Note that
[`kpi_minimize()`](https://lchansson.github.io/rKolada/reference/kpi_minimize.md)
operates on the *current table*. This means that results may vary
depending on the data you’re operating on.

### Metadata groups

Kolada provides pre-defined *groups* of KPIs and municipalities/regions.
Exploring an using thse groups can facilitate meaningful comparisons
between different entities or help paint a broader picture of
developments in a certain field or area.

To `get`, `search` or `describe` group metadata, use the same techniques
as described above for regular metadata (relevant prefixes are
`kpi_grp_` and `municipality_grp_`).

A crucial difference between group metadata and other metadata tables,
however, is that group metadata comes in the form of a
[nested](https://tidyr.tidyverse.org/articles/nest.html) table.
Typically you might want to *unnest* the groups in a group metadata
table once yo hae found the relevant group(s) for your query. To do
this, use the `unnest` functions to create a table containing unnested
entities, e.g. running `kpi_grp_unnest(kpi_grp_df)` using a KPI group
metadata table as argument creates a `kpi_df` that can be further
processed using the `kpi_` functios described in previous sections of
this vignette.

In other words: a group table has one row per *group*, with members
nested inside. `unnest` expands it so you get one row per *member*,
which you can then pipe into
[`kpi_search()`](https://lchansson.github.io/rKolada/reference/kpi_search.md),
[`kpi_describe()`](https://lchansson.github.io/rKolada/reference/kpi_describe.md),
etc.

## Downloading data using metadata

An alternative approach to downloading data using known IDs is to use
metadata tables to construct arguments to
[`get_values()`](https://lchansson.github.io/rKolada/reference/get_values.md).
`rKolada` provides a `extract_ids` family of functions for passing a
metadata table as an argument to `get_values`. A typical workflow would
be to download metadata for (groups of) KPIs and/or municipalities, use
functions like
[`kpi_search()`](https://lchansson.github.io/rKolada/reference/kpi_search.md)
to filter down the tables to a few rows, and then call
[`get_values()`](https://lchansson.github.io/rKolada/reference/get_values.md)
to fetch data.

As an example, let’s say we want to download all KPIs describing Gross
Regional Product for all municipalities that are socioeconomically
similar to Arboga, a small municipality in central Sweden:

``` r
# Get KPIs describing Gross Regional Product of municipalities
kpi_filter <- get_kpi() |> 
  kpi_search("bruttoregionprodukt") |>
  kpi_search("K", column = "municipality_type")
# Creates a table with two rows

# Get a suitable group of municipalities
munic_grp_filter <- get_municipality_groups() |> 
  municipality_grp_search("Liknande kommuner socioekonomi, Arboga")
# Creates a table with one group of 7 municipalities

# Also include Arboga itself
arboga <- get_municipality() |> municipality_search("Arboga")

# Get data
grp_data <- get_values(
  kpi = kpi_extract_ids(kpi_filter),
  municipality = c(
    municipality_grp_extract_ids(munic_grp_filter),
    municipality_extract_ids(arboga)
  )
)
```

``` r
# Visualize results
library("ggplot2")
ggplot(grp_data, aes(year, value, color = municipality)) +
  # One line per municipality; linetype adds distinction in B/W print
  geom_line(aes(linetype = municipality)) +
  # Separate panel per KPI; free y-scales since units differ
  facet_grid(kpi ~ ., scales = "free") +
  labs(
    title = "Gross Regional Product per capita 2012-2018",
    subtitle = "Swedish municipalities similar to Arboga",
    caption = values_legend(grp_data, kpi_filter)  # Auto-generated legend
  ) +
  # Colour-blind-friendly palette
  scale_color_viridis_d(option = "B") +
  scale_y_continuous(labels = scales::comma)
#> Warning: Removed 952 rows containing missing values or values outside the scale range
#> (`geom_line()`).
#> `geom_line()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_line()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_line()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_line()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_line()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_line()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_line()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_line()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_line()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_line()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_line()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_line()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_line()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_line()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_line()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_line()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_line()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_line()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_line()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_line()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_line()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_line()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_line()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_line()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_line()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_line()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_line()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_line()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_line()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_line()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_line()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_line()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_line()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_line()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_line()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_line()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_line()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_line()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_line()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_line()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_line()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_line()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_line()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
#> `geom_line()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
```

![](introduction-to-rkolada_files/figure-html/unnamed-chunk-4-1.png)

> **More on ggplot2?** See <https://ggplot2-book.org/>.

## Related packages

If you work with data from PX-Web APIs (Statistics Sweden, Statistics
Norway, Statistics Finland, etc.), see
[rpx](https://lchansson.github.io/rpx/) — a sibling package that follows
the same design principles as rKolada.
