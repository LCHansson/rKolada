# Create KPI long-form descriptions to add to a plot

In a Kolada values table, only KPI ID codes are preserved. But in plots
you often want a legend that explains what each KPI ID represents. Since
KPI descriptions are typically wordy, ggplot2 legends are
under-dimensioned for this task. `values_legend()` returns a caption
string that can be passed directly to `ggplot2::labs(caption = ...)`
instead.

## Usage

``` r
values_legend(
  values_df,
  kpi_df = NULL,
  lang = NULL,
  omit_varname = FALSE,
  omit_desc = FALSE
)
```

## Arguments

- values_df:

  A Kolada value table, as created by
  [`get_values()`](https://lchansson.github.io/rKolada/reference/get_values.md).

- kpi_df:

  A KPI table, as created by
  [`get_kpi()`](https://lchansson.github.io/rKolada/reference/get_kpi.md).
  If `NULL`, only KPI codes (no titles) will be shown.

- lang:

  Language for the caption prefix: `"SV"` (Swedish, default) or `"EN"`
  (English). Defaults to `getOption("rKolada.lang", "SV")`.

- omit_varname:

  Logical. If `TRUE`, omit the KPI codes (e.g. `"N01951: "`) and show
  only the titles.

- omit_desc:

  Logical. If `TRUE`, omit the KPI titles and show only the codes.

## Value

A single character string suitable for plot captions.

## Details

By default the caption shows both the title and the code for every KPI
included in the data, prefixed by a source line, e.g.

    Källa: Kolada
    N01951: Bruttoregionprodukt per capita
    N01952: Sysselsättningsgrad

Use `omit_varname` to drop the codes, `omit_desc` to drop the titles,
and `lang` to switch the source prefix between Swedish and English.
Kolada KPI titles are returned by the API in Swedish regardless of the
`lang` setting.

## Examples

``` r
if (kolada_available()) {
  kpis <- get_kpi() |> kpi_search("bruttoregionprodukt")
  d <- get_values(
    kpi = kpi_extract_ids(kpis),
    municipality = "0180",
    period = 2018:2020
  )
  values_legend(d, kpis)
  values_legend(d, kpis, lang = "EN")
  values_legend(d, kpis, omit_varname = TRUE)
  values_legend(d, kpis, omit_desc = TRUE)
}
#> [1] "Källa: Kolada\nN03700\nN03701\nN03702\nN03703"
```
