# Create KPI long-form descriptions to add to a plot

In a Kolada values table, only KPI ID names are preserved. But in plots
you often want to add a legend to explain what each KPI ID represents.
But since KPI explanations are mostly relatively wordy, ggplot2 legends
are under-dimensioned for this task. `values_legend` returns a string
which can conveniently be used as caption to a plot instead.

## Usage

``` r
values_legend(values_df, kpi_df)
```

## Arguments

- values_df:

  A Kolada value table, as created by
  [`get_values()`](https://lchansson.github.io/rKolada/reference/get_values.md).

- kpi_df:

  A KPI table, e.g. as created by
  [`get_kpi()`](https://lchansson.github.io/rKolada/reference/get_kpi.md).

## Value

A string which should be used as caption in a plot.
