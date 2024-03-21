#' Generate KPI metadata from a Kolada KPI object
#'
#' @return a Kolada KPI metadata table

generate_kpi_metadata <- function(kpi_df) {
  kpi_df |>
    mutate(
      estimated_source = description |>
        str_extract("(?<=Källa).+$") |>
        str_remove("^:") |>
        str_remove("\\.$") |>
        str_squish(),
      estimated_category = case_when(
        str_detect(operating_area, "[Ss]skol|[Gg]ymnasi|[Uu]tbild") ~ "Education",
        str_detect(operating_area, "[Ss]jukvård|[Hh]äls") ~ "Health",
        str_detect(operating_area, "[Oo]msorg|[Vv]ård") ~ "Care",
        TRUE ~ "Unknown"
      )
    )
}