#' Generate KPI metadata from a Kolada KPI object
#'
#' @param kpi_df A Kolada KPI tibble
#'
#' @return a Kolada KPI metadata table

generate_kpi_metadata <- function(kpi_df) {
  descvar <- "description"
  oavar <- "operating_area"

  kpi_df |>
    dplyr::mutate(
      estimated_source = {{ descvar }} |>
        stringr::str_extract("(?<=K\u00e4lla).+$") |>
        stringr::str_remove("^:") |>
        stringr::str_remove("\\.$") |>
        stringr::str_squish(),
      estimated_category = dplyr::case_when(
        stringr::str_detect({{ oavar }}, "[Ss]skol|[Gg]ymnasi|[Uu]tbild") ~ "Education",
        stringr::str_detect({{ oavar }}, "[Ss]jukv\u00e5rd|[Hh]\u00e4ls") ~ "Health",
        stringr::str_detect({{ oavar }}, "[Oo]msorg|[Vv]\u00e5rd") ~ "Care",
        TRUE ~ "Unknown"
      )
    )
}