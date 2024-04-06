#' Generate KPI metadata from a Kolada KPI object
#'
#' @param kpi_df A Kolada KPI tibble
#'
#' @return a Kolada KPI metadata table

generate_kpi_metadata <- function(kpi_df) {
  kpi_df |>
    dplyr::mutate(
      estimated_source = description |>
        stringr::str_extract("(?<=K\u00e4lla).+$") |>
        stringr::str_remove("^:") |>
        stringr::str_remove("\\.$") |>
        stringr::str_squish(),
      estimated_category = dplyr::case_when(
        stringr::str_detect(operating_area, "[Ss]skol|[Gg]ymnasi|[Uu]tbild") ~ "Education",
        stringr::str_detect(operating_area, "[Ss]jukv\u00e5rd|[Hh]äls") ~ "Health",
        stringr::str_detect(operating_area, "[Oo]msorg|[Vv]\u00e4rd") ~ "Care",
        TRUE ~ "Unknown"
      )
    )
}