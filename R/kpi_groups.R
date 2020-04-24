#' Extract KPI ID strings from a Kolada KPI Group metadata table
#'
#'
#'
#' @param kpi_grp_df A Kolada KPI Group metadata table, as created by e.g. \code{get_kpi_groups}.
#'
#' @export
kpi_grp_extract_ids <- function(kpi_grp_df) {
  purrr::map(kpi_grp_df$members, purrr::pluck(1)) %>% unlist()
}


#' Create a KPI table from a Kolada KPI Group metadata table
#'
#'
#'
#' @param kpi_grp_df A Kolada KPI Group metadata table, as created by e.g. \code{get_kpi_groups}.
#'
#' @return A Kolada KPI metadata table
#'
#' @export
kpi_grp_unnest_kpis <- function(kpi_grp_df) {
  kpi_grp_df %>%
    tidyr::unnest(cols = c(members)) %>%
    dplyr::select(group_id = id, id = member_id, group_title = title, title = member_title) %>%
    dplyr::select(id, title, group_id, group_title)
}


#' Describe the KPIs in a Kolada KPI Group metadata table
#'
#'
#'
#' @param kpi_grp_df A Kolada KPI Group metadata table, as created by e.g. \code{get_kpi_groups}.
#' @param max_n The maximum number of KPI groups to describe.
#' @param format Output format. Can be one of "inline" or "md" (markdown).
#' @param heading_level The top heading level output format is "md".
#' @param sub_heading_level The sub heading level output format is "md".
#'
#' @export
kpi_grp_describe <- function(
  kpi_grp_df,
  max_n = 5,
  format = "inline",
  heading_level = 2,
  sub_heading_level = heading_level + 1
) {
  if (!format %in% c("inline", "md"))
    stop("'format' must be one of c(\"inline\", \"md\")")

  desc_df <- kpi_grp_df %>%
    dplyr::slice(1:min(max_n, nrow(kpi_grp_df)))

  desc_df <- desc_df %>%
    dplyr::mutate(
      num_members = purrr::map(members, nrow),
      m_id = purrr::map(members, purrr::pluck("member_id")),
      m_t = purrr::map(members, purrr::pluck("member_title")),
      m_pre = "- ",
      member_data = purrr::pmap(list(m_pre, m_id, m_t), paste, sep = " ", collapse = "\n")
    )

  desc_df %>%
    glue_data_safely(desc_glue_spec("group"), .entity = "KPI", .format = format, .heading_length = heading_level, .sub_heading_length = sub_heading_level, .otherwise = "Unknown")
}


#' Search a Kolada KPI Group metadata table for group names
#'
#'
#'
#' @param kpi_grp_df A Kolada KPI Group metadata table, as created by e.g. \code{get_kpi_groups}.
#' @param query A search term or a vector of search terms to filter by. Case insensitive.
#'
#' @return A Kolada KPI Group metadata table
#'
#' @export
kpi_grp_search <- function(kpi_grp_df, query) {
  kpi_grp_df %>%
    dplyr::filter(stringr::str_detect(tolower(title), tolower(query)))
}