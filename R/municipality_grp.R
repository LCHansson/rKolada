#' Extract municipality ID strings from a Kolada municipality group table
#'
#'
#'
#' @param munic_grp_df A Kolada municipality group table, as created by e.g. \code{get_municipality_groups}.
#'
#' @export
municipality_grp_extract_ids <- function(munic_grp_df) {
  purrr::map(munic_grp_df$members, purrr::pluck(1)) %>% unlist()
}


#' Create a municipality table from a Kolada Municipality Group metadata table
#'
#'
#'
#' @param munic_grp_df A Kolada Municipality Group metadata table, as created by e.g.
#' \code{get_municipality_groups}.
#'
#' @return A Kolada Municipality metadata table
#'
#'
#' @export
municipality_grp_unnest_municipalities <- function(munic_grp_df) {
  munic_grp_df %>%
    tidyr::unnest(cols = c(members)) %>%
    dplyr::select(group_id = id, id = member_id, group_title = title, title = member_title) %>%
    dplyr::select(id, title, group_id, group_title)
}


#' Describe the KPIs in a Kolada Municipality Group metadata table
#'
#'
#'
#' @param munic_grp_df A Kolada Municipality Group metadata table, as created by e.g. \code{get_municipality_groups}.
#' @param max_n The maximum number of KPI groups to describe.
#' @param format Output format. Can be one of "inline" or "md" (markdown).
#' @param heading_level The top heading level output format is "md".
#' @param sub_heading_level The sub heading level output format is "md".
#'
#' @export
municipality_grp_describe <- function(
  munic_grp_df,
  max_n = 5,
  format = "inline",
  heading_level = 2,
  sub_heading_level = 3
) {
  if (!format %in% c("inline", "md"))
    stop("'format' must be one of c(\"inline\", \"md\")")

  desc_df <- munic_grp_df %>%
    dplyr::slice(1:min(max_n, nrow(munic_grp_df)))

  desc_df <- desc_df %>%
    dplyr::mutate(
      num_members = purrr::map(members, nrow),
      m_id = purrr::map(members, purrr::pluck("member_id")),
      m_t = purrr::map(members, purrr::pluck("member_title")),
      m_pre = "-",
      member_data = purrr::pmap(list(m_pre, m_id, m_t), paste, sep = " ", collapse = "\n")
    )

  desc_df %>%
    glue_data_safely(desc_glue_spec("group"), .entity = "Municipality", .format = format, .heading_length = heading_level, .sub_heading_length = sub_heading_level, .otherwise = "Unknown")

}


#' Search a Kolada Municipality Group metadata table for group names
#'
#'
#'
#' @param munic_grp_df A Kolada Municipality Group metadata table, as created by e.g. \code{get_municipality_groups}.
#' @param query A search term or a vector of search terms to filter by. Case insensitive.
#'
#' @return A Kolada Municipality Group metadata table
#'
#' @export
municipality_grp_search <- function(munic_grp_df, query) {
  munic_grp_df %>%
    dplyr::filter(stringr::str_detect(tolower(title), tolower(query)))
}