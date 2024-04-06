#' Extract KPI ID strings from a Kolada KPI Group metadata table
#'
#' This function is primarily intended as a convenient way to pass a (filtered)
#' Kolada KPI Group metadata table to \code{\link{get_values}}. All IDs of the
#' KPIs contained in each group in the table are extracted.
#'
#' @param kpi_grp_df A Kolada KPI Group metadata table, as created by e.g.
#' \code{get_kpi_groups}.
#'
#' @return A vector of KPI IDs.
#'
#' @export
kpi_grp_extract_ids <- function(kpi_grp_df) {
  purrr::map(kpi_grp_df$members, purrr::pluck(1)) %>% unlist()
}


#' Create a KPI table from a Kolada KPI Group metadata table
#'
#' KPI groups are a convenient way to discover sets of KPIs that can be used to
#' highlight different aspects of a policy area. A practical workflow for
#' discovering such sets can be to search through KPI Group metadata using
#' \code{\link{kpi_grp_search}} to search for keywords and
#' \code{\link{kpi_grp_describe}} to inspect contents of KPI groups. Once you
#' have created a KPI group table that has been narrowed down to the group/s you
#' are looking for, \code{\link{kpi_grp_unnest}} is used to create a KPI
#' metadata table for further processing.
#'
#' @param kpi_grp_df A Kolada KPI Group metadata table, as created by e.g.
#' \code{get_kpi_groups}.
#'
#' @return A Kolada KPI metadata table
#'
#' @examples
#' if (kolada_available()) {
#' # Download KPI Group metadata
#' kpi_grp_df <- get_kpi_groups()
#'
#' # Create a KPI metadata table from KPI groups matching the term
#' # "utbidning" (education)
#' kpi_grp_df %>%
#'   kpi_grp_search("utbildning") %>%
#'   kpi_grp_unnest()
#' }
#'
#' @export
kpi_grp_unnest <- function(kpi_grp_df) {
  kpi_grp_df %>%
    tidyr::unnest(cols = c(.data$members)) %>%
    dplyr::select(
      group_id = .data$id, id = .data$member_id,
      group_title = .data$title, title = .data$member_title
    ) %>%
    dplyr::select(.data$id, .data$title, .data$group_id, .data$group_title)
}


#' Describe the KPIs in a Kolada KPI Group metadata table
#'
#' Print a human-readable description of each row of a KPI Group metadata table,
#' including member KPIs (up to a maximum number of rows). Can be printed either
#' directly to the R console or used to populate a R markdown document, which
#' can be useful for documentation purposes.
#'
#' @param kpi_grp_df A Kolada KPI Group metadata table, as created by e.g.
#' \code{get_kpi_groups}.
#' @param max_n The maximum number of KPI groups to describe.
#' @param format Output format. Can be one of "inline" or "md" (markdown).
#' @param heading_level The top heading level output format is "md".
#' @param sub_heading_level The sub heading level output format is "md".
#'
#' @return Returns the object passed to the function, invisibly, to be re-used
#' in a pipe.
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
      num_members = purrr::map(.data$members, nrow),
      m_id = purrr::map(.data$members, purrr::pluck("member_id")),
      m_t = purrr::map(.data$members, purrr::pluck("member_title")),
      m_pre = "- ",
      member_data = purrr::pmap(
        list(.data$m_pre, .data$m_id, .data$m_t),
        paste, sep = " ", collapse = "\n"
      )
    )

  desc_df %>%
    glue_data_safely(
      desc_glue_spec("group"), .entity = "KPI", .format = format,
      .heading_length = heading_level,
      .sub_heading_length = sub_heading_level, .otherwise = "Unknown"
    )

  invisible(kpi_grp_df)
}


#' Search a Kolada KPI Group metadata table for group names
#'
#' Search a Kolada KPI Group metadata table. Only keep rows that
#' contain the search query. Searches group titles and group IDs. Note that this
#' function does not search for individual KPIs contained within KPI groups!
#' To search for KPIs within a KPI group, see examples below for an example
#' using \code{kpi_grp_unnest}.
#'
#' @param kpi_grp_df A Kolada KPI Group metadata table, as created by e.g.
#' \code{get_kpi_groups}.
#' @param query A search term or a vector of search terms to filter by. Case
#' insensitive.
#'
#' @return A Kolada KPI Group metadata table
#'
#' @examples
#' if (kolada_available()) {
#' kpi_grp_df <- get_kpi_groups()
#'
#' # Which KPI groups match the keyword "ekonomi" (economy)?
#' kpi_grp_df %>% kpi_grp_search("ekonomi")
#'
#' # Which KPI groups contain KPIs matching the keyword "arbete" (work/labour)?
#' kpi_grp_df %>%
#'   kpi_grp_unnest() %>%
#'   kpi_search("arbete") %>%
#'   dplyr::count(group_title, sort = TRUE)
#' }
#'
#' @export
kpi_grp_search <- function(kpi_grp_df, query) {
  kpi_grp_df %>%
    dplyr::filter(stringr::str_detect(tolower(.data$title), tolower(query)))
}