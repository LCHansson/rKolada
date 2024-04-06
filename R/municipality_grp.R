#' Extract municipality ID strings from a Kolada municipality group table
#'
#' This function is primarily intended as a convenient way to pass a (filtered)
#' Kolada municipality group metadata table to \code{\link{get_values}}. All IDs
#' of the municipalities contained in each group in the table are extracted.
#'
#' @param munic_grp_df A Kolada municipality group table, as created by e.g.
#' \code{get_municipality_groups}.
#'
#' @return A vector of Municipality IDs.
#'
#' @export
municipality_grp_extract_ids <- function(munic_grp_df) {

  if (is.null(munic_grp_df)) {
    warning("\nAn empty object was used as input to municipality_grp_extract_ids().")
    return(NULL)
  }

  purrr::map(munic_grp_df$members, purrr::pluck(1)) %>% unlist()
}


#' Create a municipality table from a Kolada Municipality Group metadata table
#'
#' Municipality groups are a convenient way to discover pre-rendered sets of
#' municipalities. A practical workflow for discovering
#' such sets can be to search through Municipality Group metadata using
#' \code{\link{municipality_grp_search}} to search for keywords and
#' \code{\link{municipality_grp_describe}} to inspect contents of KPI groups.
#' Once you have created a Municipality Group metadata table that has been
#' narrowed down to the group/s you are looking for,
#' \code{\link{municipality_grp_unnest}} is used to create a municipality
#' metadata table for further processing.
#'
#' @param munic_grp_df A Kolada Municipality Group metadata table, as created by
#' e.g. \code{get_municipality_groups}.
#'
#' @return A Kolada Municipality metadata table
#'
#' @examples
#' if (kolada_available()) {
#' # Download Municipality Group metadata
#' # (skip the parameter "max_results" to actually download all available data)
#' munic_grp_df <- get_municipality_groups(max_results = 100)
#'
#' # Create a Municipality metadata table from municipality groups matching the
#' # term "Arboga"
#' munic_grp_df %>%
#'   municipality_grp_search("arboga") %>%
#'   municipality_grp_unnest()
#' }
#'
#' @export
municipality_grp_unnest <- function(munic_grp_df) {

  if (is.null(munic_grp_df)) {
    warning("\nAn empty object was used as input to municipality_grp_unnest().")
    return(NULL)
  }

  munic_grp_df %>%
    tidyr::unnest(cols = c(.data$members)) %>%
    dplyr::select(
      group_id = .data$id, id = .data$member_id,
      group_title = .data$title, title = .data$member_title
    ) %>%
    dplyr::select(.data$id, .data$title, .data$group_id, .data$group_title)
}


#' Describe the municipalitie in a Kolada Municipality Group metadata table
#'
#' Print a human-readable description of each row of a Municipality Group
#' metadata table, including member municipalities (up to a maximum number of
#' rows). Can be printed either directly to the R console
#' or used to populate a R markdown document, which can be useful for
#' documentation purposes.
#'
#' @param munic_grp_df A Kolada Municipality Group metadata table, as created
#' by e.g. \code{get_municipality_groups}.
#' @param max_n The maximum number of KPI groups to describe.
#' @param format Output format. Can be one of "inline" or "md" (markdown).
#' @param heading_level The top heading level output format is "md".
#' @param sub_heading_level The sub heading level output format is "md".
#'
#' @return Returns the object passed to the function, invisibly, to be re-used
#' in a pipe.
#'
#' @export
municipality_grp_describe <- function(
  munic_grp_df,
  max_n = 5,
  format = "inline",
  heading_level = 2,
  sub_heading_level = heading_level + 1
) {
  if (!format %in% c("inline", "md"))
    stop("'format' must be one of c(\"inline\", \"md\")")

  desc_df <- munic_grp_df %>%
    dplyr::slice(1:min(max_n, nrow(munic_grp_df)))

  desc_df <- desc_df %>%
    dplyr::mutate(
      num_members = purrr::map(.data$members, nrow),
      m_id = purrr::map(.data$members, purrr::pluck("member_id")),
      m_t = purrr::map(.data$members, purrr::pluck("member_title")),
      m_pre = "-",
      member_data = purrr::pmap(list(.data$m_pre, .data$m_id, .data$m_t),
                                paste, sep = " ", collapse = "\n")
    )

  desc_df %>%
    glue_data_safely(desc_glue_spec("group"), .entity = "Municipality",
                     .format = format, .heading_length = heading_level,
                     .sub_heading_length = sub_heading_level,
                     .otherwise = "Unknown")

  invisible(munic_grp_df)
}


#' Search a Kolada Municipality Group metadata table for group names
#'
#' Search a Kolada Municipality Group metadata table. Only keep rows that
#' contain the search query. Searches group titles and group IDs. Note that this
#' function does not search for individual municipalities contained within
#' municipality groups! To search for KPIs within a KPI group, see examples
#' below for an example using \code{municipality_grp_unnest}.
#'
#' @param munic_grp_df A Kolada Municipality Group metadata table, as created by
#' e.g. \code{get_municipality_groups}.
#' @param query A search term or a vector of search terms to filter by. Case
#' insensitive.
#'
#' @return A Kolada Municipality Group metadata table
#'
#' @export
municipality_grp_search <- function(munic_grp_df, query) {
  munic_grp_df %>%
    dplyr::filter(stringr::str_detect(tolower(.data$title), tolower(query)))
}