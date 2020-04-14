#' Simplify a KPI table
#'
#' @param kpi_df A KPI table
#' @param remove_undocumented_columns Remove columns from the KPI table which are
#' undocumented in the API?
#' @param remove_monotonous_data Remove columns from the KPI table which contain
#' exactly the same information for all entries in the table?
#'
#' @export
kpi_minimize <- function(kpi_df, remove_undocumented_columns = TRUE, remove_monotonous_data = FALSE) {
  if (isTRUE(remove_undocumented_columns) & "auspices" %in% names(kpi_df)) {
    kpi_df <- kpi_df %>%
      dplyr::select(-auspices)
  }
  if (isTRUE(remove_monotonous_data))
    kpi_df <- kpi_df %>%
      dplyr::select_if(names(.) %in% c("id", "title", "desctiption") | purrr::map(., dplyr::n_distinct) > 1)

  kpi_df <- kpi_df %>%
    dplyr::select(id, title, description, dplyr::everything())

  kpi_df
}

#' Add keywords to a Kolada KPI table
#'
#' @param kpi_df A KPI table
#' @param n How many keyword columns should be added?
#'
#' @examples
#' \dontrun{
#' kpi_df <- kpi_get() %>%
#'   kpi_add_keywords(3)
#' }
#'
#' @export
kpi_add_keywords <- function(kpi_df, n = 2) {
  kpi_df <- kpi_df %>%
    dplyr::mutate(
      title_words = stringr::str_extract_all(tolower(title), "\\w+")
    ) %>%
    tidyr::unnest(cols = c(title_words)) %>%
    dplyr::filter(!title_words %in% stopwords()) %>%
    dplyr::group_by(id) %>%
    dplyr::slice(1:n) %>%
    dplyr::mutate(nm_col = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(
      names_from = nm_col, names_prefix = "keyword_",
      values_from = title_words, values_fill = list(title_words = "")
    )

  kpi_df
}

#' Search for Kolada KPIs using a Kolada KPI table
#'
#'
#'
#' @param kpi_df A KPI table
#' @param query One or a vector of search terms. Can be a string or a number.
#' Search is case insensitive.
#' @param columns (Optional) A string och character vector with the names of
#' columns in which to search for \code{query}
#'
#' @examples
#' \dontrun{
#' # Search for a single search term in a KPI table
#' kpis <- kpi_get()
#' kpi_filter <- kpi_search(kpis, "inkomst")
#'
#' # Add keywords to a KPI table and search for multiple terms among the keywords
#' kpi_filter <- kpi_get(cache = TRUE) %>%
#'   kpi_add_keywords(n = 3) %>%
#'   kpi_search(c("inkomst", "arbete"), c("keyword_1", "keyword_2", "keyword_3"))
#' }
#'
#' @export
kpi_search <- function(kpi_df, query, columns = NULL) {
  if (is.null(columns))
    columns <- names(kpi_df)

  f <- function(obj, query) {
    stringr::str_detect(tolower(obj), tolower(as.character(paste(query, collapse="|"))))
  }

  hits <- kpi_df %>%
    dplyr::filter_at(
      .vars = dplyr::vars(columns),
      .vars_predicate = dplyr::any_vars(f(., query))
    )

  hits
}


#' Describe the KPIs in a Kolada KPI table
#'
#' @param kpi_df A KPI table
#' @param max_n The maximum number of KPIs to describe.
#'
#' @export
kpi_describe <- function(kpi_df, max_n = 5) {
  desc_df <- kpi_df %>%
    dplyr::slice(1:min(max_n, nrow(kpi_df)))

  if (any(stringr::str_detect(names(desc_df), "keyword")))
    desc_df <- desc_df %>%
      dplyr::mutate(keyword_1 = paste("- ", keyword_1, sep = "")) %>%
      tidyr::unite(keywords, dplyr::starts_with("keyword"), sep = "\n- ") %>%
      dplyr::mutate(keywords = stringr::str_remove(keywords, "(-[\\s]*)+$"))

  desc_df %>%
    glue_data_safely(desc_glue_spec("kpi"), .otherwise = "Unknown")
}

#' Create a search filter from a Kolada KPI table
#' @param kpi_df
#' @export
kpi_query_filter <- function(kpi_df) {
  kpi_df$id
}

#' Create a search filter from a Kolada KPI group table
#' @param kpig_df
#' @export
kpi_groups_query_filter <- function(kpig_df) {
  purrr::map(kpig_df$members, purrr::pluck(1)) %>% unlist()
}


#' Describe KPIs in a KPI group
#' @param kpig_df
#' @param kpi_df
#' @param query
#' @export
kpi_groups_to_kpi_df <- function(kpig_df, kpi_df = NULL, query = NULL) {

  if (!is.null(query))
    kpig_df <- kpig_df %>% kpi_groups_search(query)

  if (is.null(kpi_df))
    kpi_df <- kpi_get()

  kpi_df %>%
    dplyr::inner_join(
      kpig_df %>%
        tidyr::unnest(cols = c(members)) %>%
        dplyr::select(id = member_id, title = member_title),
      by = c("id", "title")
    )
}


#' Describe the KPIs in a Kolada KPI group table
#'
#' @param kpi_df A KPI group table
#' @param max_n The maximum number of KPI groups to describe.
#'
#' @export
kpi_groups_describe <- function(kpig_df, max_n = 5) {
  desc_df <- kpig_df %>%
    dplyr::slice(1:min(max_n, nrow(kpig_df)))

  desc_df <- desc_df %>%
    dplyr::mutate(
      num_members = purrr::map(members, nrow),
      m_id = purrr::map(members, purrr::pluck("member_id")),
      m_t = purrr::map(members, purrr::pluck("member_title")),
      m_pre = "- ",
      member_data = purrr::pmap(list(m_pre, m_id, m_t), paste, sep = " ", collapse = "\n")
    )

  desc_df %>%
    glue_data_safely(desc_glue_spec("kpi_group"), .otherwise = "Unknown")
}

#' Search a Kolada KPI group table for group names
#' @param kpig_df
#' @param query
#' @export
kpi_groups_search <- function(kpig_df, query) {
  kpig_df %>%
    dplyr::filter(stringr::str_detect(tolower(title), tolower(query)))
}
