#' Simplify a KPI table
#'
#' Remove all columns from a Kolada KPI metadata table that are monotonous across
#' the table, i.e. columns that contain only one single value. Also remove undocumented
#' columns, i.e. columns that contain unintelligible and undocumented information.
#'
#' @param kpi_df A Kolada KPI metadata table, e.g. as created by \code{\link{get_kpi}}.
#' @param remove_undocumented_columns Remove columns from the KPI table which are
#' undocumented in the API?
#' @param remove_monotonous_data Remove columns from the KPI table which contain
#' exactly the same information for all entries in the table?
#'
#' @return A Kolada KPI metadata table
#'
#' @export
kpi_minimize <- function(kpi_df, remove_undocumented_columns = TRUE, remove_monotonous_data = TRUE) {
  if (isTRUE(remove_undocumented_columns) & "auspices" %in% names(kpi_df)) {
    kpi_df <- kpi_df %>%
      dplyr::select(-auspices)
  }
  if (isTRUE(remove_monotonous_data))
    kpi_df <- kpi_df %>%
      dplyr::select_if(names(.) %in% c("id", "title", "description") | purrr::map(., dplyr::n_distinct) > 1)

  kpi_df <- kpi_df %>%
    dplyr::select(id, title, description, dplyr::everything())

  kpi_df
}


#' Add keyword columns to a Kolada KPI table
#'
#' Identify \code{n} keywords describing the KPI and add them as new columns. Keywords are inferred from the \code{title} field of the table.
#'
#' @param kpi_df A Kolada KPI metadata table, e.g. as created by \code{\link{get_kpi}}.
#' @param n How many keyword columns should be added?
#' @param form Can be either "wide" (default) or "long". Whether to return keywords as separate columns ("wide") or as
#' separate rows, duplicating all other data ("long").
#'
#' @return A Kolada KPI metadata table
#'
#' @examples
#' \dontrun{
#' kpi_df <- get_kpi() %>%
#'   kpi_add_keywords(n = 3)
#' }
#'
#' @export
kpi_bind_keywords <- function(kpi_df, n = 2, form = c("wide", "long")) {
  form <- form[[1]]

  kpi_df <- kpi_df %>%
    dplyr::mutate(
      title_words = stringr::str_extract_all(tolower(title), "\\w+")
    ) %>%
    tidyr::unnest(cols = c(title_words)) %>%
    dplyr::filter(!title_words %in% stopwords()) %>%
    dplyr::group_by(id) %>%
    dplyr::slice(1:n) %>%
    dplyr::ungroup()

  if (form == "wide") {
    kpi_df <- kpi_df %>%
      dplyr::mutate(nm_col = dplyr::row_number()) %>%
      tidyr::pivot_wider(
        names_from = nm_col, names_prefix = "keyword_",
        values_from = title_words, values_fill = list(title_words = "")
      )
  }

  kpi_df
}


#' Search for Kolada KPIs using a Kolada KPI table
#'
#' Search a Kolada KPI metadata table. Only keep rows that contain the search query.
#'
#' @param kpi_df A Kolada KPI metadata table, e.g. as created by \code{\link{get_kpi}}.
#' @param query A search term or a vector of search terms to filter by. Case insensitive.
#' @param column (Optional) A string or character vector with the names of
#' columns in which to search for \code{query}.
#'
#' @return A Kolada KPI metadata table
#'
#' @examples
#' \dontrun{
#' # Search for a single search term in a KPI table
#' kpis <- get_kpi()
#' kpi_filter <- kpi_search(kpis, "inkomst")
#'
#' # Add keywords to a KPI table and search for multiple terms among the keywords
#' kpi_filter <- get_kpi(cache = TRUE) %>%
#'   kpi_bind_keywords(n = 3) %>%
#'   kpi_search(c("inkomst", "arbete"), column = c("keyword_1", "keyword_2", "keyword_3"))
#' }
#'
#' @export
kpi_search <- function(kpi_df, query, column = NULL) {
  if (is.null(column))
    column <- names(kpi_df)

  f <- function(obj, query) {
    stringr::str_detect(tolower(obj), tolower(as.character(paste(query, collapse="|"))))
  }

  hits <- kpi_df %>%
    dplyr::filter_at(
      .vars = dplyr::vars(column),
      .vars_predicate = dplyr::any_vars(f(., query))
    )

  hits
}


#' Describe the KPIs in a Kolada KPI metadata table
#'
#' Print a human-readable description of each entity of a KPI metadata table (up
#' to a maximum number of rows). Can be printed either directly to the R console
#' or used to populate a R markdown document, which can be useful for documentation
#' purposes.
#'
#' @param kpi_df A Kolada KPI metadata table
#' @param max_n The maximum number of KPIs to describe.
#' @param format Output format. Can be one of "inline" (default) or "md", i.e. markdown.
#' @param heading_level The top heading level output format is "md".
#' @param sub_heading_level The sub heading level output format is "md".
#'
#' @export
kpi_describe <- function(
  kpi_df,
  max_n = 5,
  format = "inline",
  heading_level = 2,
  sub_heading_level = heading_level + 1
) {
  if (!format %in% c("inline", "md"))
    stop("'format' must be one of c(\"inline\", \"md\")")

  desc_df <- kpi_df %>%
    dplyr::slice(1:min(max_n, nrow(kpi_df)))

  if (any(stringr::str_detect(names(desc_df), "keyword")))
    desc_df <- desc_df %>%
      dplyr::mutate(keyword_1 = paste("- ", keyword_1, sep = "")) %>%
      tidyr::unite(keywords, dplyr::starts_with("keyword"), sep = "\n- ") %>%
      dplyr::mutate(keywords = stringr::str_remove(keywords, "(-[\\s]*)+$"))

  desc_df %>%
    glue_data_safely(desc_glue_spec("nogroup"), .entity = "KPI", .format = format, .heading_length = heading_level, .sub_heading_length = sub_heading_level, .otherwise = "Unknown")
}


#' Extract a vector of KPI ID strings from a Kolada KPI metadata table
#'
#'
#'
#' @param kpi_df A Kolada KPI metadata table, e.g. as created by \code{\link{get_kpi}}.
#' @export
kpi_extract_ids <- function(kpi_df) {
  kpi_df$id
}


