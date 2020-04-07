#' Get a table of KPIs from the Kolada metadata API
#'
#' @param id (Optional) One or several KPI IDs
#' @param cache (Optional) If you plan on doing multiple concurrent calls to retrieve the same data from the Kolada API (this is usually the case) you can use this option to store downloaded metadata to disk. If you call the function again, data will be read from disk instead of retrieving it over the internet if a cache file can be located. If set to \code{FALSE} or \code{"no"}, don't store any data on disk. If set to \code{TRUE} or \code{"yes"} or \code{"tempfile"}, store data in a \code{tempfile} (this will expire at the end of your R session). If set to \code{"wd"}, data will be stored in \code{.RData} files in your current working directory.
#'
#' @examples
#'
#' \dontrun{
#' # Download KPI table and store a cache copy of the results in your current working directory
#' kpi_df <- kpi_get(cache = "wd")
#'
#' # Download KPI table and store a cache copy of the results in a tempfile (will expire when your R session ends)
#' kpi_df <- kpi_get(cache = TRUE)
#' }
#'
#' @export
kpi_get <- function(id = NULL, cache = FALSE) {
  get_kld_metadata(entity = "kpi", id = id, cache = cache)
}

#' Get a table of KPI groups from the Kolada metadata API
#'
#' @param id (Optional) One or several KPI group IDs
#' @param cache (Optional) If you plan on doing multiple concurrent calls to retrieve the same data from the Kolada API (this is usually the case) you can use this option to store downloaded metadata to disk. If you call the function again, data will be read from disk instead of retrieving it over the internet if a cache file can be located. If set to \code{FALSE} or \code{"no"}, don't store any data on disk. If set to \code{TRUE} or \code{"yes"} or \code{"tempfile"}, store data in a \code{tempfile} (this will expire at the end of your R session). If set to \code{"wd"}, data will be stored in \code{.RData} files in your current working directory.
#'
#' @examples
#'
#' \dontrun{
#' # Download KPI table and store a cache copy of the results in your current working directory
#' kpi_df <- kpi_groups_get(cache = "wd")
#'
#' # Download KPI table and store a cache copy of the results in a tempfile (will expire when your R session ends)
#' kpi_df <- kpi_groups_get(cache = TRUE)
#' }
#'
#' @export
kpi_groups_get <- function(id = NULL, cache = FALSE) {
  get_kld_metadata(entity = "kpi_groups", id = id, cache = cache)
}

#' Simplify a KPI table
#'
#' @param kpi_df A KPI table
#' @param remove_undocumented_columns Remove columns from the KPI table which are undocumented in the API?
#' @param remove_monotonous_data Remove columns from the KPI table which contain exactly the same information for all entries in the table?
#'
#' @export
kpi_simplify <- function(kpi_df, remove_undocumented_columns = TRUE, remove_monotonous_data = FALSE) {
  if (isTRUE(remove_undocumented_columns) & "auspices" %in% names(kpi_df)) {
    kpi_df <- kpi_df %>%
      select(-auspices)
  }
  if (isTRUE(remove_monotonous_data))
    kpi_df <- kpi_df %>%
      select_if(names(.) %in% c("id", "title", "desctiption") | map(., n_distinct) > 1)

  kpi_df <- kpi_df %>%
    select(id, title, description, everything())

  kpi_df
}

#' Add keywords to KPI table
#'
#' @param kpi_df A KPI table
#' @param n How many keyword columns should be added?
#'
#' @export
kpi_add_keywords <- function(kpi_df, n = 2) {
  keyword_df <- kpis %>%
    mutate(
      title_words = str_split(title, "\\s"),
      title_words = map(title_words, str_remove_all, "[:punct:]$"),
      title_words = map(title_words, function(x) x[!x %in% stopwords()]),
      title_words = map(title_words, function(x, n) x[1:n], n)
    ) %>%
    unnest(cols = c(title_words))

}
