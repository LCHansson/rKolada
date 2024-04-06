#' Search a Kolada Organizational Unit metadata table
#'
#' Search a Kolada Organizational Unit metadata table. Only keep rows that
#' contain the search query. Matches against all columns or columns named
#' with the \code{column} parameter. For more precise matching, please use
#' \code{\link[dplyr:filter]{dplyr::filter}}.
#'
#' @param ou_df A Kolada Organizational Unit metadata table, as created by e.g.
#'  \code{get_municipality}.
#' @param query A search term or a vector of search terms to filter by. Case
#'  insensitive.
#' @param column (Optional) A string or character vector with the names of
#'  columns in which to search for \code{query}.
#'
#' @return A Kolada Organizational Unit metadata table
#'
#' @examples
#' if (kolada_available()) {
#' # Search for all OUs matching the search term "skola" (school)
#' # (skip the parameter "max_results" to actually download all data)
#' ou_df <- get_ou(max_results = 100)
#' ou_search(ou_df, "skola")
#'
#' # Only keep OU entities matching "skola" but not "förskola" (preschool)
#' # located in Gothenburg municipality and starting with an "A" using
#' # regex matching
#' ou_filter <- get_ou(municipality = "1480") %>%
#'   ou_search("^A", column = "title") %>%
#'   ou_search("[^(för)]skola")
#' }
#'
#' @export
ou_search <- function(ou_df, query, column = NULL) {

  if (is.null(ou_df)) {
    warning("\nAn empty object was used as input to ou_search().")
    return(NULL)
  }

  if (is.null(column))
    column <- names(ou_df)

  f <- function(obj, query) {
    stringr::str_detect(
      tolower(obj),
      tolower(as.character(paste(query, collapse = "|")))
    )
  }

  hits <- ou_df %>%
    dplyr::filter_at(
      .vars = dplyr::vars(column),
      .vars_predicate = dplyr::any_vars(f(., query))
    )

  hits
}