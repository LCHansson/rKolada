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
#' # Search for all OUs matching the search term "skola" (school)
#' ou_df <- get_ou()
#' ou_search(ou_df, "skola")
#'
#' # Only keep OU entities matching "skola" located in Stockholm municipality
#' ou_filter <- get_ou() %>%
#'   ou_search("Stockholm", column = "municipality") %>%
#'   ou_search("skola")
#'
#' @export
ou_search <- function(ou_df, query, column = NULL) {
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