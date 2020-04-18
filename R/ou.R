#' Search a Kolada Operating unit table
#'
#'
#'
#' @param ou_df A Kolada Municipality metadata table, as created by e.g. \code{get_municipality}.
#' @param query A search term or a vector of search terms to filter by. Case insensitive.
#' @param columns (Optional) A string or character vector with the names of
#' columns in which to search for \code{query}.
#'
#' @examples
#' \dontrun{
#' # Search for a single search term in a municipality table
#' ou_df <- get_ou()
#' ou_search(ou_df, "skola")
#'
#' # Only keep OU entities located in Stockholm municipality
#' ou_filter <- get_ou() %>%
#'   ou_search("Stockholm", column = "municipality")
#' }
#'
#' @export
ou_search <- function(ou_df, query, column = NULL) {
  if (is.null(column))
    column <- names(ou_df)

  f <- function(obj, query) {
    stringr::str_detect(tolower(obj), tolower(as.character(paste(query, collapse = "|"))))
  }

  hits <- ou_df %>%
    dplyr::filter_at(
      .vars = dplyr::vars(column),
      .vars_predicate = dplyr::any_vars(f(., query))
    )

  hits
}