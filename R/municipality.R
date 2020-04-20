#' Convert municipality names to municipality ids
#'
#'
#'
#' @param munic_df A Kolada Municipality metadata table, as created by e.g. \code{get_municipality}.
#' @param municipality Name of one or several municipalities. Case insensitive. Allows repeats.
#' @param remove_na Should NA return values be removed?
#'
#' @examples
#' munic_df <- get_municipality()
#' municipality_name_to_id(munic_df, c("Malmö", "Lund", "Stockholm", "Malmö"))
#' munic_df %>% municipality_name_to_id(c("Malmö", "Lund", "Stockholm", "Malmö"))
#'
#' @export
municipality_name_to_id <- function(munic_df, municipality, remove_na = FALSE) {
  munic_names <- tolower(municipality)

  res <- munic_df$id[match(munic_names, tolower(munic_df$title))]
  names(res) <- munic_df$title[match(munic_names, tolower(munic_df$title))]

  unused_names <- unique(municipality[which(!munic_names %in% tolower(munic_df$title))])
  unused_names <- unused_names[!is.na(unused_names)]
  if (length(unused_names) > 0)
    warning("The following names were not found in municipality data:\n- ", paste(unused_names, collapse = "\n- "))

  if (isTRUE(remove_na))
    res <- res[!is.na(res)]

  res
}

#' Convert municipality ids to municipality names
#'
#'
#'
#' @param munic_df A Kolada Municipality metadata table, as created by e.g. \code{get_municipality}.
#' @param id ID ids of one or several municipalities. Allows repeats.
#' @param remove_na Should NA return values be removed?
#'
#' @examples
#' munic_df <- get_municipality()
#' municipality_id_to_name(munic_df, c("1280", "1281", "0180", "1280"))
#'
#' @export
municipality_id_to_name <- function(munic_df, id, remove_na = FALSE) {

  res <- munic_df$title[match(id, tolower(munic_df$id))]
  names(res) <- munic_df$id[match(id, tolower(munic_df$id))]

  unused_names <- unique(id[which(!id %in% munic_df$id)])
  unused_names <- unused_names[!is.na(unused_names)]
  if (length(unused_names) > 0)
    warning("The following ids were not found in municipality data:\n- ", paste(unused_names, collapse = "\n- "))

  if (isTRUE(remove_na))
    res <- res[!is.na(res)]

  res
}


#' Extract a vector of municipality ID strings from a Kolada municipality table
#'
#'
#'
#' @param munic_df A Kolada Municipality metadata table, as created by e.g. \code{get_municipality}.
#'
#' @export
municipality_extract_ids <- function(munic_df) {
  munic_df$id
}


#' Search a Kolada municipality table
#'
#'
#'
#' @param munic_df A Kolada Municipality metadata table, as created by e.g. \code{get_municipality}.
#' @param query A search term or a vector of search terms to filter by. Case insensitive.
#' @param column (Optional) A string or character vector with the names of
#' columns in which to search for \code{query}.
#'
#' @return A Kolada Municipality metadata table
#'
#' @examples
#' \dontrun{
#' # Search for a single search term in a municipality table
#' munic_df <- get_municipality()
#' municipality_search(munic_df, "Malmö")
#'
#' # Only keep columns with type == "K" (keep municipalities, drop regions)
#' munic_filter <- get_municipality(cache = TRUE) %>%
#'   municipality_search("K", column = "type")
#' }
#'
#' @export
municipality_search <- function(munic_df, query, column = NULL) {
  if (is.null(column))
    column <- names(munic_df)

  f <- function(obj, query) {
    stringr::str_detect(tolower(obj), tolower(as.character(paste(query, collapse = "|"))))
  }

  hits <- munic_df %>%
    dplyr::filter_at(
      .vars = dplyr::vars(column),
      .vars_predicate = dplyr::any_vars(f(., query))
    )

  hits
}
