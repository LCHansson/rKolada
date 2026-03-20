#' Convert a vector of municipality names to municipality ids
#'
#' Given a vector of names of municipalities or regions, return a named vector
#' of municipality IDs/codes. Codes of municipalities and regions follow
#' the Swedish standard for municipality codes. The codes extracted can be used
#' e.g. to pass as a parameter to [get_values()]. This function is the
#' inverse to [municipality_id_to_name()].
#'
#' @param munic_df A Kolada Municipality metadata table, as created by e.g.
#' `get_municipality`.
#' @param municipality Name of one or several municipalities. Case insensitive.
#' Allows repeats.
#' @param remove_na Should NA return values be removed?
#'
#' @seealso [municipality_extract_ids()],
#' [municipality_id_to_name()]
#'
#' @examples
#' if (kolada_available()) {
#' munic_df <- get_municipality()
#' munic_df |>
#'   municipality_name_to_id(c("Arboga", "Lund", "Stockholm", "Arboga"))
#' }
#'
#' @return A vector of Municipality IDs.
#'
#' @export
municipality_name_to_id <- function(munic_df, municipality, remove_na = FALSE) {

  if (is.null(munic_df)) {
    cli::cli_warn("An empty object was used as input to {.fn municipality_name_to_id}.")
    return(NULL)
  }

  munic_names <- tolower(municipality)

  res <- munic_df$id[match(munic_names, tolower(munic_df$title))]
  names(res) <- munic_df$title[match(munic_names, tolower(munic_df$title))]

  unused_names <- unique(municipality[which(
    !munic_names %in% tolower(munic_df$title)
  )])
  unused_names <- unused_names[!is.na(unused_names)]
  if (length(unused_names) > 0)
    cli::cli_warn(c(
      "The following names were not found in municipality data:",
      "x" = "{.val {unused_names}}"
    ))

  if (isTRUE(remove_na))
    res <- res[!is.na(res)]

  res
}

#' Convert a vector of municipality ids to municipality names
#'
#' Given a vector of municipality IDs/codes, return a named vector
#' of names of municipalities or regions. Codes of municipalities and regions
#' follow
#' the Swedish standard for municipality codes. The codes extracted can be used
#' e.g. to pass as a parameter to [get_values()]. This function is the
#' inverse to [municipality_name_to_id()].
#'
#' @param munic_df A Kolada Municipality metadata table, as created by e.g.
#' `get_municipality`.
#' @param id ID ids of one or several municipalities. Allows repeats.
#' @param remove_na Should NA return values be removed?
#'
#' @seealso [municipality_extract_ids()],
#' [municipality_name_to_id()]
#'
#' @examples
#' if (kolada_available()) {
#' munic_df <- get_municipality()
#' municipality_id_to_name(munic_df, c("1280", "1281", "0180", "1280"))
#' }
#'
#' @return A vector of Municipality names.
#'
#' @export
municipality_id_to_name <- function(munic_df, id, remove_na = FALSE) {

  if (is.null(munic_df)) {
    cli::cli_warn("An empty object was used as input to {.fn municipality_id_to_name}.")
    return(NULL)
  }

  res <- munic_df$title[match(id, tolower(munic_df$id))]
  names(res) <- munic_df$id[match(id, tolower(munic_df$id))]

  unused_names <- unique(id[which(!id %in% munic_df$id)])
  unused_names <- unused_names[!is.na(unused_names)]
  if (length(unused_names) > 0)
    cli::cli_warn(c(
      "The following IDs were not found in municipality data:",
      "x" = "{.val {unused_names}}"
    ))

  if (isTRUE(remove_na))
    res <- res[!is.na(res)]

  res
}


#' Extract a vector of municipality ID strings from a Kolada municipality table
#'
#' This function is primarily intended as a convenient way to pass a (filtered)
#' Kolada municipality metadata table to [get_values()].
#'
#' @param munic_df A Kolada Municipality metadata table, as created by e.g.
#' `get_municipality`.
#'
#' @examples
#' if (kolada_available()) {
#' # Download Kolada data for all municipalities of type "L"
#' # (regions and national total) for KPI "N45933"
#' munic_filter <- get_municipality() |>
#'   municipality_search("L", column = "type")
#'
#' kld_data <- get_values(
#'   kpi = "N45933",
#'   municipality = municipality_extract_ids(munic_filter),
#'   period = 2022
#' )
#' }
#'
#' @export
municipality_extract_ids <- function(munic_df) {

  if (is.null(munic_df)) {
    cli::cli_warn("An empty object was used as input to {.fn municipality_extract_ids}.")
    return(NULL)
  }

  munic_df$id
}


#' Search a Kolada municipality metadata table
#'
#' Search a Kolada municipality metadata table. Only keep rows that contain the
#' search query. Note that some a quer might be both the name, or part of a
#' name, of a municipality and part of the name of a region. Thus, a search
#' might return rows for both municipalities and regions. To avoid this you can
#' use [dplyr::filter] to filter the `type`
#' column to keep only "K" (municipalities) or "L" (regions) rows. See also
#' examples below for an alternative approach avoiding any direct calls to
#' `filter`.
#'
#' @param munic_df A Kolada Municipality metadata table, as created by e.g.
#' `get_municipality`.
#' @param query A search term or a vector of search terms to filter by. Case
#' insensitive.
#' @param column (Optional) A string or character vector with the names of
#' columns in which to search for `query`.
#'
#' @return A Kolada Municipality metadata table
#'
#' @examples
#' if (kolada_available()) {
#' # Search for a single search term in a municipality table
#' munic_df <- get_municipality()
#' municipality_search(munic_df, "Arboga")
#'
#' # Only keep columns with type == "K" (keep municipalities, drop regions)
#' munic_filter <- get_municipality(cache = TRUE) |>
#'   municipality_search("K", column = "type")
#' }
#'
#' @export
municipality_search <- function(munic_df, query, column = NULL) {
  entity_search(munic_df, query, column, "municipality_search")
}
