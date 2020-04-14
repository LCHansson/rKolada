#' Convert municipality names to municipality ids
#'
#' @param munic_df A Kolada Municipality metadata table, e.g. created by \code{municipality_get}.
#' @param municipality Name of one or several municipalities. Case insensitive. Allows repeats.
#' @param remove_na Should NA return values be removed?
#' @param cache cache (Optional) If you plan on doing multiple concurrent calls to retrieve the same data from the Kolada API (this is usually the case) you can use this option to store downloaded metadata to disk. If you call the function again, data will be read from disk instead of retrieving it over the internet if a cache file can be located. If set to \code{FALSE} or \code{"no"}, don't store any data on disk. If set to \code{TRUE} or \code{"yes"} or \code{"tempfile"}, store data in a \code{tempfile} (this will expire at the end of your R session). If set to \code{"wd"}, data will be stored in \code{.RData} files in your current working directory.
#'
#' @examples
#' municipality_name_to_id(c("Malmö", "Lund", "Stockholm", "Malmö"))
#' municipality_name_to_filter(c("Malmö", "Lund", "Stockholm", "Malmö"))
#'
#' @export
municipality_name_to_id <- function(munic_df, municipality, remove_na = FALSE, cache = FALSE) {
  munic_names <- tolower(municipality)
  munic_df <- municipality_get(cache = cache)

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


#' @rdname municipality_name_to_id
#' @export
municipality_name_to_filter <- municipality_name_to_id


#' Convert municipality ids to municipality names
#'
#' @param munic_df A Kolada Municipality metadata table, e.g. created by \code{municipality_get}.
#' @param id ID ids of one or several municipalities. Allows repeats.
#' @param remove_na Should NA return values be removed?
#' @param cache cache (Optional) If you plan on doing multiple concurrent calls to retrieve the same data from the Kolada API (this is usually the case) you can use this option to store downloaded metadata to disk. If you call the function again, data will be read from disk instead of retrieving it over the internet if a cache file can be located. If set to \code{FALSE} or \code{"no"}, don't store any data on disk. If set to \code{TRUE} or \code{"yes"} or \code{"tempfile"}, store data in a \code{tempfile} (this will expire at the end of your R session). If set to \code{"wd"}, data will be stored in \code{.RData} files in your current working directory.
#'
#' @examples
#' municipality_id_to_name(c("1280", "1281", "0180", "1280"))
#'
#' @export
municipality_id_to_name <- function(munic_df, id, remove_na = FALSE, cache = FALSE) {

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
