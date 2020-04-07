#' Get a table of municipalities from the Kolada metadata API
#'
#' @param id (Optional) One or several municipality IDs
#' @param cache (Optional) If you plan on doing multiple concurrent calls to retrieve the same data from the Kolada API (this is usually the case) you can use this option to store downloaded metadata to disk. If you call the function again, data will be read from disk instead of retrieving it over the internet if a cache file can be located. If set to \code{FALSE} or \code{"no"}, don't store any data on disk. If set to \code{TRUE} or \code{"yes"} or \code{"tempfile"}, store data in a \code{tempfile} (this will expire at the end of your R session). If set to \code{"wd"}, data will be stored in \code{.RData} files in your current working directory.
#'
#' @examples
#'
#' \dontrun{
#' # Download KPI table and store a cache copy of the results in your current working directory
#' kpi_df <- municipality_get(cache = "wd")
#'
#' # Download KPI table and store a cache copy of the results in a tempfile (will expire when your R session ends)
#' kpi_df <- municipality_get(cache = TRUE)
#' }
#'
#' @export
municipality_get <- function(id = NULL, cache = FALSE) {
  get_kld_metadata(entity = "municipality", id = id, cache = cache)
}

#' Get a table of municipality groups from the Kolada metadata API
#'
#' @param id (Optional) One or several municipality group IDs
#' @param cache (Optional) If you plan on doing multiple concurrent calls to retrieve the same data from the Kolada API (this is usually the case) you can use this option to store downloaded metadata to disk. If you call the function again, data will be read from disk instead of retrieving it over the internet if a cache file can be located. If set to \code{FALSE} or \code{"no"}, don't store any data on disk. If set to \code{TRUE} or \code{"yes"} or \code{"tempfile"}, store data in a \code{tempfile} (this will expire at the end of your R session). If set to \code{"wd"}, data will be stored in \code{.RData} files in your current working directory.
#'
#' @examples
#'
#' \dontrun{
#' # Download KPI table and store a cache copy of the results in your current working directory
#' kpi_df <- municipality_groups_get(cache = "wd")
#'
#' # Download KPI table and store a cache copy of the results in a tempfile (will expire when your R session ends)
#' kpi_df <- municipality_groups_get(cache = TRUE)
#' }
#'
#' @export
municipality_groups_get <- function(id = NULL, cache = FALSE) {
  get_kld_metadata(entity = "municipality_groups", id = id, cache = cache)
}

#' Convert municipality names to municipality codes
#'
#' @param municipality Name of one or several municipalities. Case insensitive. Allows repeats.
#' @param remove_na Should NA return values be removed?
#' @param cache cache (Optional) If you plan on doing multiple concurrent calls to retrieve the same data from the Kolada API (this is usually the case) you can use this option to store downloaded metadata to disk. If you call the function again, data will be read from disk instead of retrieving it over the internet if a cache file can be located. If set to \code{FALSE} or \code{"no"}, don't store any data on disk. If set to \code{TRUE} or \code{"yes"} or \code{"tempfile"}, store data in a \code{tempfile} (this will expire at the end of your R session). If set to \code{"wd"}, data will be stored in \code{.RData} files in your current working directory.
#'
#' @examples
#' municipality_to_code(c("Malmö", "Lund", "Stockholm", "Malmö"))
#'
#' @export
municipality_to_code <- function(municipality, remove_na = FALSE, cache = FALSE) {
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


#' Convert municipality codes to municipality names
#'
#' @param code ID codes of one or several municipalities. Allows repeats.
#' @param remove_na Should NA return values be removed?
#' @param cache cache (Optional) If you plan on doing multiple concurrent calls to retrieve the same data from the Kolada API (this is usually the case) you can use this option to store downloaded metadata to disk. If you call the function again, data will be read from disk instead of retrieving it over the internet if a cache file can be located. If set to \code{FALSE} or \code{"no"}, don't store any data on disk. If set to \code{TRUE} or \code{"yes"} or \code{"tempfile"}, store data in a \code{tempfile} (this will expire at the end of your R session). If set to \code{"wd"}, data will be stored in \code{.RData} files in your current working directory.
#'
#' @examples
#' code_to_municipality(c("1280", "1281", "0180", "1280"))
#'
#' @export
code_to_municipality <- function(code, remove_na = FALSE, cache = FALSE) {

  munic_df <- municipality_get(cache = TRUE)

  res <- munic_df$title[match(code, tolower(munic_df$id))]
  names(res) <- munic_df$id[match(code, tolower(munic_df$id))]

  unused_names <- unique(code[which(!code %in% munic_df$id)])
  unused_names <- unused_names[!is.na(unused_names)]
  if (length(unused_names) > 0)
    warning("The following ids were not found in municipality data:\n- ", paste(unused_names, collapse = "\n- "))

  if (isTRUE(remove_na))
    res <- res[!is.na(res)]

  res
}
