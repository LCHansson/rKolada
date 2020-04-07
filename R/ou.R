#' Get a table of Operating Units from the Kolada metadata API
#'
#' @param id (Optional) One or several Operating Unit IDs
#' @param cache (Optional) If you plan on doing multiple concurrent calls to retrieve the same data from the Kolada API (this is usually the case) you can use this option to store downloaded metadata to disk. If you call the function again, data will be read from disk instead of retrieving it over the internet if a cache file can be located. If set to \code{FALSE} or \code{"no"}, don't store any data on disk. If set to \code{TRUE} or \code{"yes"} or \code{"tempfile"}, store data in a \code{tempfile} (this will expire at the end of your R session). If set to \code{"wd"}, data will be stored in \code{.RData} files in your current working directory.
#'
#' @examples
#'
#' \dontrun{
#' # Download KPI table and store a cache copy of the results in your current working directory
#' kpi_df <- ou_get(cache = "wd")
#'
#' # Download KPI table and store a cache copy of the results in a tempfile (will expire when your R session ends)
#' kpi_df <- ou_get(cache = TRUE)
#' }
#'
#' @export
ou_get <- function(id = NULL, municipality = NULL, cache = FALSE) {
  get_kld_metadata(entity = "kpi_groups", id = id, municipality = municipality, cache = cache)
}
