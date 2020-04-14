#' Compose a query to fetch metadata from the Kolada API.
#'
#' Mainly used as a supporting function for \code{get_kld_metadata()} but can
#' also be used to create a working URL to paste in your web browser.
#'
#' @param entity Any allowed metadata entity. Check \code{allowed_entities()} to
#' see an updated list.
#' @param title A free-form search term or the exact title of any entry in the
#' current entity. Case insensitive.
#' @param id The ID of any entry in the current entity.
#' @param municipality If entity is \code{"ou"}, the municipality parameter can
#' be added to narrow the search.
#' @param version Version of the API. Currently only \code{"v2"} is supported.
#' @export
compose_metadata_query <- function(entity = "kpi", title = NULL, id = NULL, municipality = NULL, version = "v2") {
  if (!is.null(entity))
    entity <- tolower(entity)
  else
    stop("RAISE ENTITY ERROR HERE")

  if(!entity %in% allowed_entities())
    stop("RAISE ENTITY MISSPELLED ERROR HERE")

  base_url <- glue::glue("http://api.kolada.se/{version}/{entity}")
  query_url <- glue::glue("{base_url}")

  if (!is.null(title)) {
    if (length(title) > 1)
      stop("RAISE TITLE LENGTH > 1 ERROR HERE")

    title <- tolower(title)
    query_url <- glue::glue("{query_url}?title={title}")

  } else if (!is.null(id)) {
    id <- paste(id, collapse = ",")
    query_url <- glue::glue("{query_url}/{id}")
  }

  if (entity == "ou" & !is.null(municipality)) {
    if (!is.null(title))
      separator <- "&"
    else
      separator <- "?"
    municipality <- paste(municipality, collapse = ",")
    query_url <- glue::glue("{query_url}{separator}municipality={municipality}")
  }

  return(utils::URLencode(query_url))
}

#' Get metadata from the Kolada API
#'
#' Use this function to take full control of the Kolada metadata API.
#'
#' @param entity Any allowed metadata entity. Check \code{allowed_entities()} to
#' see an updated list.
#' @param title A free-form search term or the exact title of any entry in the
#' current entity. Case insensitive.
#' @param id The ID of any entry in the current entity.
#' @param municipality If entity is \code{"ou"}, the municipality parameter can
#' be added to narrow the search.
#' @param cache If you plan on doing multiple concurrent calls to retrieve the
#' same data from the Kolada API (this is usually the case) you can use this
#' option to store downloaded metadata to disk. If you call the function again,
#' data will be read from disk instead of retrieving it over the internet if a
#' cache file can be located. If set to \code{FALSE} or \code{"no"}, don't store
#' any data on disk. If set to \code{TRUE} or \code{"yes"} or \code{"tempfile"},
#' store data in a \code{tempfile} (this will expire at the end of your R
#' session). If set to \code{"wd"}, data will be stored in \code{.RData} files
#' in your current working directory.
#'
#' @seealso \code{\link{kpi_get}}, \code{\link{kpi_groups_get}}, \code{\link{municipality_get}}, \code{\link{municipality_groups_get}}, \code{\link{ou_get}}
#'
#' @export
get_kld_metadata <- function(entity = "kpi", title = NULL, id = NULL, municipality = NULL, cache = FALSE) {
  ch <- cache_handler(entity, cache)

  if (ch("discover"))
    return(ch("load"))

  query <- compose_metadata_query(entity, title, id, municipality)

  res <- httr::GET(query)

  contents_raw <- httr::content(res, as = "text")
  contents <- jsonlite::fromJSON(contents_raw)[["values"]]

  vals <- tibble::as_tibble(contents)
  vals <- ch("store", vals)

  vals
}


#' Get a table of KPIs from the Kolada metadata API
#'
#' @param id (Optional) One or several KPI IDs
#' @param cache If you plan on doing multiple concurrent calls to retrieve the
#' same data from the Kolada API (this is usually the case) you can use this
#' option to store downloaded metadata to disk. If you call the function again,
#' data will be read from disk instead of retrieving it over the internet if a
#' cache file can be located. If set to \code{FALSE} or \code{"no"}, don't store
#' any data on disk. If set to \code{TRUE} or \code{"yes"} or \code{"tempfile"},
#' store data in a \code{tempfile} (this will expire at the end of your R
#' session). If set to \code{"wd"}, data will be stored in \code{.RData} files
#' in your current working directory.
#' @param municipality (Optional) A string or vector of strings cantaining
#' municipality codes. If getting OU data, you can use this parameter to narrow
#' the search.
#'
#' @examples
#'
#' \dontrun{
#' # Download KPI table and store a cache copy of the results in
#' # your current working directory
#' kpi_df <- kpi_get(cache = "wd")
#'
#' # Download KPI table and store a cache copy of the results in
#' # a tempfile (will expire when your R session ends)
#' kpi_df <- kpi_get(cache = TRUE)
#' }
#'
#' @export
kpi_get <- function(id = NULL, cache = FALSE) {
  get_kld_metadata(entity = "kpi", id = id, cache = cache)
}


#' @export
#' @rdname kpi_get
kpi_groups_get <- function(id = NULL, cache = FALSE) {
  get_kld_metadata(entity = "kpi_groups", id = id, cache = cache)
}

#' @export
#' @rdname kpi_get
ou_get <- function(id = NULL, municipality = NULL, cache = FALSE) {
  get_kld_metadata(entity = "kpi_groups", id = id, municipality = municipality, cache = cache)
}


#' @export
#' @rdname kpi_get
municipality_get <- function(id = NULL, cache = FALSE) {
  get_kld_metadata(entity = "municipality", id = id, cache = cache)
}

#' @export
#' @rdname kpi_get
municipality_groups_get <- function(id = NULL, cache = FALSE) {
  get_kld_metadata(entity = "municipality_groups", id = id, cache = cache)
}
