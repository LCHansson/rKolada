#' Compose a query to fetch metadata from the Kolada API.
#'
#' Mainly used as a supporting function for \code{\link{get_metadata}} but can
#' also be used to create a working URL to paste in your web browser.
#'
#' @param entity Any allowed metadata entity. Check \code{\link{allowed_entities}} to
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

#' Download metadata from the Kolada API
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
#' @param cache Logical. If TRUE, downloaded data are stored to the local disk
#' in the place specified by \code{cache_location}. If data is already present
#' on the local disk, this data is returned instead of downloading data from the
#' API.
#' @param cache_location Where to store and search for cached data. Can be a
#' path to a directory or the name of any function that returns the path to a
#' directory when called, like \code{link{getwd}}. Defaults to
#' \code{\link{tempdir}}.
#'
#' @return Returns a tibble with metadata for the specified entity. In rKolada
#' terminology, a table returned by e.g. \code{entity = "kpi"} is referred to
#' as a \code{kpi_df} and can be passed to functions starting with "kpi" such
#' as \code{\link{kpi_bind_keywords}}.
#'#'
#' @seealso \code{\link{get_kpi}}, \code{\link{get_kpi_groups}}, \code{\link{get_municipality}}, \code{\link{get_municipality_groups}}, \code{\link{get_ou}}
#'
#' @export
get_metadata <- function(entity = "kpi", title = NULL, id = NULL, municipality = NULL, cache = FALSE, cache_location = tempdir) {
  ch <- cache_handler(entity, cache, cache_location)

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


#' Download metadata for a specific entity from the Kolada API
#'
#' There are five different types of metadata entities in the Kolada database:
#' "kpi", "kpi_groups", "municipality", "municipality_groups", and "ou". For
#' every entity there is a corresponding function \code{get_ENTITY} which
#' retrieves a table with the metadata for that entity. The \code{get_ENTITY}
#' functions are thin wrappers around \code{\link{get_metadata}}.
#'
#' @param id (Optional) One or several KPI IDs
#' @param cache Logical. If TRUE, downloaded data are stored to the local disk
#'  in the place specified by \code{cache_location}. If data is already present
#'  on the local disk, this data is returned instead of downloading data from the
#'   API.
#' @param cache_location Where to store and search for cached data. Can be a path
#'  to a directory or the name of any function that returns the path to a
#'  directory when called, like \code{link{getwd}}. Defaults to
#'  \code{\link{tempdir}}.
#' @param municipality (Optional) A string or vector of strings cantaining
#' municipality codes. If getting OU data, you can use this parameter to narrow
#' the search.
#'
#' @return Returns a tibble with metadata for the specified entity. In rKolada
#' terminology, a table returned by e.g. \code{\link{get_kpi}} is referred to
#' as a \code{kpi_df} and can be passed to functions starting with "kpi" such
#' as \code{\link{kpi_bind_keywords}}.
#'
#' @examples
#'
#' \dontrun{
#' # Download KPI table and store a cache copy of the results in
#' # your current working directory
#' kpi_df <- get_kpi(cache = "wd")
#'
#' # Download KPI table and store a cache copy of the results in
#' # a tempfile (will expire when your R session ends)
#' kpi_df <- get_kpi(cache = TRUE)
#' }
#'
#' @export
get_kpi <- function(id = NULL, cache = FALSE, cache_location = tempdir) {
  get_metadata(entity = "kpi", id = id, cache = cache, cache_location = cache_location)
}


#' @export
#' @rdname get_kpi
get_kpi_groups <- function(id = NULL, cache = FALSE, cache_location = tempdir) {
  get_metadata(entity = "kpi_groups", id = id, cache = cache, cache_location = cache_location)
}

#' @export
#' @rdname get_kpi
get_ou <- function(id = NULL, municipality = NULL, cache = FALSE, cache_location = tempdir) {
  get_metadata(entity = "ou", id = id, municipality = municipality, cache = cache, cache_location = cache_location) %>%
    dplyr::mutate(municipality_id = municipality, municipality = municipality_id_to_name(get_municipality(cache = cache, cache_location = cache_location), municipality_id)) %>%
    dplyr::select(id, title, municipality, municipality_id)
}


#' @export
#' @rdname get_kpi
get_municipality <- function(id = NULL, cache = FALSE, cache_location = tempdir) {
  get_metadata(entity = "municipality", id = id, cache = cache, cache_location = cache_location)
}

#' @export
#' @rdname get_kpi
get_municipality_groups <- function(id = NULL, cache = FALSE, cache_location = tempdir) {
  get_metadata(entity = "municipality_groups", id = id, cache = cache, cache_location = cache_location)
}
