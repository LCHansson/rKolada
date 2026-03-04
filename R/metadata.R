#' Compose a query to fetch metadata from the Kolada API.
#'
#' Mainly used as a supporting function for \code{\link{get_metadata}} but can
#' also be used to create a working URL to paste in your web browser.
#'
#' @param entity Any allowed metadata entity. Check
#' \code{\link{allowed_entities}} to see an updated list.
#' @param title A free-form search term or the exact title of any entry in the
#' current entity. Case insensitive.
#' @param id The ID of any entry in the current entity.
#' @param municipality If entity is \code{"ou"}, the municipality parameter can
#' be added to narrow the search.
#' @param page What page to fetch. Used mainly in large queries. Fetches a page using the value of \code{"per_page"} as pagination delimiter.
#' @param per_page Number of results per page.
#' @param region_type (Optional) Filter municipalities by region type. Only
#' used when \code{entity} is \code{"municipality"}. Common values: \code{"K"}
#' (municipality), \code{"L"} (region).
#' @param version Version of the API. Defaults to \code{"v3"}.
#'
#' @return A string containing a URL to the Kolada REST API.
#'
#' @export
compose_metadata_query <- function(
  entity = "kpi",
  title = NULL,
  id = NULL,
  municipality = NULL,
  region_type = NULL,
  page = NA,
  per_page = NA,
  version = "v3"
) {
  if (!is.null(entity))
    entity <- tolower(entity)
  else
    stop("No entity was specified. You must specify an entity.")

  if(!entity %in% allowed_entities())
    stop("The specified entity is no in the list of valid entities. Please check your spelling.\nFor a list of allowed entities, please see allowed_entities()")

  base_url <- glue::glue("https://api.kolada.se/{version}/{entity}")
  query_url <- glue::glue("{base_url}")

  if (!is.null(title)) {
    if (length(title) > 1) {
      title <- title[1]
      warning("You have specified a title vector of length > 1. Only the first element in the vector will be used.")
    }

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

  if (entity == "municipality" & !is.null(region_type)) {
    if (!is.null(title))
      separator <- "&"
    else if (grepl("\\?", query_url))
      separator <- "&"
    else
      separator <- "?"
    query_url <- glue::glue("{query_url}{separator}region_type={region_type}")
  }

  query_url <- append_query_params(query_url, page = page, per_page = per_page)

  return(utils::URLencode(query_url))
}

#' Download metadata from the Kolada API
#'
#' This is a generalized function for downloading metadata from the Kolada API.
#' The function parameters closely mask the names specified in the original API.
#' For further information about the Kolada API specification, please see the
#' \href{https://github.com/Hypergene/kolada}{official documentation on GitHub}.
#'
#' @param entity Any allowed metadata entity. Check \code{allowed_entities()} to
#' see an updated list.
#' @param title A free-form search term or the exact title of any entry in the
#' current entity. Case insensitive.
#' @param id The ID of any entry in the current entity.
#' @param municipality If entity is \code{"ou"}, the municipality parameter can
#' be added to narrow the search.
#' @param region_type (Optional) Filter municipalities by region type. Only
#' used when \code{entity} is \code{"municipality"}. Common values: \code{"K"}
#' (municipality), \code{"L"} (region).
#' @param max_results (Optional) Specify the maximum number of results
#'  returned by the query.
#' @param cache Logical. If TRUE, downloaded data are stored to the local disk
#' in the place specified by \code{cache_location}. If data is already present
#' on the local disk, this data is returned instead of downloading data from the
#' API.
#' @param cache_location Where to store and search for cached data. Can be a
#' path to a directory or the name of any function that returns the path to a
#' directory when called, like \code{\link{getwd}}. Defaults to
#' \code{\link{tempdir}}.
#' @param verbose Whether to print the call to the Kolada API as a message to
#' the R console.
#'
#' @return Returns a tibble with metadata for the specified entity. In rKolada
#' terminology, a table returned by e.g. \code{entity = "kpi"} is referred to
#' as a \code{kpi_df} and can be passed to functions starting with "kpi" such
#' as \code{\link{kpi_bind_keywords}}.
#'
#' @seealso \code{\link{get_kpi}}, \code{\link{get_kpi_groups}},
#'  \code{\link{get_municipality}},
#'  \code{\link{get_municipality_groups}},
#'  \code{\link{get_ou}}
#'
#' @export
get_metadata <- function(
  entity = "kpi",
  title = NULL,
  id = NULL,
  municipality = NULL,
  region_type = NULL,
  max_results = NULL,
  cache = FALSE,
  cache_location = tempdir,
  verbose = FALSE
) {
  ch <- cache_handler(entity, cache, cache_location)

  if (ch("discover"))
    return(ch("load"))

  if (isTRUE(verbose))
    message("Downloading Kolada metadata using URL(s):")

  # Chunk id to stay within the API's 25-element-per-path-segment limit
  id_chunks <- chunk_vector(id)

  all_vals <- list()

  for (id_c in id_chunks) {

    has_next <- TRUE
    page <- 1
    per_page <- 5000

    while(isTRUE(has_next)) {

      if(!is.null(max_results) && page * per_page > max_results)
        page_size <- max_results %% per_page
      else
        page_size <- per_page

      query <- compose_metadata_query(entity, title, id_c, municipality,
                                      region_type = region_type,
                                      page = page, per_page = page_size)

      if (isTRUE(verbose))
        message(query)

      res <- try(
        httr2::request(query) |>
          httr2::req_error(is_error = function(resp) FALSE) |>
          httr2::req_perform(),
        silent = TRUE
      )

      if(inherits(res, "try-error")) {
        warning("\nCould not connect to the Kolada database. Please check your internet connection. Did you misspel the query?\nRe-run query with verbose = TRUE to see the URL used in the query.")
        return(NULL)
      }

      contents_raw <- httr2::resp_body_string(res)
      contents <- try(jsonlite::fromJSON(contents_raw), silent = TRUE)

      if(inherits(contents, "try-error")) {
        warning("\nKolada returned a 404 or malformatted HTML/JSON. Did you misspel the query?\nRe-run query with verbose = TRUE to see the URL used in the query.")
        return(NULL)
      }

      if (length(contents$values) == 0)
        break

      if(page == 1)
        chunk_vals <- tibble::as_tibble(contents$values)
      else
        chunk_vals <- dplyr::bind_rows(chunk_vals, tibble::as_tibble(contents$values))

      if(is.null(contents$next_url) || contents$next_url == "")
        has_next <- FALSE
      else
        page <- page + 1

      if(!is.null(max_results) && nrow(chunk_vals) >= max_results) {
        chunk_vals <- utils::head(chunk_vals, max_results)
        has_next <- FALSE
      }
    }

    if (exists("chunk_vals", inherits = FALSE)) {
      all_vals <- c(all_vals, list(chunk_vals))
      rm(chunk_vals)
    }
  }

  if (length(all_vals) == 0)
    vals <- tibble::tibble()
  else
    vals <- dplyr::bind_rows(all_vals)

  if (!is.null(max_results) && nrow(vals) > max_results)
    vals <- utils::head(vals, max_results)

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
#' @param max_results (Optional) Specify the maximum number of results
#'  returned by the query.
#' @param cache Logical. If TRUE, downloaded data are stored to the local disk
#'  in the place specified by \code{cache_location}. If data is already present
#'  on the local disk, this data is returned instead of downloading data from
#'  the API.
#' @param cache_location Where to store and search for cached data. Can be a
#'  path to a directory or the name of any function that returns the path to a
#'  directory when called, like \code{\link{getwd}}. Defaults to
#'  \code{\link{tempdir}}.
#' @param municipality (Optional) A string or vector of strings containing
#' municipality codes. If getting OU data, you can use this parameter to narrow
#' the search.
#' @param verbose Whether to print the call to the Kolada API as a message to
#' the R console.
#'
#' @return Returns a tibble with metadata for the specified entity. In rKolada
#' terminology, a table returned by e.g. \code{\link{get_kpi}} is referred to
#' as a \code{kpi_df} and can be passed to functions starting with "kpi" such
#' as \code{\link{kpi_bind_keywords}}.
#'
#' @examples
#' # Download KPI table and store a cache copy of the results in a temporary folder
#' # (to actually download all available data, don't specify max_results)
#' if (kolada_available()) {
#' kpi_df <- get_kpi(cache = TRUE, max_results = 100)
#' }
#'
#' @export
get_kpi <- function(
  id = NULL, max_results = NULL, cache = FALSE,
  cache_location = tempdir, verbose = FALSE
) {
  get_metadata(
    entity = "kpi", id = id, max_results = max_results, cache = cache,
    cache_location = cache_location, verbose = verbose
  )
}

#' @export
#' @rdname get_kpi
get_kpi_groups <- function(
  id = NULL, cache = FALSE, max_results = NULL,
  cache_location = tempdir, verbose = FALSE
) {
  get_metadata(
    entity = "kpi_groups", id = id, max_results = max_results, cache = cache,
    cache_location = cache_location, verbose = verbose
  )
}

#' @export
#' @rdname get_kpi
get_ou <- function(
  id = NULL, municipality = NULL, max_results = NULL, cache = FALSE,
  cache_location = tempdir, verbose = FALSE
) {
  munic_df <- get_municipality(cache = cache, cache_location = cache_location)

  get_metadata(
    entity = "ou", id = id, max_results = max_results, municipality = municipality,
    cache = cache, cache_location = cache_location, verbose = verbose
  ) %>%
    dplyr::mutate(
      municipality_id = .data$municipality,
      municipality = municipality_id_to_name(.env$munic_df, .data$municipality_id)
    ) %>%
    dplyr::select("id", "title", "municipality", "municipality_id")
}


#' @param region_type (Optional) Filter municipalities by region type. Common
#' values: \code{"K"} (municipality), \code{"L"} (region).
#' @export
#' @rdname get_kpi
get_municipality <- function(
  id = NULL, region_type = NULL, cache = FALSE, max_results = NULL,
  cache_location = tempdir, verbose = FALSE
) {
  get_metadata(
    entity = "municipality", id = id, region_type = region_type,
    max_results = max_results, cache = cache,
    cache_location = cache_location, verbose = verbose
  )
}

#' @export
#' @rdname get_kpi
get_municipality_groups <- function(
  id = NULL, cache = FALSE, max_results = NULL,
  cache_location = tempdir, verbose = FALSE
) {
  get_metadata(
    entity = "municipality_groups", id = id, max_results = max_results, cache = cache,
    cache_location = cache_location, verbose = verbose
  )
}
