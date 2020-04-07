
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

  return(URLencode(query_url))
}


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
