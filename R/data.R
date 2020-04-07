#' Compose a query to fetch metadata from the Kolada API. Its use is mainly
#'
#' Mainly used as a supporting function for \code{get_kld_data()} but can also be used to create a working URL to paste in your web browser.
#'
#' @param kpi What kpis should be fetched? Can be a single name or a vector of names.
#' @param municipality For which municipalities should data be fetched? Can be a single name or a vector of names.
#' @param period For what years should data be fetched? Can be one or more four-digit integers or character strings.
#' @param ou (Optional) for what Operating Units should data be fetched? Only available for certain KPIs.
#' @param version Version of the API. Currently only \code{"v2"} is supported.
#' @export
compose_data_query <- function(kpi = NULL, municipality = NULL, period = NULL, ou = NULL, version = "v2") {

  base_url <- glue::glue("http://api.kolada.se/{version}/data")

  if (is.null(kpi))
    kpi <- ""
  else
    kpi <- paste0("/kpi/", paste(kpi, collapse = ","))

  if (!is.null(municipality) & !is.null(ou))
    warning("RAISE MUNICIPALITY AND OU GIVEN WARNING HERE")

  if (is.null(municipality) & is.null(ou))
      municipality <- ""
  else if(!is.null(municipality))
    municipality <- paste0("/municipality/", paste(municipality, collapse = ","))
  else
    municipality <- paste0("/ou/", paste(ou, collapse = ","))

  if (is.null(period))
    period <- ""
  else
    period <- paste0("/year/", paste(period, collapse = ","))

  if (sum(str_length(c(kpi, municipality, period)) > 0) < 2)
    stop("RAISE TOO FEW PARAMETERS IN DATA QUERY ERROR HERE")

  query_url <- glue::glue("{base_url}{kpi}{municipality}{period}")

  return(URLencode(query_url))
}

#' Get data from the Kolada API
#'
#' Get data from the Kolada API.
#'
#' @param kpi What kpis should be fetched? Can be a single name or a vector of names.
#' @param municipality For which municipalities should data be fetched? Can be a single name or a vector of names.
#' @param period For what years should data be fetched? Can be one or more four-digit integers or character strings.
#' @param ou (Optional) for what Operating Units should data be fetched? Only available for certain KPIs.
#' @param version Version of the API. Currently only \code{"v2"} is supported.
#' @export
get_kld_data <- function(kpi = NULL, municipality = NULL, period = NULL, ou = NULL, simplify = TRUE) {
  query <- compose_data_query(kpi, municipality, period, ou)

  res <- httr::GET(query)

  contents_raw <- httr::content(res, as = "text")
  contents <- jsonlite::fromJSON(contents_raw)[["values"]]
  ret <- tibble::as_tibble(contents) %>% unnest(cols = c(values))

  if (isTRUE(simplify)) {
    ret <- ret %>%
      # Remove "status" column (does it ever contain anything?)
      select(-status) %>%
      # Convert codes to names
      mutate(
        municipality_id = municipality,
        municipality = code_to_municipality(municipality)
      )
  }

  ret
}
