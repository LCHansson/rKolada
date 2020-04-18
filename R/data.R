#' Compose a query to fetch metadata from the Kolada API. Its use is mainly
#'
#' Mainly used as a supporting function for \code{link{get_values}} but can also be
#' used to create a working URL to paste in your web browser.
#'
#' @param kpi What kpis should be fetched? Can be a single name or a vector of
#'  names.
#' @param municipality For which municipalities should data be fetched? Can be a
#' single name or a vector of names.
#' @param period For what years should data be fetched? Can be one or more
#' four-digit integers or character strings.
#' @param ou (Optional) for what Operating Units should data be fetched? Only
#' available for certain KPIs.
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

  if (sum(stringr::str_length(c(kpi, municipality, period)) > 0) < 2)
    stop("RAISE TOO FEW PARAMETERS IN DATA QUERY ERROR HERE")

  query_url <- glue::glue("{base_url}{kpi}{municipality}{period}")

  return(utils::URLencode(query_url))
}

#' Get data from the Kolada API
#'
#' Get data from the Kolada API.
#'
#' @param kpi What kpis should be fetched? Can be a single name or a vector of
#' names.
#' @param municipality For which municipalities should data be fetched? Can be a
#' single name or a vector of names.
#' @param period For what years should data be fetched? Can be one or more
#' four-digit integers or character strings.
#' @param ou (Optional) for what Operating Units should data be fetched? Only
#' available for certain KPIs.
#' @param simplify Whether to make results more human readable.
#'
#' @export
get_values <- function(kpi = NULL, municipality = NULL, period = NULL, ou = NULL, simplify = TRUE) {
  query <- compose_data_query(kpi, municipality, period, ou)

  res <- httr::GET(query)

  contents_raw <- httr::content(res, as = "text")
  contents <- jsonlite::fromJSON(contents_raw)[["values"]]
  ret <- tibble::as_tibble(contents) %>%
    tidyr::unnest(cols = c(values))

  if (isTRUE(simplify)) {
    ret <- ret %>%
      # Remove "status" column (does it ever contain anything?)
      dplyr::select(-status) %>%
      # Convert codes to names
      dplyr::mutate(
        municipality_id = municipality,
        municipality = municipality_id_to_name(get_municipality(), municipality)
      ) %>%
      dplyr::rename(year = period)
  }

  ret
}

#' Simplify a Kolada value table
#'
#'
#'
#' @param values_df A Kolada value table, as created by \code{\link{get_values}}.
#'
#' @export

values_minimize <- function(values_df) {
  mins <- values_df %>%
    purrr::map(dplyr::n_distinct)

  drop <- names(mins[mins == 1])

  values_df %>%
    dplyr::select_if(names(.) %in% c("kpi", "municipality", "value") | purrr::map(., dplyr::n_distinct) > 1)
}

#' Create KPI long-form descriptions to add to a plot
#'
#'
#'
#' @param values_df A Kolada value table, as created by \code{\link{get_values}}.
#' @param kpi_df A KPI table, e.g. as created by \code{\link{get_kpi}}.
#'
#' @export
values_legend <- function(values_df, kpi_df) {
  kpis <- unique(values_df$kpi)
  desc <- kpi_df %>%
    dplyr::select(id, title) %>%
    dplyr::filter(id %in% kpis)

  paste(glue::glue_data(desc, "{id}: {title}"), collapse = "\n")
}