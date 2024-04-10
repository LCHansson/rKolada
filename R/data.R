#' Compose a query to fetch metadata from the Kolada API. Its use is mainly
#'
#' Mainly used as a supporting function for \code{\link{get_values}} but can also
#' be used to create a working URL to paste in your web browser.
#'
#' @param kpi What kpis should be fetched? Can be a single name or a vector of
#'  names.
#' @param municipality For which municipalities should data be fetched? Can be a
#' single name or a vector of names.
#' @param period For what years should data be fetched? Can be one or more
#' four-digit integers or character strings.
#' @param ou (Optional) for what Operating Units should data be fetched? Only
#' available for certain KPIs. Only used if \code{unit_type} is set to \code{"ou"}.
#' @param unit_type One of \code{"municipality"} or \code{"ou"}. Whether to
#' fetch data for Municipalities or Organizational Units.
#' Units. Defaults to \code{"municipality"}.
#' @param page What page to fetch. Used mainly in large queries. Fetches a page using the value of \code{"per_page"} as pagination delimiter.
#' @param per_page Number of results per page.
#' @param version Version of the API. Currently only \code{"v2"} is supported.
#'
#' @return A string containing a URL to the Kolada REST API.
compose_data_query <- function(
  kpi = NULL,
  municipality = NULL,
  period = NULL,
  ou = NULL,
  unit_type = "municipality",
  page = NA,
  per_page = NA,
  version = "v2"
) {
  unit_type <- tolower(unit_type)
  if (!unit_type %in% c("municipality", "ou"))
    stop("argument to 'unit_type' must be one of 'municipality' or 'ou'.")

  if (unit_type == "municipality")
    base_url <- glue::glue("http://api.kolada.se/{version}/data")
  else if (unit_type == "ou")
    base_url <- glue::glue("http://api.kolada.se/{version}/oudata")

  if (is.null(kpi))
    kpi <- ""
  else
    kpi <- paste0("/kpi/", paste(kpi, collapse = ","))

  if (!is.null(municipality) & !is.null(ou))
    warning("Both 'municipality' and 'ou' are specified. Only using ", unit_type, ".")

  if(unit_type == "municipality") {
    if (is.null(municipality))
      unit <- ""
    else
      unit <- paste0(
        "/municipality/",
        paste(municipality, collapse = ",")
      )
  }

  if(unit_type == "ou") {
    if (is.null(ou))
      unit <- ""
    else
      unit <- paste0(
        "/ou/",
        paste(ou, collapse = ",")
      )
  }

  if (is.null(period))
    period <- ""
  else
    period <- paste0("/year/", paste(period, collapse = ","))

  if (sum(stringr::str_length(c(kpi, unit, period)) > 0) < 2)
    stop("Too few parameters specified! At least two of the following parameters must have non-empty values: kpi, period, (municipality OR ou).")

  query_url <- glue::glue("{base_url}{kpi}{unit}{period}") %>%
    urltools::param_set("page", page) %>%
    urltools::param_set("per_page", per_page)

  return(utils::URLencode(query_url))
}

#' Get data from Kolada
#'
#' Download a table of data from Kolada. Data is selected based on three
#' metadata dimensions: KPI (ID), municipality (ID) and period (years). You must
#' supply arguments for at least two of these three dimensions. If a dimension
#' is omitted, all available data for that dimension will be downloaded.
#'
#' @param kpi What kpis should be fetched? Can be a single name or a vector of
#' names.
#' @param municipality For which municipalities should data be fetched? Can be a
#' single name or a vector of names.
#' @param period For what years should data be fetched? Can be one or more
#' four-digit integers or character strings.
#' @param ou (Optional) for what Operating Units should data be fetched? Only
#' available for certain KPIs.
#' @param unit_type One of \code{"municipality"} or \code{"ou"}. Whether to
#' fetch data for Municipalities or Organizational Units.
#' @param max_results (Optional) Specify the maximum number of results
#'  returned by the query.
#' @param simplify Whether to make results more human readable.
#' @param verbose Whether to print the call to the Kolada API as a message to
#' the R console.
#'
#' @return A tibble containing Kolada values and metadata.
#'
#' @examples
#' # Download data for KPIs for Gross Regional Product ("BRP" in Swedish)
#' # for three municipalities
#' if (kolada_available()) {
#'
#' # If you already know the ID numbers you are looking for,
#' # you can use these directly as argments.
#' # Otherwise, use the get_\*() functions in combination with \*_search()
#' # functions to find good parameter values.
#' grp_data <- get_values(
#'   kpi = c("N03700", "N03701"),
#'   municipality = c("0180", "1480", "1280")
#')
#'
#' # To download OU data instead of Municipality data, set the parameter
#' # "unit_type" to "ou".
#' ou_data <- get_values(
#'  kpi = "N15033",
#'  ou = "V15E144001101",
#'  unit_type = "ou"
#' )
#' }
#'
#' @export
get_values <- function(
  kpi = NULL,
  municipality = NULL,
  period = NULL,
  ou = NULL,
  unit_type = "municipality",
  max_results = NULL,
  simplify = TRUE,
  verbose = FALSE
) {

  if (isTRUE(verbose))
    message("Downloading Kolada data using URL(s):")

  next_page <- TRUE
  page <- 1
  per_page <- 2000

  while(isTRUE(next_page)) {

    if(!is.null(max_results) && page * per_page > max_results)
      page_size <- max_results %% per_page
    else
      page_size <- per_page

    query <- compose_data_query(kpi = kpi, municipality = municipality, period = period, ou = ou, unit_type = unit_type, page = page, per_page = page_size)

    if (isTRUE(verbose))
      message(query)

    res <- try(httr::GET(query, httr::config(verbose = verbose)), silent = TRUE)

    if(inherits(res, "try-error")) {
      warning("\nCould not connect to the Kolada database. Please check your internet connection. Did you misspel the query?\nRe-run query with verbose = TRUE to see the URL used in the query.")
      return(NULL)
    }

    contents_raw <- httr::content(res, as = "text")
    contents <- try(jsonlite::fromJSON(contents_raw), silent = TRUE)

    if(inherits(contents, "try-error")) {
      warning("\nKolada returned a 404 or malformatted HTML/JSON. Did you misspel the query?\nRe-run query with verbose = TRUE to see the URL used in the query.")
      return(NULL)
    }

    if(length(contents$values) == 0) {
      warning("\nThe query returned zero hits from the Kolada database. Did you misspel the query?\nRe-run query with verbose = TRUE to see the URL used in the query.")
      return(NULL)
    }


    if(page == 1)
      vals <- tibble::as_tibble(contents$values)
    else
      vals <- dplyr::bind_rows(vals, tibble::as_tibble(contents$values))


    if(is.null(contents$next_page))
      next_page <- FALSE
    else
      page <- page + 1

    if(!is.null(max_results) && nrow(vals) == max_results)
      next_page <- FALSE
  }

  ret <- vals %>%
    tidyr::unnest(cols = c("values"))

  if (isTRUE(simplify) & unit_type == "municipality") {
    ret_has_groups <- any(stringr::str_detect(ret$municipality, "^G"))

    munic_tbl <- get_municipality(verbose = FALSE)

    if (ret_has_groups)
      munic_tbl <- munic_tbl %>%
      dplyr::bind_rows(
        get_municipality_groups(verbose = FALSE) %>%
          dplyr::select(.data$id, .data$title) %>%
          dplyr::mutate(type = "G")
      )

    ret <- ret %>%
      # Remove "status" column (does it ever contain anything?)
      # dplyr::select(-.data$status) %>%
      # Convert codes to names
      dplyr::rename(
        municipality_id = .data$municipality
      ) %>%
      dplyr::inner_join(
        dplyr::select(
          munic_tbl,
          municipality_id = .data$id,
          municipality = .data$title,
          municipality_type = .data$type),
        by = "municipality_id"
      ) %>%
      dplyr::rename(year = .data$period)
  }

  if (isTRUE(simplify) & unit_type == "ou") {
    ou_tbl <- get_ou(id = unique(ret$ou), verbose = FALSE)

    ret <- ret %>%
      # Remove "status" column (does it ever contain anything?)
      dplyr::select(-.data$status) %>%
      # Convert codes to names
      dplyr::rename(
        ou_id = .data$ou
      ) %>%
      dplyr::inner_join(
        dplyr::select(
          ou_tbl,
          ou_id = .data$id,
          ou = .data$title),
        by = "ou_id"
      ) %>%
      dplyr::rename(year = .data$period)
  }

  ret
}

#' Simplify a Kolada values table
#'
#' Simplify a Kolada values table, i.e as created by \code{\link{get_values}},
#' by removing columns that contain monotonous data, i.e. that contain only one
#' value for all observations.
#'
#' @param values_df A Kolada value table, as created by
#' \code{\link{get_values}}.
#'
#' @return A Kolada values table
#'
#' @examples
#' # Download values for all available years of a given KPI for
#' # Malmö municipality (code 1280)
#' if (kolada_available()) {
#' vals <- get_values(kpi = "N45933", municipality = "1280", simplify = TRUE)
#' # (Returns a table with 5 rows and 8 columns)
#'
#' # Remove columns with no information to differentiate between rows
#' values_minimize(vals)
#' # (Returns a table with 5 rows and 4 columns)
#' }
#' @export

values_minimize <- function(values_df) {

  if (is.null(values_df)) {
    warning("\nAn empty object was used as input to values_df().")
    return(NULL)
  }

  values_df %>%
    dplyr::select_if(names(.) %in% c("kpi", "municipality", "value") |
                       purrr::map(., dplyr::n_distinct) > 1)
}

#' Create KPI long-form descriptions to add to a plot
#'
#' In a Kolada values table, only KPI ID names are preserved. But in plots you
#' often want to add a legend to explain what each KPI ID represents. But since
#' KPI explanations are mostly relatively wordy, ggplot2 legends are
#' under-dimensioned for this task. \code{values_legend} returns a string which
#' can conveniently be used as caption to a plot instead.
#'
#' @param values_df A Kolada value table, as created by
#' \code{\link{get_values}}.
#' @param kpi_df A KPI table, e.g. as created by \code{\link{get_kpi}}.
#'
#' @return A string which should be used as caption in a plot.
#'
#' @export
values_legend <- function(values_df, kpi_df) {

  if (is.null(values_df) || is.null(kpi_df)) {
    warning("\nAn empty object was used as input to values_legend().")
    return(NULL)
  }

  kpis <- unique(values_df$kpi)
  desc <- kpi_df %>%
    dplyr::select(.data$id, .data$title) %>%
    dplyr::filter(.data$id %in% .env$kpis)

  paste(glue::glue_data(desc, "{id}: {title}"), collapse = "\n")
}

