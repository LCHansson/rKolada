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
#' @param from_date (Optional) Only return data updated after this date.
#' Format: \code{"YYYY-MM-DD"}.
#' @param version Version of the API. Defaults to \code{"v3"}.
#'
#' @return A string containing a URL to the Kolada REST API.
compose_data_query <- function(
  kpi = NULL,
  municipality = NULL,
  period = NULL,
  ou = NULL,
  unit_type = "municipality",
  from_date = NULL,
  page = NA,
  per_page = NA,
  version = "v3"
) {
  unit_type <- tolower(unit_type)
  if (!unit_type %in% c("municipality", "ou"))
    stop("argument to 'unit_type' must be one of 'municipality' or 'ou'.")

  if (unit_type == "municipality")
    base_url <- glue::glue("https://api.kolada.se/{version}/data")
  else if (unit_type == "ou")
    base_url <- glue::glue("https://api.kolada.se/{version}/oudata")

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

  query_url <- glue::glue("{base_url}{kpi}{unit}{period}")
  query_url <- append_query_params(query_url, page = page, per_page = per_page,
                                   from_date = from_date)

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
#' @param from_date (Optional) Only return data updated after this date.
#' Format: \code{"YYYY-MM-DD"}.
#' @param keep_deleted Logical. If \code{FALSE} (default), rows where
#' \code{isdeleted} is \code{TRUE} are removed from the result.
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
  from_date = NULL,
  keep_deleted = FALSE,
  simplify = TRUE,
  verbose = FALSE
) {

  if (isTRUE(verbose))
    message("Downloading Kolada data using URL(s):")

  # Chunk parameters to stay within the API's 25-element-per-path-segment limit
  kpi_chunks <- chunk_vector(kpi)
  period_chunks <- chunk_vector(period)
  if (tolower(unit_type) == "ou")
    unit_chunks <- chunk_vector(ou)
  else
    unit_chunks <- chunk_vector(municipality)

  all_vals <- list()

  for (kpi_c in kpi_chunks) {
    for (unit_c in unit_chunks) {
      for (period_c in period_chunks) {

        if (tolower(unit_type) == "ou") {
          munic_c <- municipality
          ou_c <- unit_c
        } else {
          munic_c <- unit_c
          ou_c <- ou
        }

        has_next <- TRUE
        page <- 1
        per_page <- 5000

        while(isTRUE(has_next)) {

          if(!is.null(max_results) && page * per_page > max_results)
            page_size <- max_results %% per_page
          else
            page_size <- per_page

          query <- compose_data_query(kpi = kpi_c, municipality = munic_c,
                                      period = period_c, ou = ou_c,
                                      unit_type = unit_type, from_date = from_date,
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

          if(length(contents$values) == 0)
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
    }
  }

  if (length(all_vals) == 0) {
    warning("\nThe query returned zero hits from the Kolada database. Did you misspel the query?\nRe-run query with verbose = TRUE to see the URL used in the query.")
    return(NULL)
  }

  vals <- dplyr::bind_rows(all_vals)

  if (!is.null(max_results) && nrow(vals) > max_results)
    vals <- utils::head(vals, max_results)

  ret <- vals %>%
    tidyr::unnest(cols = c("values"))

  # Filter out deleted records if present
  if (!isTRUE(keep_deleted) && "isdeleted" %in% names(ret)) {
    ret <- ret %>%
      dplyr::filter(!.data$isdeleted) %>%
      dplyr::select(-"isdeleted")
  }

  if (isTRUE(simplify) & unit_type == "municipality") {
    ret_has_groups <- any(stringr::str_detect(ret$municipality, "^G"))

    munic_tbl <- get_municipality(verbose = FALSE)

    if (ret_has_groups)
      munic_tbl <- munic_tbl %>%
      dplyr::bind_rows(
        get_municipality_groups(verbose = FALSE) %>%
          dplyr::select("id", "title") %>%
          dplyr::mutate(type = "G")
      )

    ret <- ret %>%
      # Remove "status" column (does it ever contain anything?)
      # dplyr::select(-.data$status) %>%
      # Convert codes to names
      dplyr::rename(
        municipality_id = "municipality"
      ) %>%
      dplyr::inner_join(
        dplyr::select(
          munic_tbl,
          municipality_id = "id",
          municipality = "title",
          municipality_type = "type"),
        by = "municipality_id"
      ) %>%
      dplyr::rename(year = "period")
  }

  if (isTRUE(simplify) & unit_type == "ou") {
    ou_tbl <- get_ou(id = unique(ret$ou), verbose = FALSE)

    ret <- ret %>%
      # Remove "status" column (does it ever contain anything?)
      dplyr::select(-"status") %>%
      # Convert codes to names
      dplyr::rename(
        ou_id = "ou"
      ) %>%
      dplyr::inner_join(
        dplyr::select(
          ou_tbl,
          ou_id = "id",
          ou = "title"),
        by = "ou_id"
      ) %>%
      dplyr::rename(year = "period")
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

  keep <- names(values_df) %in% c("kpi", "municipality", "value") |
    purrr::map_lgl(values_df, ~ dplyr::n_distinct(.x) > 1)
  values_df %>% dplyr::select(dplyr::all_of(names(values_df)[keep]))
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
    dplyr::select("id", "title") %>%
    dplyr::filter(.data$id %in% .env$kpis)

  paste(glue::glue_data(desc, "{id}: {title}"), collapse = "\n")
}

