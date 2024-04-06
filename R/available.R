#' Check if the Kolada API is available
#'
#' Check if the Kolada API is available and if data from the API makes sense.
#'This is primarily useful for programming with rKolada and/or examples.
#'
#' @export

kolada_available <- function() {
  suppressWarnings({
    kpi_data <- get_values(
      kpi = "N00003",
      municipality = c("0180", "1480"),
      period = 2021
    )
  })

  if (is.null(kpi_data))
    return(FALSE)

  if (any(purrr::map_lgl(kpi_data, ~ any(is.na(.x)))))
    return(FALSE)

  if (nrow(kpi_data) != 2)
    return(FALSE)

  if (unique(kpi_data$kpi) != "N00003")
    return(FALSE)

  if (length(unique(kpi_data$kpi)) != 1)
    return(FALSE)

  if (!is.numeric(kpi_data$value))
    return(FALSE)

  return(TRUE)
}
