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

  if (nrow(kpi_data) < 1)
    return(FALSE)

  if (!"kpi" %in% names(kpi_data) || !"value" %in% names(kpi_data))
    return(FALSE)

  if (!is.numeric(kpi_data$value))
    return(FALSE)

  return(TRUE)
}
