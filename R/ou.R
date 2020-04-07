ou_get <- function(id = NULL, municipality = NULL, cache = FALSE) {
  get_kld_metadata(entity = "kpi_groups", id = id, municipality = municipality, cache = cache)
}
