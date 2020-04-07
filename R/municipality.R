municipality_get <- function(id = NULL, cache = FALSE) {
  get_kld_metadata(entity = "municipality", id = id, cache = cache)
}


municipality_groups_get <- function(id = NULL, cache = FALSE) {
  get_kld_metadata(entity = "municipality_groups", id = id, cache = cache)
}


municipality_to_code <- function(municipality, remove_na = FALSE, cache = FALSE) {
  munic_names <- tolower(municipality)
  munic_df <- municipality_get(cache = TRUE)

  res <- munic_df$id[match(munic_names, tolower(munic_df$title))]
  names(res) <- munic_df$title[match(munic_names, tolower(munic_df$title))]

  unused_names <- unique(municipality[which(!munic_names %in% tolower(munic_df$title))])
  unused_names <- unused_names[!is.na(unused_names)]
  if (length(unused_names) > 0)
    warning("The following names were not found in municipality data:\n- ", paste(unused_names, collapse = "\n- "))

  if (isTRUE(remove_na))
    res <- res[!is.na(res)]

  res
}

code_to_municipality <- function(code, remove_na = FALSE, cache = FALSE) {
  if (!is.integer(code)) {
    code <- as.integer(code)
  }

  munic_df <- municipality_get(cache = TRUE)

  res <- munic_df$title[match(code, tolower(munic_df$id))]
  names(res) <- munic_df$id[match(code, tolower(munic_df$id))]

  unused_names <- unique(code[which(!code %in% munic_df$id)])
  unused_names <- unused_names[!is.na(unused_names)]
  if (length(unused_names) > 0)
    warning("The following ids were not found in municipality data:\n- ", paste(unused_names, collapse = "\n- "))

  if (isTRUE(remove_na))
    res <- res[!is.na(res)]

  res
}
