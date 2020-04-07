kpi_get <- function(id = NULL, cache = FALSE) {
  get_kld_metadata(entity = "kpi", id = id, cache = cache)
}

kpi_groups_get <- function(id = NULL, cache = FALSE) {
  get_kld_metadata(entity = "kpi_groups", id = id, cache = cache)
}


kpi_simplify <- function(kpi_df, remove_undocumented_columns = TRUE, remove_monotonous_data = FALSE) {
  if (isTRUE(remove_undocumented_columns) & "auspices" %in% names(kpi_df)) {
    kpi_df <- kpi_df %>%
      select(-auspices)
  }
  if (isTRUE(remove_monotonous_data))
    kpi_df <- kpi_df %>%
      select_if(names(.) %in% c("id", "title", "desctiption") | map(., n_distinct) > 1)

  kpi_df <- kpi_df %>%
    select(id, title, description, everything())

  kpi_df
}

kpi_add_keywords <- function(kpis, n = 2) {
  keyword_df <- kpis %>%
    mutate(
      title_words = str_split(title, "\\s"),
      title_words = map(title_words, str_remove_all, "[:punct:]$"),
      title_words = map(title_words, function(x) x[!x %in% stopwords()]),
      title_words = map(title_words, function(x, n) x[1:n], n)
    ) %>%
    unnest(cols = c(title_words))

}
