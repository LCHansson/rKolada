
library("rKolada")

n00945 <- get_values(
  kpi = "N00945",
  municipality = c("0180", "1480", "1280"),
  period = 1970:2020
)

kpi_df <- get_kpi()

munic <- get_municipality()

munic_g <- get_municipality_groups()

kpi_filter <- kpi_df %>%
  kpi_search("BRP") %>%
  kpi_search("K", column = "municipality_type")

munic_grp_filter <- munic_g %>%
  municipality_grp_search("Liknande kommuner socioekonomi, Arboga")

arboga <- get_municipality() %>% municipality_search("Arboga")

grp_data <- get_values(
  kpi = kpi_extract_ids(kpi_filter),
  municipality = c(
    municipality_grp_extract_ids(munic_grp_filter),
    municipality_extract_ids(arboga)
  )
)

kld_data <- get_values(
  kpi = kpi_df %>%
    kpi_search("BRP") %>%
    kpi_minimize(remove_undocumented_columns = TRUE, remove_monotonous_data = TRUE) %>%
    kpi_search("K", column = "municipality_type") %>%
    kpi_extract_ids(),
  municipality = munic %>%
    municipality_search("K", column = "type") %>%
    municipality_search(c("Stockholm", "Göteborg", "Malmö")) %>%
    municipality_extract_ids(),
  period = 1990:2019,
  simplify = TRUE
)

usethis::use_data(
  n00945,
  kpi_df,
  munic,
  munic_g,
  kpi_filter,
  munic_grp_filter,
  arboga,
  grp_data,
  kld_data,
  overwrite = TRUE, internal = TRUE
)
