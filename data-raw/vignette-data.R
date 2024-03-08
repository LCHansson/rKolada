
library("rKolada")

n00945 <- get_values(
  kpi = "N00945",
  municipality = c("0180", "1480", "1280"),
  period = 1970:2020
)

kpi_df <- get_kpi()

munic_g <- get_municipality_groups()

kpi_filter <- kpi_df %>%
  kpi_search("BRP")

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

usethis::use_data(
  n00945,
  kpi_df,
  munic_g,
  kpi_filter,
  munic_grp_filter,
  arboga,
  grp_data,
  overwrite = FALSE, internal = FALSE
)
