library("tidyverse")

cache <- TRUE

kpi_filter <- kpi_get(cache = cache) %>%
  filter(str_detect(description, "BRP")) %>%
  kpi_simplify()

kpi_desc <- kpi_filter %>% select(id, title, description)

k_df <- get_kld_data(
  kpi = kpi_filter$id,
  municipality = municipality_to_code(c("Malmö", "Lund"), cache = cache),
  period = 2008:2019
)
k_df

k_plot_df <- k_df %>%
  select(kpi, municipality, gender, value) %>%
  inner_join(kpi_desc, by = c("kpi" = "id"))

p <- ggplot(k_df, aes(x = period, y = value)) +
  geom_line(aes(color = kpi)) +
  facet_grid(municipality ~ .)
p



kpis <- kpi_get(cache = cache)

kpis %>%
  count(is_divided_by_gender, has_ou_data) %>%
  mutate_at(1:2, as.character) %>%
  pivot_longer(1:2) %>%
  ggplot(aes(x = name, y = value, fill = n, color = n)) +
  geom_bin2d()

