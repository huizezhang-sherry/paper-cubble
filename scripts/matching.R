library(tidyverse)
library(cubble)
library(patchwork)
river <- weatherdata::water %>%
  stretch() %>%
  select(date, Water_course_level) %>%
  tamp() %>%
  mutate(type = "river")

climate <- weatherdata::climate_full %>%
  filter(between(stringr::str_sub(id, 7, 8), 76, 90)) %>%
  stretch() %>%
  filter(lubridate::year(date) == 2020) %>%
  tamp() %>%
  mutate(type = "climate")

vic_map <- rmapshaper::ms_simplify(ozmaps::abs_ste %>% filter(NAME == "Victoria"))
plot_map(vic_map) +
  geom_point(
    data = dplyr::bind_rows(river, climate),
    aes(x = long, y = lat, color = type)
  ) +
  scale_color_brewer(palette = "Dark2") +
  theme_bw()

river <- river %>%
  stretch() %>%
  rename(prcp = Water_course_level) %>%
  tamp()

res <- match_sites(river, climate,
                   temporal_var_to_match = prcp,
                   temporal_independent = climate,  
                   temporal_n_highest = 30,
                   temporal_min_match = 15
)

p1 <- plot_map(vic_map) +
  geom_point(
    data = res,
    aes(x = long, y = lat, color = type)
  ) +
  ggrepel::geom_label_repel(
    data = res %>% filter(type == "river"),
    aes(x = long, y = lat, label = .group)
  ) +
  scale_color_brewer(palette = "Dark2") +
  ggtitle("Victoria") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

res_long <- res %>%
  stretch(ts) %>%
  migrate(.group, type) %>%
  mutate(prcp = scale(prcp)[, 1]) %>%
  switch_key(.group) %>%
  migrate(type)

p2 <- res_long %>%
  ggplot(aes(
    x = date, y = prcp,
    group = id, color = type
  )) +
  geom_line() +
  facet_wrap(vars(.group)) +
  scale_color_brewer(palette = "Dark2", guide = "none") +
  theme_bw() +
  labs(x = "week") +
  scale_x_date(date_labels = "%b")

p1 | p2

ggsave(filename = "figures/matching.png", width = 15, height = 7)  
