library(tidyverse)
library(cubble)
library(patchwork)

climate <- weatherdata::climate_full %>%
  filter(between(stringr::str_sub(id, 7, 8), 76, 90)) %>%
  stretch() %>%
  filter(lubridate::year(date) == 2020) %>%
  tamp() %>%
  mutate(type = "climate")

river <- weatherdata::water %>%
  stretch() %>%
  select(date, Water_course_level) %>%
  rename(prcp = Water_course_level) %>% 
  tamp() %>%
  mutate(type = "river")

vic_map <- rmapshaper::ms_simplify(ozmaps::abs_ste %>% filter(NAME == "Victoria"))  
plot_map(vic_map) +
  geom_point(data = dplyr::bind_rows(river, climate), 
             aes(x = long, y = lat, color = type)) + 
  scale_color_brewer(palette = "Dark2") +
  theme_bw() + 
  theme(legend.position = "bottom") + 
  labs(x = "Longitude", y = "Latitude")
ggsave(filename = "figures/matching-map.png", width = 10, height = 5)  

res <- match_sites(river, climate,
                   temporal_var_to_match = prcp,
                   temporal_independent = climate,  
                   temporal_n_highest = 30,
                   temporal_min_match = 15)

p1 <- plot_map(vic_map) +
  geom_point(data = res, aes(x = long, y = lat, color = type)) +
  ggrepel::geom_label_repel(
    data = res %>% filter(type == "river"), aes(x = long, y = lat, label = group)) +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) + 
  labs(x = "Longitude", y = "Latitude")

res_long <- res %>%
  stretch(ts) %>%
  migrate(group, type) %>%
  mutate(prcp = (prcp - min(prcp, na.rm = TRUE))/ (max(prcp, na.rm = TRUE) - min(prcp, na.rm = TRUE))) 

p2 <- res_long %>% 
  ggplot(aes(x = date, y = prcp, color = type, group = id)) +
  geom_line(alpha = 0.8) +
  facet_wrap(vars(group)) +
  scale_color_brewer(palette = "Dark2", guide = "none") +
  theme_bw() +
  labs(x = "week") +
  scale_x_date(date_labels = "%b") + 
  labs(x = "Week", y = "Precipitation/ water level")

(p1 | p2) + 
  patchwork::plot_layout(guides = "collect") + 
  plot_annotation(tag_levels = 'A')&
  theme(legend.position = "bottom")

ggsave(filename = "figures/matching.png", width = 10, height = 5)  
