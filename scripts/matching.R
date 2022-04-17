library(tidyverse)
library(cubble)
library(patchwork)

climate <- prcp_aus |>
  filter(between(stringr::str_sub(id, 7, 8), 76, 90)) |>
  face_temporal() |>
  filter(lubridate::year(date) == 2020) |>
  face_spatial() |>
  mutate(type = "climate")

river <- river |> mutate(type = "river")

vic_map <- rmapshaper::ms_simplify(ozmaps::abs_ste |> filter(NAME == "Victoria"))  
ggplot() + 
  geom_sf(data = vic_map, fill = "transparent", color = "grey") + 
  geom_point(data = dplyr::bind_rows(river, climate), 
             aes(x = long, y = lat, color = type)) + 
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "bottom") +
  ggplot2::labs(x = "Longitude", y = "Latitude") + 
  scale_color_brewer(palette = "Dark2") 
ggsave(filename = "figures/matching-map.png", width = 10, height = 5)  

res <- match_sites(river, climate,
                   temporal_by = c("Water_course_level" = "prcp"),
                   temporal_independent = "prcp",  
                   temporal_n_highest = 30,
                   temporal_min_match = 15)

p1 <-ggplot() + 
  geom_sf(data = vic_map, fill = "transparent", color = "grey") + 
  geom_point(data = res, aes(x = long, y = lat, color = type)) +
  ggrepel::geom_label_repel(
    data = res |> filter(type == "river"), aes(x = long, y = lat, label = group)) +
  scale_color_brewer(palette = "Dark2")  + 
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "bottom") +
  ggplot2::labs(x = "Longitude", y = "Latitude")

res_long <- res |>
  face_temporal(ts) |>
  unfold(group, type) |>
  rename(prcp = matched_var) |> 
  mutate(prcp = (prcp - min(prcp, na.rm = TRUE))/ (max(prcp, na.rm = TRUE) - min(prcp, na.rm = TRUE))) 

p2 <- res_long |> 
  ggplot(aes(x = date, y = prcp, color = type, group = id)) +
  geom_line(alpha = 0.8) +
  facet_wrap(vars(group)) +
  scale_color_brewer(palette = "Dark2", guide = "none") +
  theme_bw() +
  scale_x_date(date_labels = "%b") + 
  labs(x = "Date", y = "Precipitation/ water level")

(p1 | p2) + 
  patchwork::plot_layout(guides = "collect") + 
  plot_annotation(tag_levels = 'A')&
  theme(legend.position = "bottom")

ggsave(filename = "figures/matching.png", width = 10, height = 5)  
