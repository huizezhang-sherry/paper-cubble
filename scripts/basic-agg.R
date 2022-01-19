library(tidyverse)
library(cubble)
library(patchwork)
coords <- cbind(weatherdata::climate_full$long, weatherdata::climate_full$lat)
dist_raw <- geosphere::distm(coords, coords)

set.seed(123)
station_nested <- weatherdata::climate_full %>% 
  strip_rowwise() %>% 
  mutate(cluster = kmeans(dist_raw,centers = 20, nstart = 500)$cluster)
save(station_nested, file = here::here("data/station_nested.rda"))

cluster_nested <- station_nested %>%
  switch_key(cluster) %>% 
  get_centroid()

station_long <- station_nested %>% 
  stretch(ts) %>% 
  mutate(wk = lubridate::week(date)) %>% 
  group_by(wk) %>% 
  summarise(prcp = sum(prcp, na.rm = TRUE))

cluster_long <- cluster_nested %>% 
  stretch(ts) %>% 
  mutate(wk = lubridate::week(date)) %>% 
  group_by(wk) %>% 
  summarise(prcp = mean(prcp, na.rm = TRUE)) %>% 
  migrate(cent_long, cent_lat)

state_map <- rmapshaper::ms_simplify(ozmaps::abs_ste, keep = 2e-3)
p1 <- plot_map(state_map) +
  geom_text(data = cluster_nested, 
            aes(x = cent_long, y = cent_lat, label = cluster)) +
  geom_glyph(data = cluster_long, 
             aes(x_major = cent_long, x_minor = wk, y_major = cent_lat, y_minor = prcp), 
             height = 2, width = 4)

tas_latlong <- station_long %>% 
  ungroup(wk, id) %>% 
  filter(cluster == 7) %>% 
  filter(prcp == max(prcp)) %>% 
  tamp()

tas_id <- tas_latlong %>% pull(id)
tas_long <- station_long %>% ungroup(wk, id) %>% filter(id == tas_id)

p2 <- station_long %>%
  ggplot(aes(x = wk, y = prcp, group = id)) +
  geom_line(alpha = .3) +
  geom_line(data = tas_long, color = "red") + 
  facet_wrap(vars(cluster), scales = "free_y", ncol = 4) +
  theme_bw()


p3 <- plot_map(state_map) +
  geom_point(data = station_nested, aes(x = long, y = lat), size = 0.5) +
  ggforce::geom_mark_hull(data = cluster_nested %>% tidyr::unnest(hull),
                          expand = 0, radius = 0,
                          aes(x = long, y = lat, group = cluster)) + 
  geom_point(data = tas_latlong, aes(x = long, y = lat), col = "red")

(p1 | p3)/ p2 + plot_annotation(tag_levels = 'A')
ggsave(filename = "figures/basic-agg.png", width = 7, height = 9)
