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
  summarise(prcp = sum(prcp, na.rm = TRUE)) %>% 
  migrate(cluster)

cluster_long <- cluster_nested %>% 
  stretch(ts) %>% 
  mutate(wk = lubridate::week(date)) %>% 
  group_by(id, wk) %>%
  summarise(prcp = sum(prcp, na.rm = TRUE)) %>%
  ungroup(id) %>%
  summarise(prcp = mean(prcp, na.rm = TRUE)) %>% 
  migrate(cent_long, cent_lat)

state_map <- rmapshaper::ms_simplify(ozmaps::abs_ste, keep = 2e-3)

ggplot_smooth <- cluster_long %>% 
  ggplot() +
  geom_smooth(aes(x = wk, y = prcp, group = cluster), span = 0.4) 

smoother <- layer_data(ggplot_smooth) %>% 
  left_join(cluster_long %>% select(cluster, cent_long, cent_lat), by = c("group" = "cluster"))

p1 <- ggplot(data = smoother, 
             aes(x_minor = x, y_minor = y, x_major = cent_long, y_major = cent_lat)) + 
  geom_sf(data = state_map, inherit.aes = FALSE,
          color = "grey80", alpha = 0.4, linetype = 3) + 
  geom_text(data = cluster_nested, 
            aes(x = cent_long, y = cent_lat, label = cluster), inherit.aes = FALSE) +
  geom_glyph(height = 2, width = 4) + 
  theme_void()

p3 <- ggplot() + 
  geom_sf(data = state_map, inherit.aes = FALSE,
          color = "grey80", alpha = 0.4, linetype = 3) + 
  geom_point(data = station_nested, aes(x = long, y = lat), size = 0.5) +
  ggforce::geom_mark_hull(data = cluster_nested %>% tidyr::unnest(hull),
                          expand = 0, radius = 0,
                          aes(x = long, y = lat, group = cluster)) + 
  theme_void()

(p1 | p3) + plot_annotation(tag_levels = 'A')
ggsave(filename = "figures/basic-agg.png", width = 15, height = 7)
