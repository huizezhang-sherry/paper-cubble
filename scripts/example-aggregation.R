coords <- cbind(weatherdata::climate_full$long, weatherdata::climate_full$lat)
dist_raw <- geosphere::distm(coords, coords)

station_nested <- weatherdata::climate_full %>% 
  strip_rowwise() %>% 
  mutate(cluster = kmeans(dist_raw,centers = 20, nstart = 500)$cluster)

cluster_nested <- station_nested %>%
  switch_key(cluster) %>% 
  get_centroid()

station_long <- cluster_nested %>% 
  stretch(ts) %>% 
  mutate(wk = lubridate::week(date)) %>% 
  group_by(wk, id) %>% 
  summarise(prcp = sum(prcp, na.rm = TRUE))

cluster_long <- station_long %>% 
  ungroup(id)
  summarise(prcp = mean(prcp, na.rm = TRUE)) %>% 
  migrate(cent_long, cent_lat)

gly <- GGally::glyphs(cluster_long,
                      x_major = "cent_long", x_minor = "wk",
                      y_major = "cent_lat", y_minor = "prcp",
                      height = 2, width = 4
)

state_map <- rmapshaper::ms_simplify(ozmaps::abs_ste, keep = 2e-3)
p1 <- plot_map(state_map) +
  geom_text(data = cluster_nested, 
            aes(x = cent_long, y = cent_lat, label = cluster)) +
  geom_path(data = gly,aes(x = gx, y = gy, group = gid))


p2 <- station_long %>%
  ggplot(aes(x = wk, y = prcp, group = id)) +
  geom_line(alpha = .3) +
  facet_wrap(vars(cluster), scales = "free_y", ncol = 4) +
  theme_bw()

tas_latlong <- station_long %>% 
  ungroup(wk, id) %>% 
  filter(cluster == 15) %>% 
  filter(prcp == max(prcp)) %>% 
  tamp()

p3 <- plot_map(state_map) +
  geom_point(data = station_nested, aes(x = long, y = lat), size = 0.5) +
  ggforce::geom_mark_hull(data = cluster_nested %>% tidyr::unnest(hull),
                          expand = 0, radius = 0,
                          aes(x = long, y = lat, group = cluster)) + 
  geom_point(data = tas_latlong, aes(x = long, y = lat), col = "red")

(p1 | p3)/ p2 + plot_annotation(tag_levels = 'A')
ggsave(filename = "figures/aggregation.png", width = 7, height = 9)
