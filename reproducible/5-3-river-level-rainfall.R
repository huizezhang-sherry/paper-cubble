## -----------------------------------------------------------------------------
load(here::here("data/climate_full.rda"))

climate <- climate_full %>%
  filter(between(stringr::str_sub(id, 7, 8), 76, 90)) %>%
  face_temporal() %>%
  filter(lubridate::year(date) == 2020) %>%
  face_spatial() %>%
  mutate(type = "climate")

river <- river %>% mutate(type = "river")


## ----echo = TRUE--------------------------------------------------------------
res <- match_sites(
  river, climate,
  temporal_by = c("Water_course_level" = "prcp"),
  temporal_independent = "prcp",  
  temporal_n_highest = 30,
  temporal_min_match = 15, 
)


## -----------------------------------------------------------------------------
res %>% print()


## ----matching, out.width="100%", fig.height = 5, fig.width = 10, fig.cap="Weather stations and river gauges with matched pairs labelled on the map (a) and plotted across time (b). Precipitation and water level have been standardised between 0 and 1 to be displayed on the same scale. The water level reflects the increase in precipitation. The numbers (1, 5, 6, 10) indicate the group index derived from spatial matching, only those that were selectd by temporal matching are shown here."----
vic_map <- ozmaps::abs_ste %>% 
  filter(NAME == "Victoria") %>% 
  rmapshaper::ms_simplify()  

p1 <-ggplot() + 
  geom_sf(data = vic_map, fill = "grey95", color = "white") + 
  geom_point(data = dplyr::bind_rows(river, climate), 
             aes(x = long, y = lat, color = type), alpha = 0.2, fill = 0.2) +
  geom_point(data = res, aes(x = long, y = lat, color = type)) +
  ggrepel::geom_label_repel(
    data = res %>% filter(type == "river"), aes(x = long, y = lat, label = group)) +
  scale_color_brewer(palette = "Dark2")  + 
  ggthemes::theme_map() +
  ggplot2::theme(legend.position = "bottom",
                 legend.text = element_text(size = 15),
                 legend.title = element_text(size = 15)) +
  ggplot2::labs(x = "Longitude", y = "Latitude") + 
  guides(color = guide_legend(override.aes = list(size=5)))

res_long <- res %>%
  face_temporal(ts) %>%
  unfold(group, type) %>%
  rename(prcp = matched_var) %>% 
  mutate(prcp = (prcp - min(prcp, na.rm = TRUE))/ (max(prcp, na.rm = TRUE) - min(prcp, na.rm = TRUE))) 

p2 <- res_long %>% 
  ggplot(aes(x = date, y = prcp, color = type, group = id)) +
  geom_line(alpha = 0.8) +
  facet_wrap(vars(group)) +
  scale_color_brewer(palette = "Dark2", guide = "none") +
  theme_bw() +
  scale_x_date(date_labels = "%b") + 
  labs(x = "Date", y = "Precipitation/ water level")

(p1 | p2) + 
  patchwork::plot_layout(guides = "collect") + 
  plot_annotation(tag_levels = 'a')&
  theme(legend.position = "bottom") 

