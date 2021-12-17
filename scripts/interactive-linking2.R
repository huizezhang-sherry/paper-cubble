dt <- clean %>% tamp() 

dt %>% 
  ggplot() +
  geom_sf(data = state_map, aes(geometry = geometry), fill = "transparent") +
  geom_point(aes(x = long, y = lat, label = name, color = annual)) +
  colorspace::scale_color_continuous_sequential("Red") + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        legend.position = "bottom")

nested <- clean %>% 
  SharedData$new(~id, group = "cubble")

long <- clean %>% stretch() %>% 
  SharedData$new(~id, group = "cubble")


p1 <- nested %>% 
  ggplot() +
  geom_sf(data = state_map, aes(geometry = geometry), fill = "transparent") +
  geom_point(aes(x = long, y = lat,color = diurnal, label = name)) +
  colorspace::scale_color_continuous_sequential(
    "Oslo",
    name = "Avg. temp. diff.") + 
  labs(x = "Longitude", y = "Latitude") + 
  coord_sf() + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        legend.position = "bottom")

p2 <- long %>% 
  ggplot() +
  geom_ribbon(aes(x = dummy_date, ymin = tmin, ymax = tmax, group = id), 
              color = "grey30", fill = "grey70", alpha = 0.2) +
  geom_point(aes(x = dummy_date, y = tmax), size = 0.1) + 
  scale_x_date(date_labels = "%b", date_breaks = "1 month") + 
  labs(x = "Month", y = "Temperature") + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        legend.position = "bottom")

# More adjustment:
# - lighter color on the selected temperature band
# - wider width for ts and narrow for the map
# - color correspondence in the popup map

out <- bscols(
  ggplotly(p1, width = 900, height = 700, tooltip = "label") %>%
    highlight(on = "plotly_selected", off = "plotly_deselect"),
  ggplotly(p2, width = 850, height = 700, tooltip = "label") %>% 
    highlight(on = "plotly_selected", off = "plotly_deselect", color = "grey30"),
  widths = c(6, 6)
)

