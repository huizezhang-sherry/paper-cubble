library(cubble)
library(tidyverse)
library(crosstalk)
library(plotly)

clean <-  weatherdata::climate_full %>%
  filter(!id %in% c("ASN00067033", "ASN00072091")) %>% 
  stretch() %>%
  mutate(month = lubridate::month(date),
         diff = tmax - tmin) %>% 
  group_by(month) %>% 
  summarise(tmax = mean(tmax, na.rm = TRUE),
            diff = mean(diff, na.rm = TRUE)) %>% 
  ungroup(month) %>% 
  tamp() %>%
  mutate(a = nrow(ts)) %>% 
  filter(a == 12) %>% 
  mutate(diff_winter = ts %>% filter(month %in% c(6,7,8)) %>% pull(diff) %>% mean(na.rm = TRUE),
         diff_summer = ts %>% filter(month %in% c(12, 1,2)) %>% pull(diff) %>% mean(na.rm = TRUE),
         diff_year = diff_winter - diff_summer) %>% 
  stretch() %>% 
  mutate(dummy_date = as.Date(glue::glue("2021-{month}-01"))) %>%
  migrate(name) 

long <- clean %>% 
  SharedData$new(~id, group = "cubble")

nested <- clean %>% tamp() %>% as_tibble() %>% 
  SharedData$new(~id, group = "cubble")


p1 <- nested %>%  
  ggplot(aes(x = diff_summer, y = diff_winter, color = diff_year, label = name)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") + 
  colorspace::scale_color_continuous_diverging("Purple-Green", mid = 0, l2 = 80) + 
  xlim(0, 25) + 
  ylim(0, 25) +
  theme_bw() + 
  theme(legend.position = "none") + 
  xlab("temperature difference in summer") + 
  ylab("temperature difference in winter") 

state_map <- rmapshaper::ms_simplify(ozmaps::abs_ste, keep = 2e-3)
p2 <- nested %>% 
  ggplot() +
  geom_sf(data = state_map, aes(geometry = geometry), fill = "transparent") +
  geom_point(aes(x = long, y = lat, label = name, color = diff_year)) +
  colorspace::scale_color_continuous_diverging(
    "Purple-Green", 
    mid = 0, l2 = 80,
    name = "winter-summer difference") + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        legend.position = "bottom")



out <- bscols(
  ggplotly(p1, width = 700, height = 700, tooltip = "label") %>% 
    highlight(on = "plotly_selected", off = "plotly_deselect",
              selected = attrs_selected(color = "grey10")),
  ggplotly(p2, width = 900, height = 700, tooltip = "label") %>% 
    highlight(on = "plotly_selected", off = "plotly_deselect",
              selected = attrs_selected(color = "grey10")),
  widths = c(6, 6)
)

htmltools::save_html(out, file = "figures/linking.html")
webshot2::webshot("figures/linking.html", "figures/linking.png", vwidth = 1900, vheight = 2000)
