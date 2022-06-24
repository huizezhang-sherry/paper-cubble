## ----eval = FALSE-------------------------------------------------------------
## # This chunk generates the interactive graphic and save it as figures/linking.html, where the three screencasts in the figure comes from.
## library(crosstalk)
## library(plotly)
## 
## clean <- climate_full %>%
##   filter(!id %in% c("ASN00067033", "ASN00072091", "ASN00059040",
##                     "ASN00097053", "ASN00040856", "ASN00015548")) %>%
##   face_temporal() %>%
##   mutate(month = lubridate::month(date)) %>%
##   group_by(month) %>%
##   summarise(
##     tmax = mean(tmax, na.rm = TRUE),
##     tmin = mean(tmin, na.rm = TRUE),
##     diff = mean(tmax - tmin, na.rm = TRUE)
##     ) %>%
##   ungroup(month) %>%
##   mutate(dummy_date = as.Date(glue::glue("2021-{month}-01"))) %>%
##   face_spatial() %>%
##   mutate(temp_diff_var = ts %>% pull(diff) %>% var(na.rm = TRUE))
## 
## nested <- clean %>%
##   SharedData$new(~id, group = "cubble")
## 
## long <- clean %>%
##   face_temporal() %>%
##   unfold(temp_diff_var) %>%
##   arrange(temp_diff_var) %>%
##   SharedData$new(~id, group = "cubble")
## 
## state_map <- rmapshaper::ms_simplify(ozmaps::abs_ste, keep = 2e-3)
## 
## p1 <- nested %>%
##   ggplot() +
##   geom_sf(data = state_map, fill = "grey95", color = "white") +
##   geom_point(aes(x = long, y = lat,color = temp_diff_var, label = name)) +
##   colorspace::scale_color_continuous_sequential(
##     "Rocket",  n_interp = 7, cmax = 90, rev = TRUE,
##     c2 = 40, l2= 85, c1 = 20, l1 = 30, name = "Var. temp. diff.") +
##   labs(x = "Longitude", y = "Latitude") +
##   coord_sf() +
##   theme_bw() +
##   theme(
##     panel.grid.major = element_blank(),
##     legend.position = "none"
##     )
## 
## p2 <- long %>%
##   ggplot(aes(x = dummy_date, group = id,
##              fill = temp_diff_var, color = temp_diff_var)) +
##   geom_ribbon(aes(ymin = tmin, ymax = tmax), size = 0.1, alpha = 0.3) +
##   geom_line(aes(y = tmin), alpha = 0.1) +
##   geom_line(aes(y = tmax), alpha = 0.1) +
##   geom_point(aes(y = tmax), size = 0.1) +
##   geom_point(aes(y = tmin), size = 0.1) +
##   colorspace::scale_fill_continuous_sequential(
##     "Rocket",  n_interp = 7, cmax = 90, rev = TRUE,
##     c2 = 40, l2= 85, c1 = 20, l1 = 30, name = "Var. temp. diff.") +
##   colorspace::scale_colour_continuous_sequential(
##     "Rocket",  n_interp = 7, cmax = 90, rev = TRUE,
##     c2 = 40, l2= 85, c1 = 20, l1 = 30, name = "Var. temp. diff.") +
##   scale_x_date(date_labels = "%b", date_breaks = "1 month") +
##   labs(x = "Month", y = "Temperature") +
##   theme_bw() +
##   theme(
##     panel.grid.major = element_blank(),
##     legend.position = "bottom"
##     )
## 
## out <- bscols(
##   ggplotly(p1, width = 900, height = 700, tooltip = "label") %>%
##     highlight(
##       on = "plotly_selected",
##       off = "plotly_deselect",
##       opacityDim = 0.1),
##   ggplotly(p2, width = 1200, height = 700, tooltip = "label") %>%
##     highlight(
##       on = "plotly_selected",
##       off = "plotly_deselect",
##       opacityDim = 0.012),
##   widths = c(5, 5)
## )
## 
## htmltools::save_html(out, file = "figures/linking.html")
## 


## ----interactive-linking, echo = FALSE, out.width="100%", out.height="23%", fig.retina = 2, dpi = 300, fig.cap = "Exploring temperature variation using linking of a map and seasonal display. Each row is a screen dump of the process. The top row shows all locations and all temperature profiles. Selecting a particular location on the map (here Mount Elizabeth) produces the plot in the second row. The maximum and minimum temperatures are shown using a ribbon. The bottom row first selects the lowest temperature in August in the seasonal display, which highlights the corresponding station on the map (Thredbo AWS). Another  station, located in the Tasmania Island, is then selected to compare its temperature variation with Thredbo AWS.", fig.show='hold'----
knitr::include_graphics(here::here("figures/linking.png"))
knitr::include_graphics(here::here("figures/linking-north.png"))
knitr::include_graphics(here::here("figures/linking-lower.png"))

