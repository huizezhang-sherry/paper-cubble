library(cubble)
library(tidyverse)
library(crosstalk)
library(plotly)

clean <- weatherdata::climate_full |>
  filter(!id %in% c("ASN00067033", "ASN00072091", "ASN00059040", 
                    "ASN00097053", "ASN00040856", "ASN00015548")) |>
  face_temporal() |> 
  mutate(month = lubridate::month(date)) |>
  group_by(month) |>
  summarise(tmax = mean(tmax, na.rm = TRUE),
            tmin = mean(tmin, na.rm = TRUE),
            diff = mean(tmax - tmin, na.rm = TRUE)) |> 
  ungroup(month) |>
  mutate(dummy_date = as.Date(glue::glue("2021-{month}-01"))) |> 
  face_spatial() |> 
  mutate(temp_diff_var = ts |> pull(diff) |> var(na.rm = TRUE))
  

nested <- clean |> 
  SharedData$new(~id, group = "cubble")

long <- clean |> face_temporal() |> unfold(temp_diff_var) |> arrange(temp_diff_var) |> 
  SharedData$new(~id, group = "cubble")

state_map <- rmapshaper::ms_simplify(ozmaps::abs_ste, keep = 2e-3)
p1 <- nested |> 
  ggplot() +
  geom_sf(data = state_map, aes(geometry = geometry), fill = "transparent", color = "grey50") +
  geom_point(aes(x = long, y = lat,color = temp_diff_var, label = name)) +
  colorspace::scale_color_continuous_sequential(
    "Rocket",  n_interp = 7, cmax = 90, rev = TRUE, c2 = 40, l2= 85, c1 = 20, l1 = 30,
    name = "Var. temp. diff.") +
  labs(x = "Longitude", y = "Latitude") + 
  coord_sf() + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        legend.position = "none")

p2 <- long |> 
  ggplot(aes(x = dummy_date, group = id, fill = temp_diff_var, color = temp_diff_var)) +
  geom_ribbon(aes(ymin = tmin, ymax = tmax), size = 0.1, alpha = 0.3) + 
  geom_line(aes(y = tmin), alpha = 0.1) + 
  geom_line(aes(y = tmax), alpha = 0.1) + 
  geom_point(aes(y = tmax), size = 0.1) + 
  geom_point(aes(y = tmin), size = 0.1) + 
  colorspace::scale_fill_continuous_sequential(
    "Rocket",  n_interp = 7, cmax = 90, rev = TRUE, c2 = 40, l2= 85, c1 = 20, l1 = 30,
    name = "Var. temp. diff.") +
  colorspace::scale_colour_continuous_sequential(
    "Rocket",  n_interp = 7, cmax = 90, rev = TRUE, c2 = 40, l2= 85, c1 = 20, l1 = 30,
    name = "Var. temp. diff.") +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") + 
  labs(x = "Month", y = "Temperature") + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        legend.position = "bottom")

out <- bscols(
  ggplotly(p1, width = 900, height = 700, tooltip = "label") |>
    highlight(on = "plotly_selected", off = "plotly_deselect", opacityDim = 0.1),
  ggplotly(p2, width = 1200, height = 700, tooltip = "label") |> 
    highlight(on = "plotly_selected", off = "plotly_deselect", opacityDim = 0.012),
  widths = c(5, 5)
)

htmltools::save_html(out, file = "figures/linking.html")
