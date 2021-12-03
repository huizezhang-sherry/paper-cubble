library(gstat)
library(tidyverse)
library(cubble)

data(wind)

wind_temporal <- wind %>% 
  pivot_longer(RPT:MAL, names_to = "code", values_to = "wind_speed") %>% 
  mutate(date = lubridate::make_date(year + 1900, month, day)) %>% 
  select(date, code, wind_speed)

wind.loc$y = as.numeric(sp::char2dms(as.character(wind.loc[["Latitude"]])))
wind.loc$x = as.numeric(sp::char2dms(as.character(wind.loc[["Longitude"]])))
coordinates(wind.loc) = ~x+y

wind_spatial <- wind.loc %>% 
  as_tibble() %>% 
  rename(long = x, lat = y, station = Station, code = Code) %>% 
  select(station, code, long, lat)
  
wind_cubble <- wind_temporal %>% left_join(wind_spatial) %>% 
  as_cubble(key = code, index = date, coords = c("long", "lat"))

wind_cubble %>% ggplot(aes(x = long, y = lat)) + geom_point()
ireland <- rnaturalearth::ne_countries(scale = 50, type = "countries", 
                   country = "Ireland", 
                   returnclass = "sf")
plot_map(ireland) + 
  geom_point(data = wind_cubble, aes(x = long, y = lat), shape = "+", color = "red", size = 5) + 
  geom_text(data = wind_cubble, aes(x = long, y = lat-0.05, label = station), size= 2)

######################################

nc <- ncdf4::nc_open(here::here("data/adaptor.mars.internal-1638344933.932894-4194-11-a5d35ac3-b36d-4982-afa5-a5dd193a6718.nc"))
temp <- as_cubble(nc, vars = "d2m")
vic_map <- ozmaps::abs_ste %>% filter(NAME == "Victoria")

out <- temp %>% 
  stretch(ts) %>% 
  mutate(d2m = d2m - 273.15) %>% 
  filter(lubridate::hour(time) == 12) %>% 
  group_by(id) %>% 
  summarise(d2m = mean(d2m, na.rm= TRUE))

res <- out %>%
  migrate(long, lat)

vic_string <- vic_map %>% sf::st_cast("MULTILINESTRING") 

res %>% 
  ggplot() +
  geom_tile(aes(x = long, y = lat, fill = d2m)) +
  geom_sf(data = vic_string , alpha = 0.5, fill = "transparent") +
  theme_bw() + 
  colorspace::scale_fill_continuous_sequential("Mint")


######################################
df <- dt %>%
  stretch() %>%
  mutate(month = month(date)) %>%
  group_by(month) %>%
  summarise(
    tmax = mean(tmax, na.rm = TRUE),
    tmin = mean(tmin, na.rm = TRUE),
    prcp = sum(prcp, na.rm = TRUE)
  ) %>%
  ungroup(month) %>%
  tamp() %>%
  mutate(median_tmax = median(ts$tmax, na.rm = TRUE))

nested <- df %>% 
  SharedData$new(~id, group = "cubbe")

long <- df %>%
  stretch() %>%
  mutate(dummy_date = as.Date(glue::glue("2021-{month}-01"))) %>%
  migrate(median_tmax, name) %>%
  SharedData$new(~id, group = "cubbe")

p1 <- long %>%
  ggplot(aes(
    x = dummy_date, y = tmax, group = name,
    color = median_tmax, label = name
  )) +
  geom_point(size = 0.5) + 
  geom_line() +
  scale_x_date(date_labels = "%b") + 
  scale_color_distiller(palette = "YlOrRd",direction = 1) + 
  theme_bw() + 
  theme(legend.position = "none") + 
  xlab("Month")

state_map <- rmapshaper::ms_simplify(ozmaps::abs_ste, keep = 2e-3)
p2 <- nested %>%
  ggplot() +
  geom_sf(data = state_map, aes(geometry = geometry)) +
  geom_point(aes(x = long, y = lat, color = median_tmax, label = name)) +
  scale_color_distiller(palette = "YlOrRd", direction = 1) + 
  theme_void()

out <- bscols(
  ggplotly(p1, width = 800, height = 800, tooltip = "label") %>%
    highlight(on = "plotly_selected", off = "plotly_deselect", color = "red"),
  ggplotly(p2, width = 800, height = 800, tooltip = "label") 
  %>%
    highlight(on = "plotly_selected", off = "plotly_deselect", color = "red")
)

htmltools::save_html(out, file = "figures/linking.html")
webshot2::webshot("figures/linking.html", "figures/linking.png", vwidth = 1800, vheight = 2000)
```
