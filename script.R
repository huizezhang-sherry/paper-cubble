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

nc <- ncdf4::nc_open(file.choose())
temp <- as_cubble(nc, vars = "tp")

aus <- temp %>% 
  stretch() %>% 
  filter(between(lubridate::date(time), as.Date("2021-11-17"), as.Date("2021-11-23"))) %>% 
  tamp() %>% 
  mutate(long = floor(long), lat = floor(lat), id2 = paste0(long, lat)) %>% 
  switch_key(id2) %>% 
  filter(s2::s2_within(s2::s2_lnglat(long, lat), sf::st_union(state_map))) 

out <- aus %>% 
  stretch(ts) %>% 
  mutate(tp = tp * 39 * 31) %>% 
  group_by(id) %>% 
  summarise(tp = sum(tp, na.rm = TRUE)) %>% 
  ungroup(id) %>% 
  summarise(tp = mean(tp, na.rm = TRUE))

res <- out %>%
  migrate(long, lat) %>% 
  distinct()

state_map <- rmapshaper::ms_simplify(ozmaps::abs_ste, keep = 2e-3) 

state_string <- state_map %>% sf::st_cast("MULTILINESTRING") 
res %>% 
  ggplot() +
  geom_tile(aes(x = long, y = lat, fill = tp)) +
  geom_sf(data = state_string , alpha = 0.5, fill = "transparent") +
  theme_bw() + 
  colorspace::scale_fill_continuous_sequential(
    "Purple", breaks = seq(0, 150, 25)
    )

