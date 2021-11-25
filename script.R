library(gstat)
library(tidyverse)
library(cubble)

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

