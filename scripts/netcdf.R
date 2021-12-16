library(tidyverse)
library(cubble)

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


