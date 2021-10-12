library(tidyverse)
library(cubble)
library(s2)
library(ggplot2)
state_map <- rmapshaper::ms_simplify(ozmaps::abs_ste, keep = 2e-3)
# plot_map(state_map) +
#   geom_point(data = weatherdata::climate_large, aes(x = longitude, y = latitude))
vic_map <- ozmaps::abs_ste %>% filter(NAME == "Victoria")
vic_stations <- weatherdata::climate_large %>% 
  rename(long = longitude, lat = latitude) %>% 
  mutate(ll = s2_lnglat(long, lat)) %>% 
  filter(s2_within(ll, vic_map)) 

save(vic_stations, file = here::here("data/vic_stations.rda"))
