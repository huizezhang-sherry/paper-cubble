library(cubble)
library(dplyr)
library(ggplot2)
library(tsibble)

set.seed(123)
dt <- prcp_aus %>% slice_sample(n = 50)

###########################
# from map to ts
state_map <- rmapshaper::ms_simplify(ozmaps::abs_ste, keep = 2e-3)
plot_map(state_map) +
  geom_point(data = dt, aes(x = long, y = lat)) + 
  geom_point(data = dt %>% filter(id == "ASN00003057"), aes(x = long, y = lat), 
             color = "red", size =5) 

ggsave(filename = "figures/keynotes-figures/illu-highlighted-map.png")

plot_map(state_map) +
  geom_point(data = dt, aes(x = long, y = lat)) 
  
ggsave(filename = "figures/keynotes-figures/illu-default-map.png")

long <- dt %>% 
  face_temporal() %>% 
  mutate(month = lubridate::month(date)) %>% 
  group_by(month) %>% 
  summarise(tmax = mean(tmax, na.rm = TRUE)) %>% 
  mutate(dummy_date = as.Date(glue::glue("2021-{month}-01")))

highlight_line <- long %>% filter(id == "ASN00003057")
#highlight_point <- highlight_line %>% filter(month == 9)
long %>% 
  ggplot(aes(x = dummy_date, y = tmax, group = id)) +
  #geom_point(size = 0.3) + 
  geom_line() + 
  geom_line(data = highlight_line, color = "red", size = 1.5) + 
  #geom_point(data = highlight_point, color = "red") + 
  scale_x_date(date_labels = "%b") + 
  xlab("Month")
  
ggsave(filename = "figures/keynotes-figures/illu-highlighted-long.png", width = 5, height = 3)

long %>% 
  ggplot(aes(x = dummy_date, y = tmax, group = id)) +
  geom_line() + 
  scale_x_date(date_labels = "%b") + 
  xlab("Month")

ggsave(filename = "figures/keynotes-figures/illu-default-long.png", width = 5, height = 3)

###########################
# from ts to map

highlight_line <- long %>% filter(id == "ASN00003057")
highlight_point <- highlight_line %>% filter(month == 9)
long %>% 
  ggplot(aes(x = dummy_date, y = tmax, group = id)) +
  geom_point(size = 0.3) + 
  geom_line() + 
  geom_point(data = highlight_point, color = "red", size = 3) + 
  scale_x_date(date_labels = "%b") + 
  xlab("Month")

ggsave(filename = "figures/keynotes-figures/illu-highlighted-long-point.png", width = 5, height = 3)


