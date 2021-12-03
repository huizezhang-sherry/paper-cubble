library(cubble)
library(dplyr)
library(ggplot2)
library(leaflet)
library(leafpop)

set.seed(123)
long <- weatherdata::climate_full %>% 
  slice_sample(n = 100) %>% 
  stretch() %>%
  mutate(ym = tsibble::yearmonth(date)) %>% 
  group_by(ym) %>%
  summarise(
    tmax = mean(tmax, na.rm = TRUE),
    tmin = mean(tmin, na.rm = TRUE),
    prcp = sum(prcp, na.rm = TRUE)
  ) %>%
  ungroup(ym) %>%
  mutate(month = lubridate::month(ym),
         year = as.factor(lubridate::year(ym))) %>% 
  migrate(name)

nested <- long %>% tamp()

i <- 1
df_id <- nested$id %>% unique()
p <- vector("list", length(df_id))
name <- unique(nested$name)
for (i in 1:length(df_id)) {
  dt <- long %>% 
    filter(id == df_id[i]) 
  
  p[[i]] <- dt %>% 
    ggplot(aes(
      x = month, y = tmax, group = year,
      color = year, label = name
    )) +
    geom_point(size = 0.5) + 
    geom_line() +
    scale_color_brewer(palette = "Reds",direction = 1) + 
    theme_bw() + 
    labs(x = "Month", title = name[i]) 

  i <- i + 1
  
}

leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(data = nested, group = "a", radius = 1) %>% 
  addPopupGraphs(graph = p, group = "a", width = 300, height =200)

