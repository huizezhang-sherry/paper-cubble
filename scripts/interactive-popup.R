library(cubble)
library(dplyr)
library(ggplot2)
library(leaflet)
library(leafpop)

tas <- weatherdata::climate_full %>%
  filter(stringr::str_sub(id, 7, 8) > 90) %>% 
  stretch() %>%
  mutate(month = month(date),
         diff = tmax - tmin) %>% 
  filter(month %in% c(12, 1,2, 6,7,8)) %>% 
  mutate(dummy_date = as.Date(glue::glue("2021-{month}-01")),
         season = case_when(month %in% c(12, 1, 2) ~ "winter",
                            month %in% c(6,7,8) ~ "summer"))

i <- 1
df_id <- tas$id %>% unique()
p <- vector("list", length(df_id))
name <- unique(tas$name)
for (i in 1:length(df_id)) {
  dt <- tas %>% 
    filter(id == df_id[i]) 
  
  p[[i]] <- dt %>% 
    ggplot() + 
    geom_histogram(aes(x = diff, group = season), binwidth = 1) + 
    geom_vline(aes(xintercept = mean(diff, na.rm = TRUE)), 
               color = "red", data = ~ filter(.x, season == "summer")) + 
    geom_vline(aes(xintercept = mean(diff, na.rm = TRUE)), 
               color = "red", data = ~ filter(.x, season == "winter")) + 
    facet_wrap(vars(season), ncol = 1) + 
    theme_bw() + 
    xlab("temperature difference")
  
  i <- i + 1
  
}

tas_nested <- tamp(tas) %>%
  mutate(diff_winter = ts %>% filter(month %in% c(6,7,8)) %>% pull(diff) %>% mean(na.rm = TRUE),
         diff_summer = ts %>% filter(month %in% c(12, 1,2)) %>% pull(diff) %>% mean(na.rm = TRUE),
         diff_year = diff_winter - diff_summer)

domain <- range(tas_nested$diff_year)
pal <- colorNumeric(colorspace::diverging_hcl(palette = "Purple-Green", n = 100),
                    domain = domain)

leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(tas_nested, group = "a") %>% 
  addPopupGraphs(graph = p, group = "a", width = 300, height =200)

