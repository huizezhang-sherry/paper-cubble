library(cubble)
library(tidyverse)
library(leaflet)
library(leafpop)

clean <-  weatherdata::climate_full %>%
  filter(!id %in% c("ASN00067033", "ASN00072091")) %>% 
  stretch() %>%
  mutate(month = lubridate::month(date),
         diff = tmax - tmin) %>% 
  group_by(month) %>% 
  summarise(tmax = mean(tmax, na.rm = TRUE),
            diff = mean(diff, na.rm = TRUE)) %>% 
  ungroup(month) %>% 
  mutate(dummy_date = as.Date(glue::glue("2021-{month}-01"))) %>% 
  tamp() %>%
  mutate(a = nrow(ts)) %>% 
  filter(a == 12) %>% 
  mutate(diff_winter = ts %>% filter(month %in% c(6,7,8)) %>% pull(diff) %>% mean(na.rm = TRUE),
         diff_summer = ts %>% filter(month %in% c(12, 1,2)) %>% pull(diff) %>% mean(na.rm = TRUE),
         diff_year = diff_winter - diff_summer) %>% 
  stretch() %>% 
  mutate(dummy_date = as.Date(glue::glue("2021-{month}-01"))) %>%
  migrate(name) 

i <- 1
df_id <- clean$id %>% unique()
p <- purrr::map(1:length(df_id), function(i){
  dt <- clean %>% filter(id == df_id[i])
  dt %>% 
    ggplot(aes(x = dummy_date, ymin = tmax - diff, ymax = tmax, group = id)) + 
    geom_ribbon(color = "grey50", fill = "grey50", alpha = 0.5) + 
    theme_bw() + 
    scale_x_date(date_labels = "%b", date_breaks = "1 month") + 
    labs(x = "Month", y = "Temperature") + 
    ylim(-10, 50) + 
    ggtitle(paste0(stringr::str_to_title(unique(dt$name))))
})


for (i in 1:length(df_id)) {
  dt <- clean %>% 
    filter(id == df_id[i]) 
  
  p[[i]] <- dt %>% 
    ggplot(aes(x = dummy_date, ymin = tmax - diff, ymax = tmax, group = id)) + 
    geom_ribbon(color = "grey50", fill = "grey50", alpha = 0.5) + 
    theme_bw() + 
    scale_x_date(date_labels = "%b", date_breaks = "1 month") + 
    labs(x = "Month", y = "Temperature") + 
    ylim(-10, 50) + 
    ggtitle(paste0(stringr::str_to_title(unique(dt$name))))
  
  i <- i + 1
  
}

nested <- tamp(clean) 
domain <- nested$diff_year
pal <- colorNumeric(colorspace::sequential_hcl(palette = "Oslo", 
                                              n = 11),
                    domain = domain)

out <- leaflet(nested) %>% 
  addTiles() %>% 
  addCircleMarkers(group = "a", color = ~pal(domain), radius = 0.1) %>% 
  addPopupGraphs(graph = p, group = "a", width = 300, height =200)

htmlwidgets::saveWidget(out, file = "figures/popup.html")
