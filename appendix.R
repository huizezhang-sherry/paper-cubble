## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)
library(tidyverse)
library(cubble)
library(patchwork)
library(leaflet)
library(leafpop)


## ---- echo = FALSE------------------------------------------------------------
load(here::here("data/station_nested.rda"))
load(here::here("data/climate_full.rda"))


## ----eval = FALSE, echo = TRUE------------------------------------------------
## station_nested <- climate_full %>% mutate(cluster = ...)


## ----eval = FALSE, echo = TRUE------------------------------------------------
## cluster_nested <- station_nested %>% switch_key(cluster)


## ----eval = FALSE, echo = TRUE------------------------------------------------
## cluster_nested <- cluster_nested %>% get_centroid()


## ----eval = FALSE, echo = FALSE-----------------------------------------------
## # The kmeans clustering in station_nested data can takes some time to run,
## # This chunk runs the algorithm and save the result as staiton_nested for further use
## coords <- cbind(prcp_aus$long, prcp_aus$lat)
## dist_raw <- geosphere::distm(coords, coords)
## 
## station_long <- cubble::prcp_aus %>%
##   face_temporal(ts) %>%
##   mutate(wk = lubridate::week(date)) %>%
##   group_by(wk) %>%
##   summarise(prcp = sum(prcp, na.rm = TRUE)) %>%
##   unfold(cluster)
## 
## set.seed(123)
## station_nested <- station_long %>%
##   face_spatial() %>%
##   strip_rowwise() %>%
##   mutate(cluster = kmeans(dist_raw,centers = 20, nstart = 500)$cluster)
## save(station_nested, file = here::here("data/station_nested.rda"))


## ----basic-agg, fig.height = 7, fig.width = 15, echo = FALSE, fig.cap="Profile of aggregated precipitation at 639 weather stations in Australia. Subplot A shows the glyph map of the weekly averaged precipitation of each cluster. The group number is printed in the middle of the y minor axis and can be used as a reference line to read the magnitude. Subplot B shows the station membership of each cluster."----
load(here::here("data/station_nested.rda"))
cluster_nested <- station_nested %>%
  switch_key(cluster) %>% 
  get_centroid()

cluster_long <- cluster_nested %>% 
  face_temporal() %>% 
  group_by(wk) %>%
  summarise(prcp = mean(prcp, na.rm = TRUE)) %>% 
  unfold(cent_long, cent_lat)

state_map <- rmapshaper::ms_simplify(ozmaps::abs_ste, keep = 2e-3)

ggplot_smooth <- cluster_long %>% 
  ggplot() +
  geom_smooth(aes(x = wk, y = prcp, group = cluster), span = 0.4) 

smoother <- layer_data(ggplot_smooth) %>% 
  left_join(cluster_long %>% select(cluster, cent_long, cent_lat), by = c("group" = "cluster"))

p1 <- ggplot(data = smoother, 
             aes(x_minor = x, y_minor = y, x_major = cent_long, y_major = cent_lat)) + 
  geom_sf(data = state_map, inherit.aes = FALSE,
          color = "grey80", alpha = 0.4, linetype = 3) + 
  geom_text(data = cluster_nested, 
            aes(x = cent_long, y = cent_lat, label = cluster), inherit.aes = FALSE) +
  geom_glyph(height = 2, width = 4) + 
  theme_void()

p2 <- ggplot() + 
  geom_sf(data = state_map, inherit.aes = FALSE,
          color = "grey80", alpha = 0.4, linetype = 3) + 
  geom_point(data = station_nested, aes(x = long, y = lat), size = 0.5) +
  ggforce::geom_mark_hull(data = cluster_nested %>% tidyr::unnest(hull),
                          expand = 0, radius = 0,
                          aes(x = long, y = lat, group = cluster)) + 
  theme_void()

(p1 | p2) + plot_annotation(tag_levels = 'A')


## ----eval = FALSE, echo = FALSE-----------------------------------------------
## clean <-  climate_full %>%
##   filter(!id %in% c("ASN00067033", "ASN00072091", "ASN00059040",
##                     "ASN00097053", "ASN00040856", "ASN00015548")) %>%
##   face_temporal() %>%
##   mutate(month = lubridate::month(date)) %>%
##   group_by(month) %>%
##   summarise(tmax = mean(tmax, na.rm = TRUE),
##             tmin = mean(tmin, na.rm = TRUE),
##             diff = mean(tmax - tmin, na.rm = TRUE)) %>%
##   ungroup(month) %>%
##   face_spatial() %>%
##   mutate(temp_diff_var = ts %>% pull(diff) %>% var(na.rm = TRUE)) %>%
##   face_temporal() %>%
##   mutate(dummy_date = as.Date(glue::glue("2021-{month}-01"))) %>%
##   unfold(name)
## 
## df_id <- clean$id %>% unique()
## p <- purrr::map(1:length(df_id), function(i){
##   dt <- clean %>% filter(id == df_id[i])
##   dt %>%
##     ggplot(aes(x = dummy_date, ymin = tmin, ymax = tmax, group = id)) +
##     geom_ribbon(color = "grey50", fill = "grey50", alpha = 0.5) +
##     theme_bw() +
##     scale_x_date(date_labels = "%b", date_breaks = "1 month") +
##     labs(x = "Month", y = "Temperature") +
##     ylim(-10, 50) +
##     ggtitle(paste0(stringr::str_to_title(unique(dt$name))))
## })
## 
## nested <- face_spatial(clean)
## domain <- nested$temp_diff_var
## pal <- colorNumeric(
##   colorspace::sequential_hcl(
##     "Rocket",  n = 7, cmax = 90, rev = TRUE,
##     c2 = 40, l2= 85, c1 = 20, l1 = 30),
##   domain = domain)
## 
## leaflet(nested) %>%
##   addTiles() %>%
##   addCircleMarkers(color = ~pal(domain), radius = 0.1,
##                    fillOpacity = 1, opacity = 1,
##                    popup = popupGraph(p, width = 300, height =200))


## ----interactive-popup, fig.align='center', echo = FALSE, out.width="45%", out.height="25%", fig.retina = 2, dpi = 300, fig.cap = "Same as Figure 11 except the temperature variation is now shown as a popup in the leaflet map.", fig.show='hold'----
knitr::include_graphics(here::here("figures/popup.png"))


## ----illu-interactive-2, echo = FALSE, fig.align="center", out.height="40%", out.width = "100%", fig.cap = "Linking between multiple plots. The line plots and the map are constructed from shared crosstalk objects (long and nested cubbles). When a point on the time series is selected, the corresponding row in the long cubble will be activated (a). This will link to all the rows with the same id in the long cubble and the row in the nested cubble with the same id (b). Both plots will be updated with the full line selected and the point highlighted on the map (c)."----
knitr::include_graphics(here::here("figures/diagram-keynotes/diagram-keynotes.005.png"))


## ----eval = FALSE-------------------------------------------------------------
## library(tidyverse)
## library(cubble)
## all_stations <- rnoaa::ghcnd_stations() %>%
##   filter(str_starts(id, "ASN")) %>% # Australian stations start wiht "ASN"
##   filter(last_year >= 2020) %>%
##   mutate(wmo_id = as.numeric(wmo_id), name = str_to_lower(name)) %>%
##   select(-state, -gsn_flag) %>%
##   select(id, longitude, latitude, elevation, name,
##          wmo_id, element, first_year, last_year) %>%
##   rename(long = longitude, lat = latitude, elev = elevation)
## 
## tmax_stations <- all_stations %>%
##   filter(element == "TMAX", first_year < 1970, !is.na(wmo_id))
## 
## raw_tmax <- all_stations %>%
##   rowwise() %>%
##   mutate(ts = list(rnoaa::meteo_pull_monitors(
##     monitors = id, var = "TMAX",
##     date_min = glue::glue("{first_year}-01-01"),
##     date_max = glue::glue("{last_year}-12-31")
##     ) %>%
##       select(-id)
##     )
##   )
## 
## historical_tmax <- raw_tmax %>%
##   select(-element) %>%
##   unnest(ts) %>%
##   mutate(tmax = tmax/10) %>%
##   filter(lubridate::year(date) %in% c(1971: 1975, 2016:2020)) %>%
##   as_cubble(index = date, key = id, coords = c(long, lat))
## 
## save(historical_tmax, file = here::here("data/historical_tmax.rda"))


## ----eval = FALSE-------------------------------------------------------------
## # all the Australian stations have all of the three PRCP, TMAX, and TMIN recorded
## aus_stations <- all_stations %>%
##   filter(element %in% c("PRCP", "TMAX", "TMIN")) %>%
##   nest(element: last_year) %>%
##   rowwise() %>%
##   filter(nrow(data) == 3) %>%
##   select(-data)
## 
## aus_climate_raw <- aus_stations %>%
##   rowwise() %>%
##   mutate(ts = list(
##     rnoaa::meteo_pull_monitors(
##       monitors = id, var = c("PRCP", "TMAX", "TMIN"),
##       date_min = "2016-01-01", date_max = "2020-12-31"
##       ) %>%
##       select(-id)
##     )
##   )
## 
## climate_full <- aus_climate_raw %>%
##   unnest(ts) %>%
##   mutate(tmax = tmax/10, tmin = tmin/10) %>%
##   cubble::as_cubble(key = id, index = date, coords = c(long, lat))
## 
## save(climate_full, file = here::here("data/climate_full.rda"))

