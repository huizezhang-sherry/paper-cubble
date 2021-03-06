## ----setup, echo = FALSE------------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  warning = FALSE,
  message = FALSE,
  cache = TRUE,
  fig.path = here::here("figures/")
)

options(prompt = "R> ", continue = "+ ",
        tibble.print_max = 5, tibble.print_min = 5,
        width = 70)


## ----echo = FALSE-------------------------------------------------------------
library(cubble)
library(tidyverse)
library(patchwork)

## -----------------------------------------------------------------------------
cubble::climate_flat


## ----echo = TRUE--------------------------------------------------------------
cubble_nested <- climate_flat |>
  as_cubble(key = id, index = date, coords = c(long, lat))
cubble_nested


## ----echo = TRUE--------------------------------------------------------------
cubble_nested |>
  mutate(rain_day = sum(ts$prcp != 0))


## ----echo = TRUE--------------------------------------------------------------
cubble_long <- cubble_nested |> face_temporal()
cubble_long


## ----echo = TRUE--------------------------------------------------------------
cubble_back <- cubble_long |> face_spatial()
cubble_back
identical(cubble_nested, cubble_back)


## ----echo = TRUE--------------------------------------------------------------
cubble_unfold <- cubble_long |> unfold(long, lat)
cubble_unfold


## ----echo = TRUE--------------------------------------------------------------
climate_flat_ts <- climate_flat |> 
  tsibble::as_tsibble(key = id, index = date)
climate_flat_cb <-  climate_flat_ts |> 
  cubble::as_cubble(coords = c(long, lat))
climate_flat_cb


## ----echo = TRUE--------------------------------------------------------------
climate_flat_cb |> 
  mutate(
    fabletools::features(
      ts, tmax, list(tmax_mean = mean, tmax_var = var)
      )
  )


## ----eval = FALSE-------------------------------------------------------------
## library(tidyverse)
## #https://www.coronavirus.vic.gov.au/victorian-coronavirus-covid-19-data
## raw <- read_csv(here::here("data/NCOV_COVID_Cases_by_LGA_Source_20220324.csv"),
##                   col_names = c("date", "postcode", "source", "lga"), skip = 1)
## 
## covid <- raw |>
##   count(date, lga, source, postcode) |>
##   filter(date >= as.Date("2022-01-01")) |>
##   filter(source == "Contact with a confirmed case") |>
##   group_by(date, lga, source) |>
##   count() |>
##   ungroup(date) |>
##   mutate(roll_mean = zoo::rollmean(n, k = 14, fill = NA)) |>
##   tsibble::as_tsibble(key = lga, index = date) |>
##   tsibble::fill_gaps(.full = TRUE)
## save(covid, file = here::here("data/covid.rda"))


## -----------------------------------------------------------------------------
load(here::here("data/covid.rda"))

lga <- strayr::read_absmap("lga2018") |>
  rename(lga = lga_name_2018) |>
  filter(state_name_2016 == "Victoria") 


## ----echo = TRUE--------------------------------------------------------------
covid |> head(5)


## ----echo = TRUE--------------------------------------------------------------
lga |> head(5)


## ----echo = TRUE, warning = TRUE, message= TRUE-------------------------------
cb <- as_cubble(
  list(spatial = lga, temporal = covid),
  key = lga, index = date, coords = c(cent_long, cent_lat)
  )


## ----echo = TRUE--------------------------------------------------------------
pair <- as_cubble(
  list(spatial = lga, temporal = covid),
  key = lga, index = date, coords = c(cent_long, cent_lat),
  output = "unmatch"
  )

pair


## ----echo = TRUE--------------------------------------------------------------
lga <- lga |>
  mutate(lga = ifelse(lga == "Kingston (C) (Vic.)", "Kingston (C)", lga),
         lga = ifelse(lga == "Latrobe (C) (Vic.)", "Latrobe (C)", lga)) |>
  filter(!lga %in% pair$others$spatial)

covid <- covid |> filter(!lga %in% pair$others$temporal)

cb <- as_cubble(data = list(spatial = lga, temporal = covid),
                key = lga, index = date, coords = c(cent_long, cent_lat))

## -----------------------------------------------------------------------------
load(here::here("data/historical_tmax.rda"))


## ----echo = TRUE--------------------------------------------------------------
tmax <- historical_tmax |>
  filter(between(stringr::str_sub(id, 7, 8), 46, 90))


## ----echo = TRUE--------------------------------------------------------------
tmax <- tmax |>
  face_temporal() |> 
  group_by(month = lubridate::month(date),
         group = as.factor(
           ifelse(lubridate::year(date) > 2015,
           "2016 ~ 2020", "1971 ~ 1975"))) |>
  summarise(tmax = mean(tmax, na.rm = TRUE))


## ----echo = TRUE--------------------------------------------------------------
tmax |> 
  face_spatial() |> 
  mutate(n = nrow(ts)) |>
  arrange(n) |>
  pull(n) |> 
  head(10)

tmax <- tmax |> 
  face_spatial() |> 
  filter(nrow(ts) == 24) |>
  face_temporal()

## -----------------------------------------------------------------------------
cobar <- tmax |> filter(id == "ASN00048027") |>
  ggplot(aes(x = month, y = tmax, color = group)) +
  geom_line() +
  scale_color_brewer(palette = "Dark2", guide = "none") +
  scale_x_continuous(
    breaks = seq(1, 12, 1), 
    labels = c("J", "F", "M", "A",
               "M", "J", "J", "A",
               "S", "O", "N", "D")
    ) +
  labs(x = "", y  = "Temp (C)") +
  theme_bw() + 
  theme(aspect.ratio = 0.3)



## ----basic-manip, out.width="100%", fig.height = 5, fig.width = 10, fig.cap="A glyph map of the monthly maximum average temperature for weather stations in  Victoria and New South Wales (NSW) for the periods (1971-1975, 2016-2020). The corresponding average time series for the cobar station are display on the top left corner. From the glyph map we can observe that the monthly trend is similar for all locations (low in the winter, high in the summer), and small increased temperatures, particularly in late summer can be seen at most stations in NSW."----
tmax <- tmax |> unfold(long, lat)
box_df <- tmax |> face_spatial() |> filter(id == "ASN00048027")
nsw_vic <- ozmaps::abs_ste |> filter(NAME %in% c("Victoria","New South Wales"))

p1 <- ggplot() + 
  geom_sf(data = nsw_vic, fill = "grey95", 
          color = "white", size=2) + 
  geom_glyph(
    data = tmax, 
    aes(x_major = long, x_minor = month, 
        y_major = lat, y_minor = tmax,
        group = interaction(id, group), color = group),
    width = 1.2, height = 0.8) +
    geom_rect(
      data = box_df,
      aes(xmin = long - 0.6, xmax = long + 0.6,
          ymin = lat - 0.12, ymax = lat + 0.35),
      fill = "transparent", color = "black") +
  scale_color_brewer("", palette = "Dark2") + 
  coord_sf(xlim = c(141, 154), ylim = c(-39, -28.5)) + 
  ggthemes::theme_map() +
  theme(legend.position = "bottom", legend.text = element_text(size = 10)) + 
  guides(color = guide_legend(override.aes = list(size=2)))

(p1 | cobar)  + 
  patchwork::plot_layout(width = c(1,1)) + 
  plot_annotation(tag_levels = 'a')


## -----------------------------------------------------------------------------
load(here::here("data/climate_full.rda"))

climate <- climate_full |>
  filter(between(stringr::str_sub(id, 7, 8), 76, 90)) |>
  face_temporal() |>
  filter(lubridate::year(date) == 2020) |>
  face_spatial() |>
  mutate(type = "climate")

river <- river |> mutate(type = "river")


## ----echo = TRUE--------------------------------------------------------------
res <- match_sites(
  river, climate,
  temporal_by = c("Water_course_level" = "prcp"),
  temporal_independent = "prcp",  
  temporal_n_highest = 30,
  temporal_min_match = 15, 
)


## -----------------------------------------------------------------------------
res |> print()


## ----matching, out.width="100%", fig.height = 5, fig.width = 10, fig.cap="Weather stations and river gauges with matched pairs labelled on the map (a) and plotted across time (b). Precipitation and water level have been standardised between 0 and 1 to be displayed on the same scale. The water level reflects the increase in precipitation. The numbers (1, 5, 6, 10) indicate the group index derived from spatial matching, only those that were selectd by temporal matching are shown here."----
vic_map <- ozmaps::abs_ste |> 
  filter(NAME == "Victoria") |> 
  rmapshaper::ms_simplify()  

p1 <-ggplot() + 
  geom_sf(data = vic_map, fill = "grey95", color = "white") + 
  geom_point(data = dplyr::bind_rows(river, climate), 
             aes(x = long, y = lat, color = type), alpha = 0.2, fill = 0.2) +
  geom_point(data = res, aes(x = long, y = lat, color = type)) +
  ggrepel::geom_label_repel(
    data = res |> filter(type == "river"), aes(x = long, y = lat, label = group)) +
  scale_color_brewer(palette = "Dark2")  + 
  ggthemes::theme_map() +
  ggplot2::theme(legend.position = "bottom",
                 legend.text = element_text(size = 15),
                 legend.title = element_text(size = 15)) +
  ggplot2::labs(x = "Longitude", y = "Latitude") + 
  guides(color = guide_legend(override.aes = list(size=5)))

res_long <- res |>
  face_temporal(ts) |>
  unfold(group, type) |>
  rename(prcp = matched_var) |> 
  mutate(prcp = (prcp - min(prcp, na.rm = TRUE))/ (max(prcp, na.rm = TRUE) - min(prcp, na.rm = TRUE))) 

p2 <- res_long |> 
  ggplot(aes(x = date, y = prcp, color = type, group = id)) +
  geom_line(alpha = 0.8) +
  facet_wrap(vars(group)) +
  scale_color_brewer(palette = "Dark2", guide = "none") +
  theme_bw() +
  scale_x_date(date_labels = "%b") + 
  labs(x = "Date", y = "Precipitation/ water level")

(p1 | p2) + 
  patchwork::plot_layout(guides = "collect") + 
  plot_annotation(tag_levels = 'a')&
  theme(legend.position = "bottom") 


## ----echo = TRUE--------------------------------------------------------------
raw <- ncdf4::nc_open(here::here("data/era5-pressure.nc"))
dt <- as_cubble(
  raw, vars = c("q", "z"),
  long_range = seq(-180, 180, 1), lat_range = seq(-88, 88, 1))


## ----eval = FALSE-------------------------------------------------------------
## # prepare data/era5-pressure.nc
## library(ecmwfr)
## my_request <- list(
##   product_type = "reanalysis",
##   format = "netcdf",
##   pressure_level = "10",
##   year = "2002",
##   month = c("09", "10"),
##   day = c("04", "22", "26", "30"),
##   time = "00:00",
##   area = c(-5, -180, -90, 180),
##   variable = c("specific_humidity", 'geopotential'),
##   dataset_short_name = "reanalysis-era5-pressure-levels",
##   target = "era5-pressure.nc"
## )
## #
## # # here you need to create an account on Climate Data Store
## # # at https://cds.climate.copernicus.eu/cdsapp#!/home) to obtain the user ID and API key
## # # then set up the with ecmwfr::wf_set_key(user, key, service = "cds")
## # # the user below is the same `user` in the setup line
## wf_request(
##   user = "113339",
##   request = my_request,
##   transfer = TRUE,
##   path = here::here("data/"))


## ----netcdf, out.width="100%", fig.height = 4, fig.width = 10, fig.cap = "A reproduction of the second row (ERA5 data) of Figure 19 in Hersbach et al (2020) to illustrate the break-up of sourthern polar vortex in late September and early October 2002. The polar vortex, signalled by the high speicfic humidity, splits into two on 2002-09-26 and furthers split into four on 2002-10-04."----
date <- c("2002-09-22", "2002-09-26", "2002-09-30", "2002-10-04") |> as.Date()
res <- dt |> 
  face_temporal() |> 
  filter(lubridate::date(time) %in% date) |>
  unfold(long, lat) |> 
  mutate(q =  q* 10^6)

box = c(xmin = -180, ymin = -90, xmax = 180, ymax = -5)
sf::sf_use_s2(FALSE)
country <- rnaturalearth::ne_coastline("small", returnclass = "sf") |> 
  sf::st_crop(box) |> 
  sf::st_cast("MULTILINESTRING")

res |> 
  ggplot() +
  # q for specific humidity
  geom_point(aes(x = long, y = lat, color = q), size = 2) + 
  #geom_tile(aes(x = long, y = lat, fill = q), color = "transparent") +
  # z for geopotential
  geom_contour(data = res, aes(x = long, y = lat, z = z),
               color = "grey20", binwidth = 4000, size = 0.5) +
  geom_sf(data = country , alpha = 0.5, fill = "transparent", color = "lightgreen") +
  coord_sf(
    crs = "+proj=stere +lat_0=-90 +lon_0=-180 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs ", default_crs = sf::st_crs(4326), label_graticule = "EW") + 
  theme_bw() + 
  theme(legend.position = "bottom",
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(), 
        panel.grid = element_blank()) + 
  facet_wrap(vars(as.Date(time)), nrow = 1, shrink = TRUE)+  
  colorspace::scale_color_continuous_sequential("Purple-Yellow", name = "Specific humidity") + 
  theme(legend.text = element_text(size = 13),
        legend.title = element_text(size = 13)) + 
  labs(x ="", y = "")


## ----eval = FALSE-------------------------------------------------------------
## # This chunk generates the interactive graphic and save it as figures/linking.html, where the three screencasts in the figure comes from.
## library("crosstalk")
## library("plotly")
## 
## clean <- climate_full |>
##   filter(!id %in% c("ASN00067033", "ASN00072091", "ASN00059040",
##                     "ASN00097053", "ASN00040856", "ASN00015548")) |>
##   face_temporal() |>
##   mutate(month = lubridate::month(date)) |>
##   group_by(month) |>
##   summarise(
##     tmax = mean(tmax, na.rm = TRUE),
##     tmin = mean(tmin, na.rm = TRUE),
##     diff = mean(tmax - tmin, na.rm = TRUE)
##     ) |>
##   ungroup(month) |>
##   mutate(dummy_date = as.Date(glue::glue("2021-{month}-01"))) |>
##   face_spatial() |>
##   mutate(temp_diff_var = ts |> pull(diff) |> var(na.rm = TRUE))
## 
## nested <- clean |>
##   SharedData$new(~id, group = "cubble")
## 
## long <- clean |>
##   face_temporal() |>
##   unfold(temp_diff_var) |>
##   arrange(temp_diff_var) |>
##   SharedData$new(~id, group = "cubble")
## 
## state_map <- rmapshaper::ms_simplify(ozmaps::abs_ste, keep = 2e-3)
## 
## p1 <- nested |>
##   ggplot() +
##   geom_sf(data = state_map, fill = "grey95", color = "white") +
##   geom_point(aes(x = long, y = lat,color = temp_diff_var, label = name)) +
##   colorspace::scale_color_continuous_sequential(
##     "Rocket",  n_interp = 7, cmax = 90, rev = TRUE,
##     c2 = 40, l2= 85, c1 = 20, l1 = 30, name = "Var. temp. diff.") +
##   labs(x = "Longitude", y = "Latitude") +
##   coord_sf() +
##   theme_bw() +
##   theme(
##     panel.grid.major = element_blank(),
##     legend.position = "none"
##     )
## 
## p2 <- long |>
##   ggplot(aes(x = dummy_date, group = id,
##              fill = temp_diff_var, color = temp_diff_var)) +
##   geom_ribbon(aes(ymin = tmin, ymax = tmax), size = 0.1, alpha = 0.3) +
##   geom_line(aes(y = tmin), alpha = 0.1) +
##   geom_line(aes(y = tmax), alpha = 0.1) +
##   geom_point(aes(y = tmax), size = 0.1) +
##   geom_point(aes(y = tmin), size = 0.1) +
##   colorspace::scale_fill_continuous_sequential(
##     "Rocket",  n_interp = 7, cmax = 90, rev = TRUE,
##     c2 = 40, l2= 85, c1 = 20, l1 = 30, name = "Var. temp. diff.") +
##   colorspace::scale_colour_continuous_sequential(
##     "Rocket",  n_interp = 7, cmax = 90, rev = TRUE,
##     c2 = 40, l2= 85, c1 = 20, l1 = 30, name = "Var. temp. diff.") +
##   scale_x_date(date_labels = "%b", date_breaks = "1 month") +
##   labs(x = "Month", y = "Temperature") +
##   theme_bw() +
##   theme(
##     panel.grid.major = element_blank(),
##     legend.position = "bottom"
##     )
## 
## out <- bscols(
##   ggplotly(p1, width = 900, height = 700, tooltip = "label") |>
##     highlight(
##       on = "plotly_selected",
##       off = "plotly_deselect",
##       opacityDim = 0.1),
##   ggplotly(p2, width = 1200, height = 700, tooltip = "label") |>
##     highlight(
##       on = "plotly_selected",
##       off = "plotly_deselect",
##       opacityDim = 0.012),
##   widths = c(5, 5)
## )
## 
## htmltools::save_html(out, file = "figures/linking.html")
## 

library("leaflet")
library("leafpop")


## ---- echo = FALSE------------------------------------------------------------
load(here::here("data/station_nested.rda"))
load(here::here("data/climate_full.rda"))


## ----eval = FALSE-----------------------------------------------
## # The kmeans clustering in station_nested data can takes some time to run,
## # This chunk runs the algorithm and save the result as staiton_nested for further use
## coords <- cbind(prcp_aus$long, prcp_aus$lat)
## dist_raw <- geosphere::distm(coords, coords)
## 
## station_long <- cubble::prcp_aus |>
##   face_temporal(ts) |>
##   mutate(wk = lubridate::week(date)) |>
##   group_by(wk) |>
##   summarise(prcp = sum(prcp, na.rm = TRUE)) |>
##   unfold(cluster)
## 
## set.seed(123)
## station_nested <- station_long |>
##   face_spatial() |>
##   strip_rowwise() |>
##   mutate(cluster = kmeans(dist_raw,centers = 20, nstart = 500)$cluster)
## save(station_nested, file = here::here("data/station_nested.rda"))


## ----basic-agg, fig.height = 7, fig.width = 15, fig.cap="Profile of aggregated precipitation at 639 weather stations in Australia. Subplot A shows the glyph map of the weekly averaged precipitation of each cluster. The group number is printed in the middle of the y minor axis and can be used as a reference line to read the magnitude. Subplot B shows the station membership of each cluster."----
load(here::here("data/station_nested.rda"))
cluster_nested <- station_nested |>
  switch_key(cluster) |> 
  get_centroid()

cluster_long <- cluster_nested |> 
  face_temporal() |> 
  group_by(wk) |>
  summarise(prcp = mean(prcp, na.rm = TRUE)) |> 
  unfold(cent_long, cent_lat)

state_map <- rmapshaper::ms_simplify(ozmaps::abs_ste, keep = 2e-3)

ggplot_smooth <- cluster_long |> 
  ggplot() +
  geom_smooth(aes(x = wk, y = prcp, group = cluster), span = 0.4) 

smoother <- layer_data(ggplot_smooth) |> 
  left_join(cluster_long |> select(cluster, cent_long, cent_lat), by = c("group" = "cluster"))

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
  ggforce::geom_mark_hull(data = cluster_nested |> tidyr::unnest(hull),
                          expand = 0, radius = 0,
                          aes(x = long, y = lat, group = cluster)) + 
  theme_void()

(p1 | p2) + plot_annotation(tag_levels = 'A')


## ----eval = FALSE -----------------------------------------------
## clean <-  climate_full |>
##   filter(!id %in% c("ASN00067033", "ASN00072091", "ASN00059040",
##                     "ASN00097053", "ASN00040856", "ASN00015548")) |>
##   face_temporal() |>
##   mutate(month = lubridate::month(date)) |>
##   group_by(month) |>
##   summarise(tmax = mean(tmax, na.rm = TRUE),
##             tmin = mean(tmin, na.rm = TRUE),
##             diff = mean(tmax - tmin, na.rm = TRUE)) |>
##   ungroup(month) |>
##   face_spatial() |>
##   mutate(temp_diff_var = ts |> pull(diff) |> var(na.rm = TRUE)) |>
##   face_temporal() |>
##   mutate(dummy_date = as.Date(glue::glue("2021-{month}-01"))) |>
##   unfold(name)
## 
## df_id <- clean$id |> unique()
## p <- purrr::map(1:length(df_id), function(i){
##   dt <- clean |> filter(id == df_id[i])
##   dt |>
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
## leaflet(nested) |>
##   addTiles() |>
##   addCircleMarkers(color = ~pal(domain), radius = 0.1,
##                    fillOpacity = 1, opacity = 1,
##                    popup = popupGraph(p, width = 300, height =200))


## ----interactive-popup, fig.align='center', echo = FALSE, out.width="45%", out.height="25%", fig.retina = 2, dpi = 300, fig.cap = "Same as Figure 11 except the temperature variation is now shown as a popup in the leaflet map.", fig.show='hold'----
knitr::include_graphics(here::here("figures/popup.png"))


## ----eval = FALSE-------------------------------------------------------------
## library(tidyverse)
## library(cubble)
## all_stations <- rnoaa::ghcnd_stations() |>
##   filter(str_starts(id, "ASN")) |> # Australian stations start wiht "ASN"
##   filter(last_year >= 2020) |>
##   mutate(wmo_id = as.numeric(wmo_id), name = str_to_lower(name)) |>
##   select(-state, -gsn_flag) |>
##   select(id, longitude, latitude, elevation, name,
##          wmo_id, element, first_year, last_year) |>
##   rename(long = longitude, lat = latitude, elev = elevation)
## 
## tmax_stations <- all_stations |>
##   filter(element == "TMAX", first_year < 1970, !is.na(wmo_id))
## 
## raw_tmax <- all_stations |>
##   rowwise() |>
##   mutate(ts = list(rnoaa::meteo_pull_monitors(
##     monitors = id, var = "TMAX",
##     date_min = glue::glue("{first_year}-01-01"),
##     date_max = glue::glue("{last_year}-12-31")
##     ) |>
##       select(-id)
##     )
##   )
## 
## historical_tmax <- raw_tmax |>
##   select(-element) |>
##   unnest(ts) |>
##   mutate(tmax = tmax/10) |>
##   filter(lubridate::year(date) %in% c(1971: 1975, 2016:2020)) |>
##   as_cubble(index = date, key = id, coords = c(long, lat))
## 
## save(historical_tmax, file = here::here("data/historical_tmax.rda"))


## ----eval = FALSE-------------------------------------------------------------
## aus_stations <- all_stations |>
##   filter(element %in% c("PRCP", "TMAX", "TMIN")) |>
##   nest(element: last_year) |>
##   rowwise() |>
##   filter(nrow(data) == 3) |>
##   select(-data)
## 
## aus_climate_raw <- aus_stations |>
##   rowwise() |>
##   mutate(ts = list(
##     rnoaa::meteo_pull_monitors(
##       monitors = id, var = c("PRCP", "TMAX", "TMIN"),
##       date_min = "2016-01-01", date_max = "2020-12-31"
##       ) |>
##       select(-id)
##     )
##   )
## 
## climate_full <- aus_climate_raw |>
##   unnest(ts) |>
##   mutate(tmax = tmax/10, tmin = tmin/10) |>
##   cubble::as_cubble(key = id, index = date, coords = c(long, lat))
## 
## save(climate_full, file = here::here("data/climate_full.rda"))

