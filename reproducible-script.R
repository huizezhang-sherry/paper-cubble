## ----setup, echo = FALSE--------------------------------------------------------
knitr::opts_chunk$set(
  echo = FALSE,
  warning = FALSE,
  message = FALSE,
  cache = TRUE,
  fig.path = here::here("figures/")
)

options(prompt = "R> ", continue = "+ ",
        tibble.print_max = 5, tibble.print_min = 5,
        width = 80, styler.cache_root = "styler-perm")


## ----echo = FALSE---------------------------------------------------------------
library("cubble")
library("dplyr")
library("tidyr")
library("ggplot2")
library("patchwork")
library("tsibble")
library("sf")
library("lubridate")
library("ozmaps")
library("ggthemes")
library("ggrepel")
library("ncdf4")
library("rnaturalearth")
library("colorspace")
library("units")


## -------------------------------------------------------------------------------
cb_nested <- climate_mel


## ----echo = TRUE----------------------------------------------------------------
cb_nested


## -------------------------------------------------------------------------------
cb_long <- face_temporal(climate_mel)


## ----echo = TRUE----------------------------------------------------------------
cb_long


## ----class, fig.align="center", out.width = "100%", fig.cap = "Illustration of a cubble object in the long form and nested form, along with the associated attributes."----
#knitr::include_graphics(here::here("figures/diagram-keynotes/diagram-keynotes.004.png"))


## ----echo = TRUE----------------------------------------------------------------
spatial(cb_long)


## ----echo = TRUE----------------------------------------------------------------
make_cubble(spatial = stations, temporal = meteo,
            key = id, index = date, coords = c(long, lat))


## ----echo = TRUE----------------------------------------------------------------
climate_flat |> as_cubble(key = id, index = date, coords = c(long, lat))


## ----cubble-fun, echo = TRUE----------------------------------------------------
identical(face_temporal(cb_nested), cb_long)
identical(face_spatial(cb_long), cb_nested)


## ----cubble-fun2, echo = TRUE---------------------------------------------------
identical(face_spatial(face_temporal(cb_nested)), cb_nested)
identical(face_temporal(face_spatial(cb_long)), cb_long)


## ----face, echo = FALSE, fig.align="center", out.width = "100%", fig.cap = "An illustration of the function \\code{face\\_temporal()} and \\code{face\\_spatial()}: \\code{face\\_temporal()} converts a spatial cubble into a temporal cubbl to focus on the temporal variables. Conversely, \\code{face\\_spatial()} transforms a temporal cubble into a spatial one to for focus on the spatial variables."----
knitr::include_graphics(here::here("figures/diagram-keynotes/diagram-keynotes.001.png"))


## ----echo = TRUE----------------------------------------------------------------
ts_nested <- make_cubble(
  spatial = stations, temporal = meteo_ts, coords = c(long, lat))
(ts_long <- face_temporal(ts_nested))
class(ts_long)


## ----echo = TRUE----------------------------------------------------------------
ts_long |> has_gaps()


## ----echo = TRUE----------------------------------------------------------------
cb_long |> make_temporal_tsibble() 


## ----echo = TRUE----------------------------------------------------------------
(sf_nested <- make_cubble(
  spatial = stations_sf, temporal = meteo, 
  key = id, index = date))
class(sf_nested)


## ----echo =TRUE, message=FALSE--------------------------------------------------
sf_nested |> sf::st_transform(crs = "EPSG:3857")


## ----echo = TRUE----------------------------------------------------------------
cb_nested |> make_spatial_sf() 


## ----illu-interactive, echo = FALSE, fig.align="center", out.height = "35%", out.width = "100%", fig.cap = "Linking between multiple plots. The line plots and the map are constructed from shared \\code{crosstalk} objects. When a station is selected on the map (a), the corresponding row in the spatial \\code{cubble} will be activated. This will link to all the rows with the same id in the temporal \\code{cubble} (b) and update the line plot (c)."----
knitr::include_graphics(here::here("figures/diagram-keynotes/‎diagram-keynotes.‎002.png")) 


## ----covid-4, echo = TRUE, warning = TRUE, message= TRUE------------------------
cb <- make_cubble(lga, covid, by = c("lga_name_2018" = "lga"))


## ----covid-5, echo = TRUE-------------------------------------------------------
(check_res <- check_key(
  spatial = lga, temporal = covid, 
  by = c("lga_name_2018" = "lga")
))


## ----covid-6, echo = TRUE-------------------------------------------------------
lga2 <- lga |>
  rename(lga = lga_name_2018) |> 
  mutate(lga = ifelse(lga == "Kingston (C) (Vic.)", "Kingston (C)", lga),
         lga = ifelse(lga == "Latrobe (C) (Vic.)", "Latrobe (C)", lga))
  
covid2 <- covid |> filter(!lga %in% check_res$others$temporal)

(cb <- make_cubble(spatial = lga2, temporal = covid2))


## ----echo = TRUE----------------------------------------------------------------
a <- historical_tmax |> make_spatial_sf() |> st_distance()
a[upper.tri(a, diag = TRUE)] <- 1e6

(tmax <- historical_tmax |> 
  filter(rowSums(a < units::as_units(50, "km")) == 0))


## ----echo = TRUE----------------------------------------------------------------
(tmax <- tmax |>
  face_temporal() |> 
  group_by(
    yearmonth = tsibble::make_yearmonth(
      year = ifelse(lubridate::year(date) > 2015, 2016, 1971),
      month = lubridate::month(date))
  )|>
  summarise(tmax = mean(tmax, na.rm = TRUE)) |> 
  mutate(group = as.factor(lubridate::year(yearmonth)),
         month = lubridate::month(yearmonth)) |> 
  unfold(long, lat))


## ----echo = TRUE----------------------------------------------------------------
tmax <- tmax |> 
  face_spatial() |> 
  rowwise() |>
  filter(nrow(ts) == 24) |>
  face_temporal()


## ----glyphmap, out.width="100%", fig.width=15, fig.height=10, fig.cap="Comparison of average maximum temperature between 1971-1975 and 2016-2020 for 54 stations in Victoria and New South Wales, Australia. (a) and (b): the monthly temperature series for the two periods in a glyph map and for a single station Cobar, highlighted in orange in the glyph map. (c) and (d): the difference series between the two periods (2016s minus 1971s) in a glyph map and for station Cobar. The grey horizontal line marks zero difference. The glyph map displaying the difference series (c) reveals more pronounced changes between the two periods, with many inland locations in New South Wales show an increased temperature in late summer (Jan-Feb) in recent years."----
tmax2 <- tmax |> 
  pivot_wider(id_cols = -yearmonth, names_from = group, values_from = tmax) |> 
  mutate(diff = `2016` - `1971`) |> 
  mutate(x = long + cubble:::rescale11(month) * 0.8/2, 
         y = lat + last(cubble:::rescale11(c(diff, 0)) * 0.3/2))

box_df <- tmax |> filter(id == "ASN00048027")
nsw_vic <- ozmaps::abs_ste |> filter(NAME %in% c("Victoria","New South Wales"))

cobar1 <-  tmax |> filter(id == "ASN00048027") |>
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
  theme(aspect.ratio = 0.3,panel.grid.minor = element_blank())

cobar2 <- tmax2 |> filter(id == "ASN00048027") |>
  ggplot(aes(x = month, y = diff)) +
  geom_hline(yintercept = 0, color = "grey", linewidth = 0.8) + 
  geom_line() +
  scale_color_brewer(palette = "Dark2", guide = "none") +
  scale_x_continuous(
    breaks = seq(1, 12, 1),
    labels = c("J", "F", "M", "A",
               "M", "J", "J", "A",
               "S", "O", "N", "D")
  ) +
  labs(x = "", y  = "Temp. diff. (C)") +
  theme_bw() + 
  theme(aspect.ratio = 0.3,panel.grid.minor = element_blank())

p1 <- tmax |> 
  ggplot(aes(x_major = long, x_minor = month, 
             y_major = lat, y_minor = tmax,
             group = interaction(id, group))) + 
  geom_sf(data =  nsw_vic, color = "white", 
          fill = "grey95", inherit.aes = FALSE) + 
  geom_glyph_box(width = 0.8, height = 0.3) + 
  geom_glyph(aes(color = group), width = 0.8, height = 0.3) +
  geom_rect(data = box_df,
            aes(xmin = long - 0.6, xmax = long + 0.6,
                ymin = lat - 0.2, ymax = lat + 0.2),
            fill = "transparent", color = "orange", inherit.aes = FALSE) +
  scale_color_brewer("", palette = "Dark2") + 
  coord_sf(xlim = c(141.2, 153.4), ylim = c(-39.2, -28.9)) + 
  ggthemes::theme_map() +
  theme(legend.position = "bottom", legend.text = element_text(size = 10)) + 
  guides(color = guide_legend(override.aes = list(size=2)))

p2 <- tmax2 |> 
  ggplot(aes(x_major = long, x_minor = month, 
             y_major = lat, y_minor = diff)) + 
  geom_sf(data =  nsw_vic, color = "white", 
          fill = "grey95", inherit.aes = FALSE) + 
  geom_path(aes(x = x, y = y, group = id), color = "grey", linewidth = 0.8) + 
  geom_glyph(width = 0.8, height = 0.3) +
  geom_rect(data = box_df,
    aes(xmin = long - 0.6, xmax = long + 0.6,
        ymin = lat - 0.2, ymax = lat + 0.2),
    fill = "transparent", color = "orange", inherit.aes = FALSE) +
  scale_color_brewer("", palette = "Dark2") + 
  coord_sf(xlim = c(141.2, 153.4), ylim = c(-39.2, -28.9)) + 
  ggthemes::theme_map() +
  theme(legend.position = "bottom", legend.text = element_text(size = 10)) + 
  guides(color = guide_legend(override.aes = list(size=2)))

((p1/cobar1) | (p2/cobar2)) + 
  patchwork::plot_layout(guides = "collect") + 
  plot_annotation(tag_levels = 'a')  &
  theme(legend.position='bottom')


## ----echo = TRUE----------------------------------------------------------------
climate_vic <- climate_aus |>
  filter(between(as.numeric(substr(id, 7, 8)), 76, 90)) |>
  mutate(type = "climate")
river <- cubble::river |> mutate(type = "river") 


## ----echo = TRUE----------------------------------------------------------------
res_sp <- match_spatial(climate_vic, river, spatial_n_group = 10)
print(res_sp, n = 20)


## ----echo = TRUE----------------------------------------------------------------
res_sp <- match_spatial(
  climate_vic, river, 
  spatial_n_group = 10, return_cubble = TRUE)
(res_sp <- res_sp[-c(5, 8)] |> bind_rows())


## ----echo = TRUE----------------------------------------------------------------
(res_tm <- res_sp |> 
  match_temporal(
    data_id = type, match_id = group,
    temporal_by = c("prcp" = "Water_course_level")))


## ----echo = TRUE----------------------------------------------------------------
res_tm <- res_sp |> 
  match_temporal(
    data_id = type, match_id = group,
    temporal_by = c("prcp" = "Water_course_level"),
    return_cubble = TRUE)
(res_tm <- res_tm |> bind_rows() |> filter(group %in% c(1, 7, 6, 9)))


## ----matching, out.width="100%", fig.height = 5, fig.width = 10, fig.cap="Matched weather stations and river gauges on the map (a) and across time (b). Precipitation and water level have been standardised between 0 and 1 to be displayed in the same scale in (b). The water level reflects the increase in precipitation."----
res_tm <- res_tm |>  filter(group %in% c(1, 7, 6, 9))

res_tm_long <- res_tm |>  
  face_temporal() |>  
  unfold(group, type) |>  
  group_by(group, type) |>
  mutate(matched = (matched - min(matched, na.rm = TRUE))/ 
           (max(matched, na.rm = TRUE) - min(matched, na.rm = TRUE))) 

vic_map <- ozmaps::abs_ste |> 
  filter(NAME == "Victoria") |> 
  rmapshaper::ms_simplify()  

p1 <-ggplot() + 
  geom_sf(data = vic_map, fill = "grey95", color = "white") + 
  geom_point(data = dplyr::bind_rows(river, climate_vic), 
             aes(x = long, y = lat, color = type), 
             alpha = 0.2, fill = 0.2) +
  geom_point(data = res_tm |> as_tibble(), 
             aes(x = long, y = lat, color = type)) +
  ggrepel::geom_label_repel(
    data = res_tm |> filter(type == "river") |> as_tibble(), 
    aes(x = long, y = lat, label = group)) +
  scale_color_brewer(palette = "Dark2")  + 
  ggthemes::theme_map() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15)) +
  labs(x = "Longitude", y = "Latitude") + 
  guides(color = guide_legend(override.aes = list(size=5)))

p2 <- res_tm_long |>  
  ggplot(aes(x = date, y = matched, group = type,color = type)) + 
  geom_line() + 
  facet_wrap(vars(group)) + 
  scale_color_brewer(palette = "Dark2", guide = "none") + 
  theme_bw() + 
  labs(x=  "date") + 
  scale_x_date(date_labels = "%b") + 
  labs(x = "Week", y = "Precipitation/ water level")

(p1 | p2) + 
  patchwork::plot_layout(guides = "collect") + 
  plot_annotation(tag_levels = 'a')&
  theme(legend.position = "bottom") 


## ----echo = TRUE----------------------------------------------------------------
raw <- ncdf4::nc_open(here::here("data/era5-pressure.nc"))
(dt <- as_cubble(
  raw, vars = c("q", "z"),
  long_range = seq(-180, 180, 1), lat_range = seq(-88, -15, 1)))


## ----netcdf, out.width="100%", fig.height = 4, fig.width = 10, fig.cap = "A reproduction of the second row (ERA5 data) of Figure 19 in Hersbach et al (2020) to illustrate the break-up of sourthern polar vortex in late September and early October 2002. The polar vortex, signalled by the high specific humidity, splits into two on 2002-09-26 and further splits into four on 2002-10-04.", dev = "png"----
res <- dt |> 
  face_temporal() |> 
  filter(lubridate::date(time) %in% 
           as.Date(c("2002-09-22", "2002-09-26",
                     "2002-09-30", "2002-10-04"))) |>
  unfold(long, lat) |> 
  mutate(q = q* 10^6)

con <- rnaturalearth::ne_coastline("small", returnclass = "sf")
box <- st_bbox(c(xmin = -180, ymin = -90, xmax = 180, ymax = -15), crs = st_crs(con)) 
sf_use_s2(FALSE)
country <- con |> 
  st_geometry() |> 
  st_crop(box) |> 
  st_cast("MULTILINESTRING")

res |> 
  ggplot() +
  # q for specific humidity
  geom_point(aes(x = long, y = lat, color = q)) + 
  #geom_tile(aes(x = long, y = lat, fill = q), color = "transparent") +
  # z for geopotential
  geom_contour(data = res, aes(x = long, y = lat, z = z),
               color = "grey20", binwidth = 4000, linewidth = 0.5) +
  geom_sf(data = country , alpha = 0.5, fill = "transparent", color = "lightgreen") +
  coord_sf(
    crs = "+proj=stere +lat_0=-90 +lon_0=-180 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs", default_crs = st_crs(4326), clip = "off") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank()) +
  facet_wrap(vars(as.Date(time)), nrow = 1)+  
  colorspace::scale_color_continuous_sequential("Purple-Yellow", name = "Specific humidity") + 
  theme(legend.text = element_text(size = 13),
        legend.title = element_text(size = 13)) + 
  labs(x ="", y = "")


## ----interactive-linking, echo = FALSE, out.width="100%", out.height="23%", fig.retina = 2, dpi = 300, fig.cap = "Exploring temperature variation using linking of a map and seasonal display. Each row is a screen dump of the process. The top row shows all locations and all temperature profiles. Selecting a particular location on the map (here Mount Elizabeth) produces the plot in the second row. The maximum and minimum temperatures are shown using a ribbon. The bottom row first selects the lowest temperature in August in the seasonal display, which highlights the corresponding station on the map (Thredbo). Another  station, located in the Tasmania Island, is then selected to compare its temperature variation with the Thredbo station.", fig.show='hold'----
knitr::include_graphics(here::here("figures/linking.png"))
knitr::include_graphics(here::here("figures/linking-north.png"))
knitr::include_graphics(here::here("figures/linking-lower.png"))

