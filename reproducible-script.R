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
library("here")


## -------------------------------------------------------------------------------
cb_spatial <- climate_mel


## ----echo = TRUE----------------------------------------------------------------
cb_spatial


## -------------------------------------------------------------------------------
cb_temporal <- face_temporal(climate_mel)


## ----echo = TRUE----------------------------------------------------------------
cb_temporal


## ----echo = TRUE----------------------------------------------------------------
spatial(cb_temporal)


## ----echo = TRUE----------------------------------------------------------------
make_cubble(spatial = stations, temporal = meteo,
            key = id, index = date, coords = c(long, lat))


## ----echo = TRUE----------------------------------------------------------------
climate_flat |> as_cubble(key = id, index = date, coords = c(long, lat))


## ----cubble-fun, echo = TRUE----------------------------------------------------
face_temporal(cb_spatial)


## ----cubble-fun2, echo = TRUE---------------------------------------------------
face_spatial(face_temporal(cb_spatial))




## ----echo = TRUE----------------------------------------------------------------
cb_temporal |> unfold(long, lat)


## ----echo = TRUE----------------------------------------------------------------
class(meteo_ts)
ts_spatial <- make_cubble(
  spatial = stations, temporal = meteo_ts, coords = c(long, lat))
(ts_temporal <- face_temporal(ts_spatial))
class(ts_temporal)


## ----echo = TRUE----------------------------------------------------------------
ts_temporal |> has_gaps()


## ----echo = TRUE----------------------------------------------------------------
cb_temporal |> make_temporal_tsibble() 


## ----echo = TRUE----------------------------------------------------------------
(sf_spatial <- make_cubble(
  spatial = stations_sf, temporal = meteo, 
  key = id, index = date))
class(sf_spatial)


## ----echo =TRUE, message=FALSE--------------------------------------------------
sf_spatial |> sf::st_transform(crs = "EPSG:3857")


## ----echo = TRUE----------------------------------------------------------------
cb_spatial |> make_spatial_sf() 




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


## ----glyphmap, out.width="100%", fig.width=15, fig.height=10, fig.cap="Glyph maps comparing temperature change between 1971-1975 and 2016-2020 for 54 stations in Victoria and New South Wales, Australia. Overlaid line plots show monthly temperature (a) where a hint of late summer warming can be seen. Transforming to temperature differences (c) shows pronounced changes between the two periods. The horizontal guideline marks zero difference. One station, Cobar, is highlighted in the glyph maps and shown separately (b, d). Here the late summer (Jan-Feb) warming pattern, which is more prevalent at inland locations, is clear."----
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
  mutate(group = factor(group, levels = c("1971", "2016"), 
     labels = c("1971-1975", "2016-2020"))) |>
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
res_sp <- match_spatial(df1 = climate_vic, df2 = river, 
                        spatial_n_group = 10)
print(res_sp, n = 20)


## ----echo = TRUE----------------------------------------------------------------
res_sp <- match_spatial(
  df1 = climate_vic, df2 = river, 
  spatial_n_group = 10, return_cubble = TRUE)
(res_sp <- res_sp[-c(5, 8)] |> bind_rows())


## ----echo = TRUE----------------------------------------------------------------
(res_tm <- match_temporal(data = res_sp,
                          data_id = type, match_id = group,
                          temporal_by = c("prcp" = "Water_course_level")))


## ----echo = TRUE----------------------------------------------------------------
res_tm <- match_temporal(data =  res_sp,
                         data_id = type, match_id = group,
                         temporal_by = c("prcp" = "Water_course_level"),
                         return_cubble = TRUE)
(res_tm <- res_tm |> bind_rows() |> filter(group %in% c(1, 7, 6, 9)))


## ----matching, out.width="100%", fig.height = 5, fig.width = 10, fig.cap="Example of matching weather stations and river gauges. These four stations show on the map (a) and time (b) would be considered to be matching. Precipitation and water level have been standardised between 0 and 1 to be displayed in the same scale in (b). The peaks in the time series roughly match, and would reflect percipitation increasing water levels."----
res_tm <- res_tm |>  filter(group %in% c(1, 7, 6, 9))

res_tm_long <- res_tm |>  
  face_temporal() |>  
  unfold(group, type) |>  
  group_by(group, type) |>
  mutate(matched = (matched - min(matched, na.rm = TRUE))/ 
           (max(matched, na.rm = TRUE) - min(matched, na.rm = TRUE))) 

vic_map <- ozmaps::abs_ste |> 
  filter(NAME == "Victoria") |>
  st_transform("EPSG:28356") |> 
  sf::st_simplify(dTolerance = 3000)

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
  coord_sf(crs = "EPSG:4283") + 
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


## ----netcdf, out.width="100%", fig.height = 4, fig.width = 10, fig.cap = "An example illustrating that cubble can be used to readily reproduce common spatiotemporal analyses. This plot of ERA5 reanalysis (Fig. 19, Hersbach et al, 2020) shows the break-up of the southern polar vortex in late September and early October 2002. The polar vortex, signalled by the high specific humidity, splits into two on 2002-09-26 and further splits into four on 2002-10-04.", dev = "png", fig.retina=4----
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
  geom_point(aes(x = long, y = lat, color = q)) + 
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

