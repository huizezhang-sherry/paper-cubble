## ----echo = TRUE--------------------------------------------------------------
raw <- ncdf4::nc_open(here::here("data/era5-pressure.nc"))
dt <- as_cubble(
  raw, vars = c("q", "z"),
  long_range = seq(-180, 180, 1), lat_range = seq(-90, 90, 1))


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


## ----netcdf,  fig.height = 4, fig.width = 10, fig.cap = "A reproduction of the second row (ERA5 data) of Figure 19 in Hersbach et al (2020) to illustrate the break-up of sourthern polar vortex in late September and early October 2002. The polar vortex, signalled by the high speicfic humidity, splits into two on 2002-09-26 and furthers split into four on 2002-10-04."----

date <- c("2002-09-22", "2002-09-26", "2002-09-30", "2002-10-04") %>% as.Date()
res <- dt %>% 
  face_temporal() %>% 
  filter(lubridate::date(time) %in% date) %>%
  unfold(long, lat) %>% 
  mutate(q =  q* 10^6)

box = c(xmin = -180, ymin = -90, xmax = 180, ymax = -5)
sf::sf_use_s2(FALSE)
country <- rnaturalearth::ne_coastline("small", returnclass = "sf") %>% 
  sf::st_crop(box) %>% 
  sf::st_cast("MULTILINESTRING")

res %>% 
  ggplot() +
  # q for specific humidity
  geom_point(aes(x = long, y = lat, color = q)) +
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
  labs(x ="", y = "") 


