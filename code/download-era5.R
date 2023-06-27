# prepare data/era5-pressure.nc
library(ecmwfr)
my_request <- list(
  product_type = "reanalysis",
  format = "netcdf",
  pressure_level = "10",
  year = "2002",
  month = c("09", "10"),
  day = c("04", "22", "26", "30"),
  time = "00:00",
  area = c(-15, -180, -90, 180),
  variable = c("specific_humidity", 'geopotential'),
  dataset_short_name = "reanalysis-era5-pressure-levels",
  target = "era5-pressure.nc"
)
# 
# # here you need to create an account on Climate Data Store 
# # at https://cds.climate.copernicus.eu/cdsapp#!/home) to obtain the user ID and API key
# # then set up the with ecmwfr::wf_set_key(user, key, service = "cds")
# # the user below is the same `user` in the setup line
wf_request(
  user = "113339",
  request = my_request,
  transfer = TRUE,
  path = here::here("data/"))