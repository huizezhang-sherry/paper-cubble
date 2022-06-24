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


## ----face, echo = FALSE, fig.align="center", out.width = "100%", fig.cap = "An illustration of function \\code{face\\_temporal} and \\code{face\\_spatial} in cubble. In the first row, \\code{face\\_temporal} switches a cubble from the nested form into the long form and the focus has switched from the spatial aspect (the side face) to the temporal aspect (the front face). In the second row, \\code{face\\_spatial} switches a cubble back to the nested form from the long form and shifts focus back to the spatial aspect."----
knitr::include_graphics(here::here("figures/diagram-keynotes/diagram-keynotes.002.png"))


## ----echo = TRUE--------------------------------------------------------------
cubble_back <- cubble_long |> face_spatial()
cubble_back
identical(cubble_nested, cubble_back)


## ----echo = TRUE--------------------------------------------------------------
cubble_unfold <- cubble_long |> unfold(long, lat)
cubble_unfold


## ----echo = TRUE--------------------------------------------------------------
cubble_unfold |> face_spatial() |> face_temporal()


## ----echo = TRUE--------------------------------------------------------------
climate_flat_ts <- climate_flat |> 
  tsibble::as_tsibble(key = id, index = date)
climate_flat_cb <-  climate_flat_ts |> 
  cubble::as_cubble(coords = c(long, lat))
climate_flat_cb


## ----echo = TRUE--------------------------------------------------------------
# add station-based features in the nested form.
climate_flat_cb |> 
  mutate(
    fabletools::features(
      ts, tmax, list(tmax_mean = mean, tmax_var = var)
      )
  )


## ----echo = TRUE, eval = FALSE------------------------------------------------
## # read in the .nc file as a ncdf4 class
## raw <- ncdf4::nc_open(here::here("data/era5-pressure.nc"))
## 
## # convert the variable q and z in the ncdf4 into a cubble
## dt <- as_cubble(raw, vars = c("q", "z"))


## ----eval = FALSE, echo = TRUE------------------------------------------------
## # Assume my_ncdf has a bounding box of [-180, -90, 180, -90]
## # at 0.25 degree resolution and subset it to have
## # 1 degree resolution:
## dt <- as_cubble(my_ncdf, vars = c("q", "z"),
##                 long_range = seq(-180, 180, 1),
##                 lat_range = seq(-90, 90, 1))

