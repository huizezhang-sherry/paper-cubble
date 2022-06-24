## -----------------------------------------------------------------------------
load(here::here("data/historical_tmax.rda"))


## ----echo = TRUE--------------------------------------------------------------
tmax <- historical_tmax %>%
  filter(between(stringr::str_sub(id, 7, 8), 46, 90))


## ----echo = TRUE--------------------------------------------------------------
tmax <- tmax %>%
  face_temporal() %>% 
  group_by(month = lubridate::month(date),
         group = as.factor(
           ifelse(lubridate::year(date) > 2015,
           "2016 ~ 2020", "1971 ~ 1975"))) %>%
  summarise(tmax = mean(tmax, na.rm = TRUE))


## ----echo = TRUE--------------------------------------------------------------
tmax %>% 
  face_spatial() %>% 
  mutate(n = nrow(ts)) %>%
  arrange(n) %>%
  pull(n) %>% 
  head(10)

tmax <- tmax %>% 
  face_spatial() %>% 
  filter(nrow(ts) == 24) %>%
  face_temporal()


## ----eval = FALSE, echo = TRUE------------------------------------------------
## nsw_vic <- ozmaps::abs_ste %>%
##   filter(NAME %in% c("Victoria", "New South Wales"))
## 
## ggplot() +
##   geom_sf(data = nsw_vic,
##           fill = "transparent", color = "grey",
##           linetype = "dotted") +
##   geom_glyph(data = tmax,
##              aes(x_major = long, x_minor = month,
##                  y_major = lat, y_minor = tmax,
##                  group = interaction(id, group), color = group),
##              width = 1, height = 0.5) +
##   ...


## -----------------------------------------------------------------------------
cobar <- tmax %>% filter(id == "ASN00048027") %>%
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



## ----basic-manip, fig.height=6, fig.cap="A glyph map of the monthly maximum average temperature for weather stations in  Victoria and New South Wales for the periods (1971-1975, 2016-2020). The corresponding average time series for the Cobar station are display on the top left corner. From the glyph map we can observe that the monthly trend is similar for all locations (low in the winter, high in the summer), and small increased temperatures, particularly in late summer can be seen at most stations in New South Wales."----
tmax <- tmax %>% unfold(long, lat)
box_df <- tmax %>% face_spatial() %>% filter(id == "ASN00048027")
nsw_vic <- ozmaps::abs_ste %>% filter(NAME %in% c("Victoria","New South Wales"))

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
  theme(legend.position = "bottom", legend.text = element_text(size = 15)) + 
  guides(color = guide_legend(override.aes = list(size=2)))

(cobar / p1) + 
  patchwork::plot_layout(heights = c(1,5)) + 
  plot_annotation(tag_levels = 'a')


