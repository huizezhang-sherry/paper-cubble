library(tidyverse)
library(GGally)
tmax <- weatherdata::historical_tmax %>%
  # filter out VIC and NSW stations
  filter(between(stringr::str_sub(id, 7, 8), 46, 90)) %>% 
  
  # filter out observations in between 1971-1975 and 2016 - 2020
  stretch() %>%
  filter(lubridate::year(date) %in% c(1971:1975, 2016:2020)) %>%
  group_by(month = lubridate::month(date), 
         group = as.factor(ifelse(lubridate::year(date) > 2015, 
                                  "2016 ~ 2020", "1971 ~ 1975"))) %>%
  # summarise tmax into monthly mean for both periods
  summarise(tmax = mean(tmax, na.rm = TRUE)) %>% 
  
  # filter out stations that have complete records in both periods 
  tamp() %>%
  filter(nrow(ts) == 24) %>% 
  
  # migrate longitude and latitude to the long form for making the glyph map
  stretch() %>%
  migrate(latitude, longitude)


nsw_vic <- ozmaps::abs_ste %>% filter(NAME %in% c("Victoria", "New South Wales"))

p1 <- ggplot() + 
  geom_sf(data = nsw_vic, fill = "transparent", color = "grey", linetype = "dotted") + 
  geom_glyph(data = tmax, 
             aes(x_major = longitude, x_minor = month, 
                 y_major = latitude, y_minor = tmax,
                 group = interaction(id, group), color = group),
             width = 1, height = 0.5) +
  scale_color_brewer(palette = "Dark2") + 
  theme_bw() + 
  coord_sf(xlim = c(141, 154)) + 
  theme(legend.position = "bottom") +
  labs(x = "Longitude", y = "Latitude")


#################
# inset
tmax %>% filter(id == "ASN00048027") %>%
  ggplot(aes(x = month,
             y = tmax,
             color = group)) +
  geom_line(size = 1.5) +
  scale_color_brewer(palette = "Dark2", guide = "none") +
  scale_x_continuous(breaks = seq(1, 12, 1)) +
  labs(x = "Month", y  = "Temperature", title = "ASN00048027: Cobar") +
  theme_bw() +
  theme(
    aspect.ratio = 0.3,
    axis.text = element_text(size = 20),
    title =  element_text(size = 20)
  )

#ggsave(filename = here::here("figures/basic-manip-inset.png"), width = 7)

box_df <- tmax %>% tamp() %>% filter(id == "ASN00048027")
single <-
  tibble::tibble(img = here::here("figures/basic-manip-inset.png"))
p1 +
  geom_rect(
    data = box_df,
    aes(
      xmin = longitude - 0.6,
      xmax = longitude + 0.6,
      ymin = latitude - 0.12,
      ymax = latitude + 0.35
    ),
    fill = "transparent",
    color = "black"
  ) +
  ggimg::geom_point_img(data = single,
                        aes(x = 143, y = -29, img = img), size = 6)

ggsave(filename = "figures/basic-manip.png", width = 10, height = 10)
