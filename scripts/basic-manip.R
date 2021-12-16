tmax <- weatherdata::historical_tmax %>%
  # filter out VIC and NSW stations
  filter(between(stringr::str_sub(id, 7, 8), 46, 90)) %>% 
  
  # filter out observations in between 1971-1975 and 2016 - 2020
  stretch() %>%
  filter(lubridate::year(date) %in% c(1971:1975, 2016:2020)) %>%
  mutate(month = lubridate::month(date), 
         group = as.factor(ifelse(lubridate::year(date) > 2015, 
                                  "2016 ~ 2020", "1971 ~ 1975"))) %>%
  group_by(month, group) %>%
  # summarise tmax into monthly mean for both periods
  summarise(tmax = mean(tmax, na.rm = TRUE)) %>% 
  
  # filter out stations that have complete records in both periods 
  tamp() %>%
  filter(nrow(ts) == 24) %>% 
  
  # migrate longitude and latitude to the long form for making the glyph map
  stretch() %>%
  migrate(latitude, longitude)

gly <- glyphs(tmax, "longitude", "month", "latitude", "tmax",
              height = 0.6, width = 1)

nsw_vic <- ozmaps::abs_ste %>% filter(NAME %in% c("Victoria", "New South Wales"))

ggplot() + 
  geom_sf(data = nsw_vic, fill = "transparent") + 
  geom_path(data = gly, 
            aes(gx, gy, group = interaction(id, group), color = group)) +
  scale_color_brewer(palette = "Dark2") + 
  theme_bw() + 
  coord_sf(xlim = c(140, 155)) + 
  theme(legend.position = "bottom")

ggsave(filename = "figures/basic-manip.png", width = 10, height = 10)