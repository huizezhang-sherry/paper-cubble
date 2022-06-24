## ----eval = FALSE-------------------------------------------------------------
## library(tidyverse)
## #https://www.coronavirus.vic.gov.au/victorian-coronavirus-covid-19-data
## raw <- read_csv(here::here("data/NCOV_COVID_Cases_by_LGA_Source_20220324.csv"),
##                   col_names = c("date", "postcode", "source", "lga"), skip = 1)
## 
## covid <- raw %>%
##   count(date, lga, source, postcode) %>%
##   filter(date >= as.Date("2022-01-01")) %>%
##   filter(source == "Contact with a confirmed case") %>%
##   group_by(date, lga, source) %>%
##   count() %>%
##   ungroup(date) %>%
##   mutate(roll_mean = zoo::rollmean(n, k = 14, fill = NA)) %>%
##   tsibble::as_tsibble(key = lga, index = date) %>%
##   tsibble::fill_gaps(.full = TRUE)
## save(covid, file = here::here("data/covid.rda"))


## -----------------------------------------------------------------------------
load(here::here("data/covid.rda"))

lga <- strayr::read_absmap("lga2018") %>%
  rename(lga = lga_name_2018) %>%
  filter(state_name_2016 == "Victoria") 


## ----echo = TRUE--------------------------------------------------------------
covid %>% head(5)


## ----echo = TRUE--------------------------------------------------------------
lga %>% head(5)


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
lga <- lga %>%
  mutate(lga = ifelse(lga == "Kingston (C) (Vic.)", "Kingston (C)", lga),
         lga = ifelse(lga == "Latrobe (C) (Vic.)", "Latrobe (C)", lga)) %>%
  filter(!lga %in% pair$others$spatial)

covid <- covid %>% filter(!lga %in% pair$others$temporal)

cb <- as_cubble(data = list(spatial = lga, temporal = covid),
                key = lga, index = date, coords = c(cent_long, cent_lat))
