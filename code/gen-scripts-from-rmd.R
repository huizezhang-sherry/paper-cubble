knitr::purl(input = here::here("jss/main.Rmd"), output = here::here("code/reproducible-script.R"))
knitr::purl(input = here::here("jss/appendix.Rmd"), output = here::here("code/appendix.R"))
# manually merge the two scripts into one
# remove chunks for illustration, but keep codes for deriving data
# remove "child = here::here("5-1-covid.Rmd")" and alike
# change in line 5 to echo = TRUE to show all the codes, also line 446, 467, 509
knitr::spin(here::here("code/reproducible-script.R"), knit = FALSE)
