# An R version of 4.2 is needed to run the code and it can be downloaded from https://cran.r-project.org/

# The following R packages, and their dependencies, are needed to run the reproducible code:  

packages <- c(
  "dplyr", "ggplot2", "tibble", "patchwork", "tsibble", "lubridate", "ggthemes", 
  "ggrepel", "ncdf4", "colorspace", "units", "here", "sf", "cubble", 
  "rnaturalearth", "rnaturalearthdata", "ozmaps"
)

# They can be installed via:

install.packages(packages, repos = "https://cloud.r-project.org/")
# A cubble version >=0.3.0 is required for reproducing the paper and a dplyr version >= 1.0.8 is required for the cubble package.
