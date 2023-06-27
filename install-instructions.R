# An R version of 4.2 is needed to run the code and it can be downloaded from https://cran.r-project.org/

# The following R packages, and their dependencies, are needed to run the reproducible code:  

packages <- c(
	"cubble", "dplyr", "stringr", "ggplot2", "tibble", "knitr", 
	"patchwork", "tsibble", "here", "lubridate", "ozmaps", 
	"ggthemes", "ggrepel", "ncdf4", "sf", "rnaturalearth", 
	"rnaturalearthdata", "colorspace", "units"
)

# They can be installed via:

install.packages(packages, repos = "https://cloud.r-project.org/")

# A cubble version >=0.3.0 is required for reproducing the paper and a dplyr version >= 1.0.8 is required for the cubble package.
