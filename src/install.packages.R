packages <- c("shiny",
              "DT",
              "DBI",
              "tidyr",
              "RPostgreSQL",
              "dbplyr")

install.packages(packages)

# Specified versions of certain packages are required for Ninni to work
devtools::install_github("rstudio/pool")
devtools::install_github('hadley/ggplot2')
devtools::install_github("tidyverse/dplyr")
devtools::install_version("plotly",version="4.6.0")