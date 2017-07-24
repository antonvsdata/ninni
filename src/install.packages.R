packages <- c("shiny",
              "DT",
              "DBI",
              "dplyr",
              "tidyr",
              "RPostgreSQL",
              "dbplyr")

install.packages(packages)

devtools::install_github("rstudio/pool")
devtools::install_github('hadley/ggplot2')
devtools::install_version("plotly",version="4.6.0")