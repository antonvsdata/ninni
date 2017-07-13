packages <- c("shiny",
              "DT",
              "DBI",
              "dplyr",
              "tidyr",
              "RPostgreSQL",
              "plotly",
              "reshape2",
              "dbplyr")

install.packages(packages)

devtools::install_github("rstudio/pool")
devtools::install_github('hadley/ggplot2')
