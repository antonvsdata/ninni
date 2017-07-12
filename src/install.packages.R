packages <- c("shiny",
              "DT",
              "DBI",
              "dplyr",
              "tidyr",
              "RPostgreSQL",
              "ggplot2",
              "plotly",
              "reshape2",
              "dbplyr")

install.packages(packages)

devtools::install_github("rstudio/pool")