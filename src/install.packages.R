# Install devtools package to be able to install specific versions of packages
install.packages("devtools")

# List packages and specify versions
packages <- list("shiny" = "1.0.3",
              "plotly" = "4.6.0",
              "DT"  = "0.2",
              "DBI" = "0.7",
              "tidyr" = "0.6.3",
              "RPostgreSQL" = "0.6-2",
              "dbplyr" = "1.1.0")

# Install packages
for(package.name in names(packages)){
  print(paste("Installing package:", package.name, "version:", packages[[package.name]]))
  devtools::install_version(package.name, version = packages[[package.name]])
}

# Dev versions of certain packages are required for Ninni to work
devtools::install_github("rstudio/pool")
devtools::install_github("hadley/ggplot2")
devtools::install_github("tidyverse/dplyr")
