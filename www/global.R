library(shiny)
library(dplyr)
library(tidyr)
library(RPostgreSQL)
library(pool)
library(ggplot2)

source("visualizations.R")
source("queries.R")

pool <- dbPool(
  drv = PostgreSQL(),
  dbname = "antom",
  host = "biodb.uef.fi",
  user = "antom",
  password = "d0189244be"
)