library(shiny)
library(DT)
library(DBI)
library(dplyr)
library(tidyr)
library(RPostgreSQL)
library(pool)
library(ggplot2)
library(plotly)
library(reshape2)

source("visualizations.R")
source("queries.R")

pool <- dbPool(
  drv = PostgreSQL(),
  dbname = "antom",
  host = "biodb.uef.fi",
  user = "antom",
  password = "d0189244be"
)