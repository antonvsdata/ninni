library(shiny)
library(DT)
library(DBI)
library(dplyr)
library(tidyr)
library(RPostgreSQL)
library(pool)
library(ggplot2)
library(plotly)
library(UpSetR)
library(purrr)
library(shinyFeedback)
library(ggridges)
library(igraph)
library(ggraph)
library(ggrepel)
library(networkD3)
library(bcrypt)

source("R/queries.R")

db_info <- read_db_info("www/database_www.config")

if(!exists("pool")){
  pool <- dbPool(
    drv = RPostgres::Postgres(),
    dbname = db_info$db_name,
    host = db_info$db_host,
    port = db_info$db_port,
    user = db_info$db_user,
    password = db_info$db_password
    #maxSize = 10,
    #idleTimeout = 40000
  )
}

