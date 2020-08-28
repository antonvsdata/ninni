library(shiny)
library(DT)
library(DBI)
library(dplyr)
library(tidyr)
library(RSQLite)
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
source("R/import_functions.R")

# If database does not exist, initialize an empty database
if (!file.exists("db/ninni.db")) {
  file.create("db/ninni.db")
  init_con <- dbConnect(RSQLite::SQLite(), "db/ninni.db")
  execute_sql_file("www/create_schema_sqlite.sql", init_con)
  dbDisconnect(init_con)
}

if(!exists("pool")){
  pool <- dbPool(
    drv = RSQLite::SQLite(),
    dbname = "db/ninni.db"
    #maxSize = 10,
    #idleTimeout = 40000
  )
}

