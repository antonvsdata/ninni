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

if(!exists("pool")){
  pool <- dbPool(
    drv = RSQLite::SQLite(),
    dbname = "www/ninni.db"
    #maxSize = 10,
    #idleTimeout = 40000
  )
}

