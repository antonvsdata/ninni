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
source("db_info.R")

if(!exists("pool")){
  pool <- dbPool(
    drv = RPostgreSQL::PostgreSQL(),
    dbname = db_name,
    host = db_host,
    user = db_user,
    password = db_password,
    maxSize = 10,
    idleTimeout = 40000
  )
}


ds_dframe <- get_datasets(pool) %>%
  select(label,description,varnum,effect_type,rowcount) %>%
  rename(Label = label, Description = description, Number_of_variables = varnum,
         Effect_type = effect_type, Number_of_associations = rowcount)