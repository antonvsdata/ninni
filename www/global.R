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

db_info <- read_db_info("/home/users/antom/Projects/ninni/src/database_import.config")

if(!exists("pool")){
  pool <- dbPool(
    drv = RPostgreSQL::PostgreSQL(),
    dbname = db_info$db_name,
    host = db_info$db_host,
    port = db_info$db_port,
    user = db_info$db_user,
    password = db_info$db_password
    #maxSize = 10,
    #idleTimeout = 40000
  )
}


ds_dframe <- get_datasets(pool) %>%
  select(label,description,varnum,effect_type,rowcount) %>%
  rename(Label = label, Description = description, Number_of_variables = varnum,
         Effect_type = effect_type, Number_of_associations = rowcount)