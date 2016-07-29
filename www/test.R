setwd("~/Database")
library(dplyr)
source("functions.R")

db <- src_postgres(dbname = "antom", host = "biodb.uef.fi", user = "antom", password = "d0189244be")

dsids <- get_dsids(db_conn,"","BIVARIATE")
assids <- numeric()

input <- list(ds_labels = "", ds_tags = "BIVARIATE", var_labels = "", p_limit = 0.01)

dataset_ids <- numeric()
if (input$ds_labels != "" | input$ds_tags != ""){
  dataset_ids <- get_dsids(db_conn,input$ds_labels,input$ds_tags)
}

association_ids <- numeric()
if (input$var_labels != ""){
  association_ids <- get_associds_by_var(var_labels)
}

assocs <- get_associations(db_conn,dataset_ids,association_ids,input$p_limit)
View(assocs)
