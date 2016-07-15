library(shiny)
library(dplyr)
source("functions.R")

db <- src_postgres(dbname = "antom", host = "biodb.uef.fi", user = "antom", password = "d0189244be")



shinyServer(function(input,output){
  
  mainlist <- eventReactive(input$submit_main,{
    
    ds_labels <- input$ds_label %>% strsplit(split=",") %>% unlist
    ds_tags <- input$ds_tags %>% strsplit(split=",") %>% unlist
    var_labels <- input$var_label  %>% strsplit(split=",") %>% unlist
    p_limit <- input$p_limit
    
    list(ds_labels,ds_tags,var_labels,p_limit)
  })
  
  #reactive producing the limited association table according to mainlist
  assoc_limited <- reactive({
    if (mainlist$ds_label){
      db_filtered <- db_filtered %>% filter()
    }
  })
  
  
  #functions for rendering plots
  
})