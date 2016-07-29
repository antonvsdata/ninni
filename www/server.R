shinyServer(function(input,output){
  
  
  associations_list <- eventReactive(input$submit_main,{
    
    db_conn = src_pool(pool)
    
    if (input$ds_labels != "" | input$ds_tags != ""){
      associations_list <- get_associations_by_ds(db_conn,input$ds_labels,input$ds_tags)
    }
    else{
      return (NULL)
    }
    
    if (input$n_limit != ""){
      associations_list$associations_tbl <- associations_list$associations_tb %>%
        filter(n > as.numeric(input$n_limit))
    }
    
    if(input$p_limit != ""){
      associations_list$associations_tbl <- associations_list$associations_tb %>%
        filter(p < as.numeric(input$p_limit))
    }
    
    if (input$p_fdr_limit != ""){
      associations_list$associations_tbl <- associations_list$associations_tb %>%
        filter(p_fdr < as.numeric(input$p_fdr_limit))
    }
    
    if (input$var_labels != ""){
      associations_list$associations_tbl <- filter_by_var(db_conn,assocs_tbl,input$var_labels)
    }
    else{
      associations_list$associations_tbl <- join_variables(db_conn,associations_list$associations_tbl,associations_list$varnum)
    }
    
    return (associations_list)
    
  })
  
  output$tabular <- renderTable({
    datbl <- associations_list()$associations_tbl
    disp <- switch(input$table_choice,
           "top" = head(datbl,10),
           "bot" = tail(datbl,10),
           "rnd" = sample_n(datbl,10,replace = F))
    return (disp)
    
  })
  
  output$volcano <- renderPlot({
    datbl <- associations_list()$associations_tbl
    make_volcanoplot(datbl,associations_list()$effect_type)
  })
  
  output$heatmap <- renderPlot({
    if (associations_list()$varnum ==2){
      datbl <- associations_list()$associations_tbl
      make_heatmap(datbl,associations_list()$effect_type)
    }
  })
  
  output$qqplot <- renderPlot({
    datbl <- associations_list()$associations_tbl
    x <- datbl$effect %>% log2()
    gg_qq(x)
  })
  
})