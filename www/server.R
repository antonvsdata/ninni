shinyServer(function(input,output){
  
  
  associations_list <- eventReactive(input$submit_main,{
    
    db_conn = src_pool(pool)
    
    if (input$ds_labels != "" | input$ds_tags != ""){
      associations_list <- get_associations_by_ds(db_conn,input$ds_labels,input$ds_tags)
    }
    else{
      return (NULL)
    }
    
    # If non compatible datasets are found
    if (associations_list$varnum == -1){
      return(associations_list)
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
  
  output$dstable <- renderDataTable({
    associations_list()$ds_tbl
  })
  
  output$tabular <- renderDataTable({
    associations_list()$associations_tbl
    
  })
  
  output$volcano <- renderPlotly({
    if (input$df){
      make_volcanoplot(associations_list()$associations_tbl,associations_list()$effect_type,associations_list()$varnum,input$df,
                       as.numeric(input$df_p_lim),as.numeric(input$df_effect_lim))
    }
    else{
      make_volcanoplot(associations_list()$associations_tbl,associations_list()$effect_type,associations_list()$varnum,input$df)
    }
    
  })
  
  output$heatmap <- renderPlot({
    if (associations_list()$varnum ==2){
      make_heatmap(associations_list()$associations_tbl,associations_list()$effect_type)
    }
  })
  
  output$qqplot <- renderPlot({
    x <- associations_list()$associations_tbl$effect %>% log2()
    gg_qq(x)
  })
  
})