shinyServer(function(input,output){
  
  ds_df <- get_datasets(src_pool(pool)) %>%
    select(label,description,varnum,effect_type,rowcount)
  
  
  associations_list <- eventReactive(input$submit_main,{
    
    db_conn <- src_pool(pool)
    
    if (input$ds_label != "None selected"){
      associations_list <- get_associations_by_ds(db_conn,input$ds_label)
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
  
  output$dstable <- DT::renderDataTable({
    
    datatable(ds_df, selection = "single")
  })
  
  output$tabular <- DT::renderDataTable({
    associations_list()$associations_tbl
  })
  
  ds_label_sel <- reactive({
    if (length(input$dstable_rows_selected) > 0){
      ds_label <- ds_df[input$dstable_rows_selected,]$label
    }
    else{
      ds_label <- "None selected"
    }
  })
  
  output$ds_choice <- renderUI({
    textInput("ds_label",label = "Dataset", value = ds_label_sel())
  })
  
  output$volcano <- renderPlotly({
    if (input$double_filter){
      make_volcanoplot(associations_list()$associations_tbl,associations_list()$effect_type,associations_list()$varnum,input$double_filter,
                       as.numeric(input$df_p_lim),as.numeric(input$df_effect_lim))
    }
    else{
      make_volcanoplot(associations_list()$associations_tbl,associations_list()$effect_type,associations_list()$varnum,input$double_filter)
    }
  })
  
  output$heatmap <- renderPlotly({
    make_heatmap(associations_list()$associations_tbl,associations_list()$effect_type)
  })
      
  
  output$qqplot <- renderPlotly({
    gg_qq(associations_list()$associations_tbl, associations_list()$effect_type, associations_list()$varnum)
  })
  
})