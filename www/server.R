shinyServer(function(input,output){
  
  ds_df <- get_datasets(src_pool(pool)) %>%
    select(label,description,varnum,effect_type,rowcount) %>%
    rename(Label = label, Description = description, Number_of_variables = varnum,
           Effect_type = effect_type, Number_of_associations = rowcount)
  
  ds_labels <- reactive({
    ds_df$Label
  })
  
  output$ds_choice <- renderUI({
    selectizeInput("ds_label",label = "Dataset", width = "100%",
                   choices = ds_labels(), options = list(maxItems = 1,
                                                         placeholder = 'Choose a dataset',
                                                         onInitialize = I('function() { this.setValue(""); }')))
  })
  
  
  associations_list <- eventReactive(input$submit_main,{
    
    db_conn <- src_pool(pool)
    
    if (input$ds_label != "None selected"){
      associations_list <- get_associations_by_ds(db_conn,input$ds_label)
    }
    else{
      return (NULL)
    }
    
    if (input$n_limit != ""){
      associations_list$associations_tbl <- associations_list$associations_tbl %>%
        filter(n > as.numeric(input$n_limit))
    }
    
    if(input$p_limit != ""){
      associations_list$associations_tbl <- associations_list$associations_tbl %>%
        filter(p < as.numeric(input$p_limit))
    }
    
    if (input$p_fdr_limit != ""){
      associations_list$associations_tbl <- associations_list$associations_tbl %>%
        filter(p_fdr < as.numeric(input$p_fdr_limit))
    }
    
    associations_list$associations_tbl <- join_variables(db_conn,associations_list$associations_tbl,associations_list$varnum)
    associations_list$associations_tbl <- make_pretty(associations_list$associations_tbl,associations_list$varnum)
    
    if (input$var_labels != ""){
      associations_list$associations_tbl <- filter_vars(associations_list$associations_tbl,input$var_labels,associations_list$varnum)
    }
    
    return (associations_list)
    
  })
  
  output$dstable <- DT::renderDataTable({
    datatable(ds_df, selection = "none")
  })
  
  pretty_num <- function(x){
    prettyNum(x, format = f)
  }
  
  output$tabular <- DT::renderDataTable({
    datatable(associations_list()$associations_tbl, selection = "none")
  })
  
  output$heatmap <- renderUI({
    if (associations_list()$varnum == 2){
      if (dim(associations_list()$associations_tbl)[1] == 4942){
        plotlyOutput("heatmaply", height = "800")
      }
      else{
        plotOutput("heatmap_stat", height = "800")
      }
    }
    else{
      h5("Heat map requires a dataset with 2 variables.")
    }
  })
  
  output$heatmaply <- renderPlotly({
    get_heatmaply(associations_list()$associations_tbl)
  })
  
  output$heatmap_stat <- renderPlot({
    static_heatmap(associations_list()$associations_tbl,associations_list()$effect_type)
  })
  
  output$volcano <- renderUI({
    if (dim(associations_list()$associations_tbl)[1] > 10000){
      tagList(h5("Interactivity is disabled for large datasets. Please filter the search results."),
              plotOutput("volcano_stat", height = "700"))
    }
    else{
      plotlyOutput("volcanoly", height = "700")
    }
  })
  
  output$volcano_stat <- renderPlot({
    if (input$double_filter){
      volcano_static(associations_list()$associations_tbl,associations_list()$effect_type,associations_list()$varnum,input$double_filter,
                     as.numeric(input$df_p_lim),as.numeric(input$df_effect_lim))
    }
    else{
      volcano_static(associations_list()$associations_tbl,associations_list()$effect_type,associations_list()$varnum,input$double_filter)
    }
  })
  
  output$volcanoly <- renderPlotly({
    if (input$double_filter){
      make_volcanoplotly(associations_list()$associations_tbl,associations_list()$effect_type,associations_list()$varnum,input$double_filter,
                         as.numeric(input$df_p_lim),as.numeric(input$df_effect_lim))
    }
    else{
      make_volcanoplotly(associations_list()$associations_tbl,associations_list()$effect_type,associations_list()$varnum,input$double_filter)
    }
  })
  
  output$woop <- renderText({
    t <- input$qq_choice
    t
  })
  
#   output$qq_plot <-renderUI({
#     if (input$qq_choice == "p-values"){
#     plotOutput("qqplot_ps", height = "700")
#     }
#     if (input$qq_choice == "norm"){
#     plotOutput("qqplot_norm", height = "700")
#     }
#   })
  
  output$qq <- renderUI({
    if (dim(associations_list()$associations_tbl)[1] > 10000){
      tagList(h5("Interactivity is disabled for large datasets. Please filter the search results."),
              plotOutput("qq_plot", height = "700"))
    }
    else{
      plotlyOutput("qq_plotly", height = "700")
    }
  })
  
  output$qq_plotly <- renderPlotly({
    qq_normal(associations_list()$associations_tbl, associations_list()$effect_type,
                associations_list()$varnum)
  })
  
  output$qq_plot <- renderPlot({
    qq_normal(associations_list()$associations_tbl, associations_list()$effect_type,
              associations_list()$varnum, interactive = FALSE)
  })
  
  output$qq_ps <- renderUI({
    if (dim(associations_list()$associations_tbl)[1] > 10000){
      tagList(h5("Interactivity is disabled for large datasets. Please filter the search results."),
              plotOutput("qq_plot_ps", height = "700"))
    }
    else{
      plotlyOutput("qq_plotly_ps", height = "700")
    }
  })
  
  output$qq_plotly_ps <- renderPlotly({
    qq_pvalues(associations_list()$associations_tbl,associations_list()$varnum)
  })
  
  output$qq_plot_ps <- renderPlot({
    qq_pvalues(associations_list()$associations_tbl,associations_list()$varnum, interactive = FALSE)
  })
  
})