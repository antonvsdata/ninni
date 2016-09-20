shinyServer(function(input,output){
  
  ds_dframe <- get_datasets(src_pool(pool)) %>%
    select(label,description,varnum,effect_type,rowcount) %>%
    rename(Label = label, Description = description, Number_of_variables = varnum,
           Effect_type = effect_type, Number_of_associations = rowcount)
  
  ds_labels <- reactive({
    ds_dframe$Label
  })
  
  output$ds_choice <- renderUI({
    selectizeInput("ds_label",label = NULL, width = "100%",
                   choices = ds_labels(), options = list(maxItems = 1,
                                                         placeholder = 'Choose a dataset',
                                                         onInitialize = I('function() { this.setValue(""); }')))
  })
  
  output$ds_info <- renderUI({
    str1 <- paste("Number of associations:",nrow(associations_list()$dframe))
    if (associations_list()$varnum == 1)
      str2 <- paste("Number of unique variables:", associations_list()$dframe$Variable %>%
                      unique() %>% length() )
    if (associations_list()$varnum ==2)
      str2 <- paste("Number of unique variables:", c(associations_list()$dframe$Variable1,associations_list()$dframeVariable2) %>%
                      unique() %>% length() )
    str3 <- paste("P-value < 0.05", associations_list()$dframe %>% filter(P < 0.05) %>% nrow(),sep = "&nbsp;")
    str4 <- paste("P-value (FDR) < 0.05", associations_list()$dframe %>% filter(P_FDR < 0.05) %>% nrow())
    str5 <- paste("P-value range:", min(associations_list()$dframe$P), "...",max(associations_list()$dframe$P))
    str5 <- paste("Effect range:", min(associations_list()$dframe$Effect), "...",max(associations_list()$dframe$Effect))
    HTML(paste(str1, str2, str3, str4, str5, sep = '<br/>'))
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
      associations_list$dframe <- associations_list$dframe %>%
        filter(n > as.numeric(input$n_limit))
    }
    
    if(input$p_limit != ""){
      associations_list$dframe <- associations_list$dframe %>%
        filter(p < as.numeric(input$p_limit))
    }
    
    if (input$p_fdr_limit != ""){
      associations_list$dframe <- associations_list$dframe %>%
        filter(p_fdr < as.numeric(input$p_fdr_limit))
    }
    
    associations_list$dframe <- join_variables(db_conn,associations_list$dframe,associations_list$varnum)
    associations_list$dframe <- make_pretty(associations_list$dframe,associations_list$varnum)
    
    if (input$var_labels != ""){
      associations_list$dframe <- filter_vars(associations_list$dframe,input$var_labels,associations_list$varnum)
    }
    
    return (associations_list)
    
  })
  
  output$dstable <- DT::renderDataTable({
    datatable(ds_dframe, selection = "none")
  })
  
  pretty_num <- function(x){
    prettyNum(x, format = f)
  }
  
  output$tabular <- DT::renderDataTable({
    datatable(associations_list()$dframe, selection = "none")
  })
  
  output$heatmap <- renderUI({
    if (associations_list()$varnum == 2){
      if (dim(associations_list()$dframe)[1] == 4942){
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
    get_heatmaply(associations_list()$dframe)
  })
  
  output$heatmap_stat <- renderPlot({
    static_heatmap(associations_list()$dframe,associations_list()$effect_type)
  })
  
  output$volcano <- renderUI({
    if (dim(associations_list()$dframe)[1] > 10000){
      tagList(h5("Interactivity is disabled for large datasets. Please filter the search results."),
              plotOutput("volcano_stat", height = "700"))
    }
    else{
      plotlyOutput("volcanoly", height = "700")
    }
  })
  
  output$volcano_stat <- renderPlot({
    if (input$double_filter){
      volcano_static(associations_list()$dframe,associations_list()$effect_type,associations_list()$varnum,input$double_filter,
                     as.numeric(input$df_p_lim),as.numeric(input$df_effect_lim))
    }
    else{
      volcano_static(associations_list()$dframe,associations_list()$effect_type,associations_list()$varnum,input$double_filter)
    }
  })
  
  output$volcanoly <- renderPlotly({
    if (input$double_filter){
      make_volcanoplotly(associations_list()$dframe,associations_list()$effect_type,associations_list()$varnum,input$double_filter,
                         as.numeric(input$df_p_lim),as.numeric(input$df_effect_lim))
    }
    else{
      make_volcanoplotly(associations_list()$dframe,associations_list()$effect_type,associations_list()$varnum,input$double_filter)
    }
  })
  
  output$qq_plot <-renderUI({
    if (input$qq_choice == "p-values"){
      if (dim(associations_list()$dframe)[1] > 10000){
        t <- tagList(h5("Plotting static figure."),
                plotOutput("qq_plot_ps", height = "700"))
      }
      else{
        t <- plotlyOutput("qq_plotly_ps", height = "700")
      }
    }
    if (input$qq_choice == "norm"){
      if (dim(associations_list()$dframe)[1] > 10000){
        t <- tagList(h5("Plotting static figure."),
                plotOutput("qq_plot_norm", height = "700"))
      }
      else{
        t <- plotlyOutput("qq_plotly_norm", height = "700")
      }
    }
    t
  })
  
  output$qq_plotly_norm <- renderPlotly({
    qq_normal(associations_list()$dframe, associations_list()$effect_type,
                associations_list()$varnum)
  })
  
  output$qq_plot_norm <- renderPlot({
    qq_normal(associations_list()$dframe, associations_list()$effect_type,
              associations_list()$varnum, interactive = FALSE)
  })
  
  output$qq_plotly_ps <- renderPlotly({
    qq_pvalues(associations_list()$dframe,associations_list()$varnum)
  })
  
  output$qq_plot_ps <- renderPlot({
    qq_pvalues(associations_list()$dframe,associations_list()$varnum, interactive = FALSE)
  })
  
})