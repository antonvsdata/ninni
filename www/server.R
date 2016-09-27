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
    tableOutput("ds_info_table")
  })
  
  output$ds_info_table <- renderTable({
    str <- c("Number of associations:","Number of unique variables:","P-value < 0.05","P-value (FDR) < 0.05",
                 "P-value range:","Effect range:")
    values <- nrow(associations_list()$dframe) %>% as.character()
    if (associations_list()$varnum == 1)
      values <- c(values, associations_list()$dframe$Variable %>%
                      unique() %>% length() %>% as.character() )
    if (associations_list()$varnum ==2)
      values <- c(values, c(associations_list()$dframe$Variable1,associations_list()$dframeVariable2) %>%
                      unique() %>% length() %>% as.character() )
    values <- c(values,associations_list()$dframe %>% filter(P < 0.05) %>% nrow() %>% as.character(),
                associations_list()$dframe %>% filter(P_FDR < 0.05) %>% nrow() %>% as.character(),
                paste((associations_list()$dframe$P)%>%min()%>%signif(digits=3), "...",
                      (associations_list()$dframe$P)%>%max()%>%signif(digits=3)),
                paste((associations_list()$dframe$Effect)%>%min()%>%signif(digits=3), "...",
                      (associations_list()$dframe$Effect)%>%max()%>%signif(digits=3)))
    data.frame(str,values)
},include.rownames=FALSE, include.colnames = FALSE)
  
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