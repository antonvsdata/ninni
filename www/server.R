shinyServer(function(input,output){
  
  
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
  
  # Handles the query to the database
  # Reactive expressions cache their value, so filtering the same dataset multiple times
  # does not provoke a new database query
  
  associations_list_query <- reactive({
    if(is.null(input$ds_label)){
      return(NULL)
    }
    if (input$ds_label == ""){
      return (NULL)
    }
    withProgress(message = "Retrieving dataset from database",{
      associations_list <- get_associations_by_ds(pool,input$ds_label)
      incProgress(0.3)
      associations_list$dframe <- join_variables(pool,associations_list$dframe,associations_list$varnum)
      associations_list$dframe <- make_pretty(associations_list$dframe,associations_list$varnum)
    })
    
    return (associations_list)
  })
  
  associations_list_db <- eventReactive(input$submit,{
    associations_list_query()
  })
  
  output$filters <- renderUI({
    if(is.null(standard_filters())){
      return(NULL)
    }
    tagList(
      standard_filters(),
      extra_filters(),
      variable_filters(),
      actionButton("submit",
                   label = "Filter")
    )
  })
  
  standard_filters <- reactive({
    if(is.null(associations_list_query())){
      return(NULL)
    }
    tagList(
      strong("Variable"),
      textInput("var_labels","Keywords, comma separated"),
      
      fluidRow(
        column(6,
               textInput("p_limit",label = "P-value <")),
        column(4,
               radioButtons("p_limit_fdr",label = NULL,
                            choices = c("Unadjusted" = FALSE,
                                        "FDR" = TRUE),
                            selected = FALSE))),
      fluidRow(
        column(7,
               textInput("n_limit",
                         label = "Minimum n"))
      ),
      strong("Effect:"),
      fluidRow(
        column(5,
               textInput("eff_min",label="min")
        ),
        column(5,
               textInput("eff_max", label = "max"))),
      strong("Description"),
      textInput("description_labels","Keywords, comma separated")
    )
  })
  
  # Generate filters for metavariables
  extra_filters <- reactive({
    if(is.null(associations_list_query())){
      return(NULL)
    }
    if(associations_list_query()$varnum == 1){
      col_limit <- 8
    }
    else{ #varnum == 2
      col_limit <- 10
    }
    dframe <- associations_list_query()$dframe %>% as.data.frame()
    if(ncol(dframe) == col_limit){
      return(NULL)
    }
    out <- tagList()
    for(i in (col_limit+1):ncol(dframe)){
      if(class(dframe[,i]) == "numeric"){
        out <- tagList(out,
                       strong(colnames(dframe)[i]),
                       fluidRow(
                         column(5,
                                textInput(paste(colnames(dframe)[i],"min",sep="_"),label="min")
                         ),
                         column(5,
                                textInput(paste(colnames(dframe)[i],"max",sep="_"), label = "max")
                         )
                       ))
        
      }
      if(class(dframe[,i]) == "character"){
        out <- tagList(out,
                       strong(colnames(dframe)[i]),
                       textInput(paste(colnames(dframe)[i],"label",sep="_"),label = "Keywords, comma separated"))
      }
    }
    out
  })
  
  variable_filters <- reactive({
    if(is.null(associations_list_query())){
      return(NULL)
    }
    tagList(
      checkboxInput("var_filters","Variable filters"),
      conditionalPanel("input.var_filters == true",
                       h4("Variable filters"),
                       h5("At least one association with"),
                       
                       fluidRow(
                         column(6,
                                textInput("var_p_limit",label = "P-value <")),
                         column(4,
                                radioButtons("var_p_limit_fdr",label = NULL,
                                             choices = c("Unadjusted" = FALSE,
                                                         "FDR" = TRUE),
                                             selected = FALSE))
                         
                       ),
                       strong("Effect size:"),
                       fluidRow(
                         column(5,
                                textInput("var_eff_min",label="min")
                         ),
                         column(5,
                                textInput("var_eff_max", label = "max"))
                       )
      )
    )
  })
  
    # Returns a list with following objects:
  # - dframe: a data frame with the associations
  # - varnum: the number of variables in the dataset
  # - effect_type
  
  # Filter the associations dataframe
  associations_list <- eventReactive(input$submit,{
    
    associations_list <- associations_list_db()
    dframe <- as.data.frame(associations_list$dframe)
    #Variable filters:
    
    # P-value <
    if (input$var_filters & input$var_p_limit != ""){
      dframe <- dframe %>%
        varfilter_p( as.numeric(input$var_p_limit),associations_list$varnum,input$var_p_limit_fdr)
    }

    # Effect: min max
    if (input$var_filters & (input$var_eff_min != "" | input$var_eff_max != "")){
      if (input$var_eff_min == ""){
        dframe <- varfilter_eff(dframe, eff_max = as.numeric(input$var_eff_max), varnum = associations_list$varnum)
      }
      else if (input$var_eff_max == ""){
        dframe <- varfilter_eff(dframe, eff_min = as.numeric(input$var_eff_min), varnum = associations_list$varnum)
      }
      else{
        dframe <- varfilter_eff(dframe, as.numeric(input$var_eff_min), as.numeric(input$var_eff_max), associations_list$varnum)
      }
    }

    
    # Association filters:
    
    # Variable
    # Keywords, comma separated
    if (input$var_labels != ""){
      dframe <- filter_variable(dframe,input$var_labels,associations_list$varnum)
    }
    
    # Description
    # Keywords, comma separated
    if (input$description_labels != ""){
      dframe <- filter_description(dframe,input$description_labels,associations_list$varnum)
    }
    
    # P-value <
    if(input$p_limit != ""){
      if (input$p_limit_fdr){
        dframe <- dframe %>%
          filter(P_FDR < as.numeric(input$p_limit))
      }
      else{
        dframe <- dframe %>%
          filter(P < as.numeric(input$p_limit))
      }
    }
    # Minimum N
    if (input$n_limit != ""){
      dframe <- dframe %>%
        filter(N >= as.numeric(input$n_limit))
    }
    # Effect size: min max
    if (input$eff_min != "" | input$eff_max != ""){
      if (input$eff_min == ""){
        dframe <- dframe %>%
          filter(Effect < as.numeric(input$eff_max))
      }
      else if (input$eff_max == ""){
        dframe <- dframe %>%
          filter(Effect > as.numeric(input$eff_min))
      }
      else{
        dframe <- dframe %>%
          filter(Effect > as.numeric(input$eff_min) & Effect < as.numeric(input$eff_max))
      }
    }
    
    # Filters for extra metavariables
    if(!is.null(extra_filters())){
      if(associations_list$varnum == 1){
        col_limit <- 8
      }
      else{ #varnum == 2
        col_limit <- 10
      }
      for(i in (col_limit+1):ncol(dframe)){
        if(class(dframe[,i]) == "numeric"){
          inputid <- names(input)[which(names(input) == paste(colnames(dframe)[i],"min",sep="_"))]
          expr <- paste("input",inputid,sep="$")
          limit_min <- eval(parse(text = expr))
          inputid <- names(input)[which(names(input) == paste(colnames(dframe)[i],"max",sep="_"))]
          expr <- paste("input",inputid,sep="$")
          limit_max <- eval(parse(text = expr))
          if (limit_min != "" | limit_max != ""){
            if (limit_min == ""){
              dframe <- dframe[dframe[,i] < as.numeric(limit_max),]
            }
            else if (limit_max == ""){
              dframe <- dframe[dframe[,i] > as.numeric(limit_min),]
            }
            else{
              dframe <- dframe[dframe[,i] < as.numeric(limit_max) & dframe[,i] > as.numeric(limit_min),]
            }
          }
        }
        else if(class(dframe[,i]) == "character"){
          inputid <- names(input)[which(names(input) == paste(colnames(dframe)[i],"label",sep="_"))]
          expr <- paste("input",inputid,sep="$")
          keywords <- eval(parse(text = expr)) %>% split(",")
          if(keywords != ""){
            dframe <- dframe[dframe[,i] %in% keywords,]
          }
        }
      }
    }
    associations_list$dframe <- dframe
    return(associations_list)
  })
  
  # Table showing information of the chosen dataset
  output$ds_info_table <- renderTable({
    if(is.null(associations_list())){
      return(NULL)
    }
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
                paste((associations_list()$dframe$P) %>% min() %>% signif(digits=3), "...",
                      (associations_list()$dframe$P) %>% max() %>% signif(digits=3)),
                paste((associations_list()$dframe$Effect) %>% min() %>% signif(digits=3), "...",
                      (associations_list()$dframe$Effect) %>% max() %>% signif(digits=3)))
    data.frame(str,values)
  },include.rownames=FALSE, include.colnames = FALSE)
  
  # Shows all the datasets in database
  output$dstable <- DT::renderDataTable({
    datatable(ds_dframe, selection = "none")
  })
  
  # Associations data table
  output$tabular <- DT::renderDataTable({
    dframe <- associations_list()$dframe
    for(i in 1:ncol(dframe)){
      if(class(dframe[,i]) == "numeric"){
        dframe[,i] <- signif(dframe[,i],digits = 3)
      }
    }
    datatable(dframe, selection = "none")
  })
  
  output$download <- renderUI({
    if (nrow(associations_list()$dframe) > 0){
      downloadButton("download_button")
    }
  })
  
  output$download_button <- downloadHandler(
    filename = function(){
      paste(input$ds_label,".csv", sep = "")
    },
    
    content = function(file){
      write.csv(associations_list()$dframe,file, quote = F, row.names = F)
    }
  )
  
  # All the visualizations can be interactive plotly figures,
  # or static figures, if dataset has more than 10 000 associations
  
  output$heatmap <- renderUI({
    if (associations_list()$varnum == 2){
      if (nrow(associations_list()$dframe) > 10000){
        tagList(h5("Wow, your data is BIG! Plotting static figure."),
                plotOutput("heatmap_static", height = "800"))
      }
      else{
        plotlyOutput("heatmaply", height = "800")
      }
    }
    else{
      h5("Heat map requires a dataset with 2 variables.")
    }
  })
  
  output$heatmaply <- renderPlotly({
    get_heatmap_lowertri(associations_list()$dframe, associations_list()$effect_type,input$clustering,interactive = TRUE)
  })
  
  output$heatmap_static <- renderPlot({
    get_heatmap_lowertri(associations_list()$dframe, associations_list()$effect_type,input$clustering,interactive = FALSE)
  })
  
  output$volcano <- renderUI({
    if (nrow(associations_list()$dframe) > 10000){
      tagList(h5("Wow, your data is BIG! Plotting static figure."),
              plotOutput("volcano_static", height = "700"))
    }
    else{
      #plotOutput("volcano_static", height = "700")
      plotlyOutput("volcanoly", height = "700px")
    }
  })
  
  output$volcano_static <- renderPlot({
    if (input$double_filter){
      if (input$df_eff_limit_log2){
        eff_lim <- as.numeric(input$df_effect_limit)
      }
      else{
        eff_lim <- log2(as.numeric(input$df_effect_limit))
      }
      volcano_static(associations_list()$dframe,associations_list()$effect_type,associations_list()$varnum,input$double_filter,
                     as.numeric(input$df_p_limit), input$df_p_limit_fdr, eff_lim)
    }
    else{
      volcano_static(associations_list()$dframe,associations_list()$effect_type, associations_list()$varnum,input$double_filter)
    }
  })
  
  output$volcanoly <- renderPlotly({
    if (input$double_filter){
      if (input$df_eff_limit_log2){
        eff_lim <- as.numeric(input$df_effect_limit)
      }
      else{
        eff_lim <- log2(as.numeric(input$df_effect_limit))
      }
      make_volcanoplotly(associations_list()$dframe,associations_list()$effect_type,associations_list()$varnum,input$double_filter,
                         as.numeric(input$df_p_limit),input$df_p_limit_fdr, eff_lim)
    }
    else{
      make_volcanoplotly(associations_list()$dframe,associations_list()$effect_type,associations_list()$varnum,input$double_filter)
    }
  })
  
  output$qq_plot <-renderUI({
    if (input$qq_choice == "p-values"){
      if (nrow(associations_list()$dframe) > 10000){
        t <- tagList(h5("Wow, your data is BIG! Plotting static figure."),
                     plotOutput("qq_plot_static_ps", height = "700"))
      }
      else{
        t <- plotlyOutput("qq_plotly_ps", height = "700")
      }
    }
    if (input$qq_choice == "norm"){
      if (nrow(associations_list()$dframe) > 10000){
        t <- tagList(h5("Wow, your data is BIG! Plotting static figure."),
                     plotOutput("qq_plot_static_norm", height = "700"))
      }
      else{
        t <- plotlyOutput("qq_plotly_norm", height = "700")
      }
    }
    t
  })
  
  output$qq_plot_static_ps <- renderPlot({
    qq_pvalues(associations_list()$dframe,associations_list()$varnum, interactive = FALSE)
  })
  
  output$qq_plotly_ps <- renderPlotly({
    qq_pvalues(associations_list()$dframe,associations_list()$varnum)
  })
  
  output$qq_plot_static_norm <- renderPlot({
    qq_normal(associations_list()$dframe, associations_list()$effect_type,
              associations_list()$varnum, interactive = FALSE)
  })
  
  output$qq_plotly_norm <- renderPlotly({
    qq_normal(associations_list()$dframe, associations_list()$effect_type,
              associations_list()$varnum)
  })
  
  output$lady_manhattan_plot_choices <- renderUI({
    tagList(
      checkboxInput("lady_coloring","Coloring according to column"),
      conditionalPanel("input.lady_coloring == true",
                       radioButtons("lady_coloring_type",NULL,
                                    choices = c("Continuous", "Discrete")),
                       selectizeInput("lady_coloring_column","Column",
                                      choices = colnames(associations_list()$dframe),
                                      options = list(maxItems = 1,
                                                     placeholder = 'Choose a column',
                                                     onInitialize = I('function() { this.setValue(""); }'))))
    )
  })
  
  output$lady_manhattan_plot <-renderUI({
    
    if (nrow(associations_list()$dframe) > 10000){
      t <- tagList(h5("Wow, your data is BIG! Plotting static figure."),
                   plotOutput("lady_manhattan_plot_static", height = "700"))
    }
    else{
      t <- plotlyOutput("lady_manhattan_plotly", height = "700")
    }
    
    t
  })
  
  output$lady_manhattan_plot_static <- renderPlot({
    if(input$lady_coloring & !is.null(input$lady_coloring_column) & input$lady_coloring_column != ""){
      lady_manhattan_plot(associations_list()$dframe,associations_list()$effect_type,associations_list()$varnum, interactive = FALSE, input$lady_coloring_column,input$lady_coloring_type)
    }
    else{
      lady_manhattan_plot(associations_list()$dframe,associations_list()$effect_type,associations_list()$varnum, interactive = FALSE)
    }
  })
  
  output$lady_manhattan_plotly <- renderPlotly({
    if(input$lady_coloring & !is.null(input$lady_coloring_column) & input$lady_coloring_column != ""){
      lady_manhattan_plot(associations_list()$dframe,associations_list()$effect_type,associations_list()$varnum, interactive = TRUE, input$lady_coloring_column,input$lady_coloring_type)
    }
    else{
      lady_manhattan_plot(associations_list()$dframe,associations_list()$effect_type,associations_list()$varnum, interactive = TRUE)
    }
  })
  
})