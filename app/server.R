
# All elements are ordered by the tabs in Ninni
shinyServer(function(input, output, session){
  
  ds_dframe <- get_datasets(pool)
  
  #----------- Sidebar ----------------
  
  # Multiple choice dropwdown box for choosing datasets
  observeEvent(ds_dframe, {
    updateSelectizeInput(session, "ds_label", choices = ds_dframe$Label)
  })
  
  # Multiple choice dropdown: search database for datasets by metadata tags
  observeEvent(ds_dframe, {
    md_labels <- extract_meta_labels(ds_dframe)
    updateSelectizeInput(session, "metadata_tags", choices = md_labels)
  })
  
  # Handles the query to the database
  # Reactive expressions cache their value, so filtering the same dataset multiple times
  # does not provoke a new database query
  associations_list_query_ <-  reactive({
    if (!length(input$ds_label) & input$var_keywords == "" & !length(input$metadata_tags)){
      return (NULL)
    }
    withProgress(message = "Retrieving dataset from database",{
      asso_list <- get_associations(pool,input$ds_label, input$var_keywords, input$metadata_tags)
      incProgress(0.3)
      asso_list$dframe <- join_variables(pool, asso_list$dframe, asso_list$datasets)
    })
    
    return (asso_list)
  })
  
  associations_list_query <- eventReactive(input$filter,{
    associations_list_query_()
  })
  
  
  
  extra_filters_react <- reactive({
    if(is.null(associations_list_query())){
      return(p("No data"))
    }
    if(associations_list_query()$varnum == 2){
      col_limit <- 12
    }
    else{ # all datasets have varnum == 1
      col_limit <- 10
    }
    dframe <- associations_list_query()$dframe %>% as.data.frame()
    if(ncol(dframe) == col_limit){
      return(p("No extra columns"))
    }
    # If the number of columns exceeds the number of standard columns, there are metavariables
    # Generate filters for these metavariables:
    # min & max if numeric
    # keyword search if character
    extra_cols <- colnames(dframe)[seq(col_limit + 1, ncol(dframe))]
    
    out <- map(extra_cols, ~ column_filter(associations_list_query()$dframe[, .x],
                                           .x,
                                           input = input))
    out
  })
  
  # Generate filters for possible metavariables
  output$extra_filters <- renderUI({
    extra_filters_react()
  })
  
  
  # Filter the associations dataframe
  # Returns a list with following objects:
  # - datasets: table of the datasets
  # - dframe: a data frame with the associations
  # - varnum: the number of variables in the dataset
  # - effect_type
  associations_list <- eventReactive(input$filter,{
    if(is.null(associations_list_query())){
      return(NULL)
    }
    
    asso_list <- associations_list_query()
    dframe <- as.data.frame(asso_list$dframe)
    # List of all filters
    # Combined to one logical in the end
    keeps <- list()
    
    #Variable filters
    # keep variable with at least one association that satisfies constraints
    # P-value <
    if(input$toggle_variable_filters){
      if (input$var_p_limit != ""){
        keeps$var_p <- varfilter_p(input$var_p_limit,
                                  asso_list$varnum, input$var_p_limit_fdr)
      }
      
      # Effect: min max
      if ((input$var_eff_min != "" | input$var_eff_max != "")){
        keeps$var_eff <- varfilter_eff(dframe, eff_min = input$var_eff_min,
                                       eff_max = input$var_eff_max,
                                       varnum = asso_list$varnum)
      }
    }
    
    # Association filters:
    if(input$toggle_standard_filters){
      # Variable
      # Keywords, comma separated
      if (input$var_labels != ""){
        if(asso_list$varnum == 2){
          cols <- c("Variable1", "Variable2")
        }
        else{
          cols <- "Variable1"
        }
        keeps$variables <- filter_by_keyword(dframe, cols, input$var_labels)
      }
      # Description
      # Keywords, comma separated
      if (input$description_labels != ""){
        if(asso_list$varnum == 2){
          cols <- c("Description1", "Description2")
        }
        else{
          cols <- "Description1"
        }
        keeps$description <- filter_by_keyword(dframe, cols, input$description_labels)
      }
      # P-value <
      if(input$p_limit != ""){
        if (input$p_limit_fdr){
          keeps$assoc_p <- dframe$P_FDR < as.numeric(input$p_limit)
        }
        else{
          keeps$assoc_p <- dframe$P < as.numeric(input$p_limit)
        }
      }
      # Minimum N
      if (input$n_limit != ""){
        keeps$assoc_n <- dframe$N >= as.numeric(input$n_limit)
      }
      # Effect size: min max
      if (input$eff_min != "" || input$eff_max != ""){
        keeps$assoc_eff <- filter_min_max(dframe, "Effect",
                                          input$eff_min, input$eff_max)
      }
    }
    
    # Filters for extra variables
    if(input$toggle_extra_filters){
      if(asso_list$varnum == 2){
        col_limit <- 12
      }
      else{ # each varnum == 1
        col_limit <- 10
      }
      if(ncol(dframe) > col_limit){
        extra_cols <- colnames(dframe)[seq(col_limit + 1, ncol(dframe))]
        for(col in extra_cols){
          if(class(dframe[, col]) == "numeric"){
            keeps[[col]] <- filter_min_max(dframe,
                                     col = col,
                                     min = input[[paste0(col, "_min")]],
                                     max = input[[paste0(col, "_max")]])
          }
          else if(class(dframe[, col]) == "character"){
            keywords <- input[[paste0(col, "_label")]]
            if(keywords != ""){
              keeps[[col]] <- filter_by_keyword(dframe, col, keywords)
            }
          }
        }
      }
      
    }
    
    if (length(keeps)) {
      keep_master <- reduce(keeps, `&`)
      asso_list$dframe <- dframe[keep_master, ]
    }
    
    return(asso_list)
  })
  
  # Contains information about the loaded data
  output$ds_info <- renderUI({
    tableOutput("ds_info_table")
  })
  
  # Table showing information of the loaded data
  output$ds_info_table <- renderTable({
    if(is.null(associations_list())){
      return(NULL)
    }
    string <- c("Number of datasets","Effect type(s)", "Number of associations:",
                "Number of unique variables:","P-value < 0.05","P-value (FDR) < 0.05",
             "P-value range:","Effect range:")
    values <- c(nrow(associations_list()$datasets),
                associations_list()$datasets$effect_type %>% unique() %>% paste(collapse=","),
                nrow(associations_list()$dframe))
    if (associations_list()$varnum == 2){
      values <- c(values, c(associations_list()$dframe$Variable1, associations_list()$dframe$Variable2) %>%
                    unique() %>% length())
    }
    else{
      values <- c(values, associations_list()$dframe$Variable1 %>%
                    unique() %>% length())
    }
    values <- c(values, associations_list()$dframe %>% filter(P < 0.05) %>% nrow(),
                associations_list()$dframe %>% filter(P_FDR < 0.05) %>% nrow(),
                paste((associations_list()$dframe$P) %>% min() %>% signif(digits = 3), "...",
                      (associations_list()$dframe$P) %>% max() %>% signif(digits = 3)),
                paste((associations_list()$dframe$Effect) %>% min() %>% signif(digits = 3), "...",
                      (associations_list()$dframe$Effect) %>% max() %>% signif(digits = 3)))
    data.frame(string,values)
  },include.rownames=FALSE, include.colnames = FALSE)
  
  # --------------- Main ------------------
  
  # Shows all the datasets in database
  output$dstable <- DT::renderDataTable({
    datatable(ds_dframe, selection = "none")
  })
  
  # -------------- Data Table --------------
  
  # Associations data table
  output$tabular <- DT::renderDataTable({
    dframe <- associations_list()$dframe
    for(i in seq_len(ncol(dframe))){
      if(class(dframe[, i]) == "numeric"){
        dframe[, i] <- signif(dframe[, i], digits = 3)
      }
    }
    datatable(dframe, selection = "none")
  })
  
  # Download button for association data
  output$download <- renderUI({
    if (nrow(associations_list()$dframe) > 0){
      downloadButton("download_button")
    }
  })
  
  output$download_button <- downloadHandler(
    filename = function(){
      "ninni_data_output.csv"
    },
    
    content = function(file){
      write.csv(associations_list()$dframe, file, row.names = FALSE)
    }
  )
  
  # All the visualizations can be interactive plotly figures,
  # or static figures, if dataset has more than 10 000 associations
  
  plotly_limit <- 10000
  
  # -------------- Heat map --------------------
  
  
  output$heatmap <- renderUI({
    if (associations_list()$effect_type == "Multiple"){
      return(h5("Multiple different effect types can't be plotted together"))
    }
    if (associations_list()$varnum == 1){
      return(h5("Heat map requires associations with 2 variables"))
    }
    
    out <- tagList()
    # Check if there are associations with only one variable
    # They will be removed before plotting the heatmap
    n_not_plotted <- length(which(is.na(associations_list()$dframe$Variable1) |
                                    is.na(associations_list()$dframe$Variable2)))
    if(n_not_plotted > 0){
      n_plotted <- nrow(associations_list()$dframe) - n_not_plotted
      out <- tagList(out, h5(paste0("Only associations with 2 variables will be plotted in the heat map.
                                   Removed ", n_not_plotted," associations, plotted ",
                                   n_plotted, " associations.")))
    }
    if (nrow(associations_list()$dframe) > plotly_limit){
      out <- tagList(out,
                     h5("Wow, your data is BIG! Plotting static figure."),
                     plotOutput("heatmap_static", height = input$window_size[2] - 100))
    }
    else{
      height <- min(input$window_size) * 0.95
      width <- height * 1.04
      out <- tagList(out,
                     plotlyOutput("heatmaply", width = width, height = height))
    }
    out <- tagList(out,
                   uiOutput("heatmap_download"))
    out
  })
  
  heatmap <- reactive({
    if (associations_list()$varnum == 1) {
      return(NULL)
    }
    plot_effect_heatmap(associations_list()$dframe, log2_effect = input$heatmap_log2,
                        color_scale = input$heatmap_color_scale, midpoint = input$heatmap_midpoint,
                        discretize_effect = input$heatmap_discrete, breaks = input$heatmap_breaks,
                        clustering = input$clustering, symmetrical = input$symmetrical,
                        lower_tri = input$lower_tri)
  })
  
  output$heatmaply <- renderPlotly({
    ggp <- heatmap()
    ggplotly(ggp, tooltip = paste0("label", 1:9))
  })
  
  output$heatmap_static <- renderPlot({
    heatmap()
  })
  
  output$heatmap_download <- renderUI({
    tagList(
      br(),
      fluidRow(
        column(1,
               downloadButton("heatmap_download_button"),
               br(),
               br(),
               uiOutput("heatmap_download_plotly"),
               br()),
        column(2,
               radioButtons("heatmap_download_format", label=NULL,
                            choices = c("png", "pdf")))
      )
    )
  })
  
  
  output$heatmap_download_button <- downloadHandler(
    filename = function(){
      paste("ninni_heatmap", input$heatmap_download_format, sep=".")
    },
    
    content = function(file){
      p <- heatmap()
      if(nrow(associations_list()$dframe) > plotly_limit){
        scale <- 1.5
      } else{
        scale <- 1
      }
      ggsave(file, p, width = 9, height = 8, dpi = 300, units = "in", scale = scale)
    }
  )
  
  output$heatmap_download_plotly <- renderUI({
    if (nrow(associations_list()$dframe) <= plotly_limit) {
      downloadButton("heatmap_downloadly", "Download Interactive")
    } else {
      NULL
    }
  })
  
  output$heatmap_downloadly <- downloadHandler(
    filename = function(){
      "ninni_heatmap.html"
    },
    
    content = function(file){
      p <- ggplotly(heatmap(), tooltip = paste0("label", 1:9))
      saveWidget(as_widget(ggplotly(p)), file, selfcontained = TRUE, title = "Ninni heat map")
    }
  )
  
  # ------------------- Volcano plot ---------------
  
  output$volcano <- renderUI({
    if (associations_list()$effect_type == "Multiple"){
      return(h5("Multiple different effect types can't be plotted together"))
    }
    if (nrow(associations_list()$dframe) > plotly_limit){
      out <- tagList(h5("Wow, your data is BIG! Plotting static figure."),
              plotOutput("volcano_static", height = paste0(input$window_size[2] - 100, "px")))
    }
    else{
      out <- plotlyOutput("volcanoly", height = paste0(input$window_size[2] - 100, "px"))
    }
    out <- tagList(out,
                   uiOutput("volcano_download"))
  })
  
  volcanoplot <- reactive({
    plot_volcano(dframe = associations_list()$dframe, log2_effect = input$volcano_log2,
                 effect_type = associations_list()$effect_type,
                 varnum = associations_list()$varnum, double_filter = input$double_filter,
                 df_p_lim = as.numeric(input$df_p_limit), fdr = input$df_p_limit_fdr,
                 df_effect_lim = input$df_effect_limit, eff_limit_log2 = input$df_eff_limit_log2,
                 shape = input$volcano_shape)
  })
  
  output$volcano_static <- renderPlot({
    volcanoplot()
  })
  
  output$volcanoly <- renderPlotly({
    ggp <- volcanoplot()
    ggplotly(ggp, tooltip = paste0("label", 1:9))
  })
  
  output$volcano_download <- renderUI({
    tagList(
      br(),
      fluidRow(
        column(1,
               downloadButton("volcano_download_button"),
               br(),
               br(),
               uiOutput("volcano_download_plotly"),
               br()),
        column(2,
               radioButtons("volcano_download_format",label=NULL,
                            choices = c("png","pdf")))
      )
    )
  })
  
  output$volcano_download_button <- downloadHandler(
    filename = function(){
      paste("ninni_volcano_plot", input$volcano_download_format, sep=".")
    },
    
    content = function(file){
      p <- volcanoplot()
      if(nrow(associations_list()$dframe) > plotly_limit){
        scale <- 1.5
      } else{
        scale <- 1
      }
      ggsave(file, p, width = 9, height = 8, dpi = 300, units = "in", scale = scale)
    }
  )
  
  output$volcano_download_plotly <- renderUI({
    if (nrow(associations_list()$dframe) <= plotly_limit) {
      downloadButton("volcano_downloadly", "Download Interactive")
    } else {
      NULL
    }
  })
  
  output$volcano_downloadly <- downloadHandler(
    filename = function(){
      "ninni_volcano_plot.html"
    },
    
    content = function(file){
      p <- ggplotly(volcanoplot(), tooltip = paste0("label", 1:9))
      saveWidget(as_widget(p), file, selfcontained = TRUE, title = "Ninni volcano plot")
    }
  )
  
  # ---------------- Q-Q plot -------------------------
  
  output$qq_plot_choices <- renderUI({
    tagList(
      checkboxInput("qq_coloring", "Coloring according to column"),
      conditionalPanel("input.qq_coloring == true",
                       radioButtons("qq_coloring_type",NULL,
                                    choices = c("Continuous", "Discrete"), inline = TRUE),
                       selectizeInput("qq_coloring_column","Column",
                                      choices = colnames(associations_list()$dframe),
                                      options = list(maxItems = 1,
                                                     placeholder = 'Choose a column',
                                                     onInitialize = I('function() { this.setValue(""); }'))))
    )
  })
  
  output$qq_plot <-renderUI({
    if (associations_list()$effect_type == "Multiple"){
      return(h5("Multiple different effect types can't be plotted together"))
    }
    
    if (nrow(associations_list()$dframe) > plotly_limit){
      out <- tagList(h5("Wow, your data is BIG! Plotting static figure."),
                     plotOutput("qq_plot_static", height = paste0(input$window_size[2] - 100, "px")))
    }
    else{
      out <- plotlyOutput("qq_plotly", height = paste0(input$window_size[2] - 100, "px"))
    }
    
    out <- tagList(out,
                   uiOutput("qq_plot_download"))
    out
  })
  
  qq_plot <- reactive({
    ggp <- gg_qq(dframe = associations_list()$dframe, variable = input$qq_choice,
          log2_effect = input$qq_log2, effect_type = associations_list()$effect_type,
          varnum = associations_list()$varnum, color_col = input$qq_coloring_column,
          color_type = input$qq_coloring_type)
    ggp
  })
  
  output$qq_plot_static <- renderPlot({
    qq_plot()
  })
  
  output$qq_plotly <- renderPlotly({
    ggp <- qq_plot()
    ggplotly(ggp, tooltip = paste0("label", 1:9))
  })
  
  output$qq_plot_download <- renderUI({
    tagList(
      br(),
      fluidRow(
        column(1,
               downloadButton("qq_plot_download_button"),
               br(),
               br(),
               uiOutput("qq_download_plotly"),
               br()),
        column(2,
               radioButtons("qq_plot_download_format", label=NULL,
                            choices = c("png", "pdf")))
      )
    )
  })
  
  output$qq_plot_download_button <- downloadHandler(
    filename = function(){
      paste("ninni_qq_plot",input$qq_plot_download_format,sep=".")
    },
    
    content = function(file){
      if(nrow(associations_list()$dframe) > plotly_limit){
        scale <- 1.5
      } else{
        scale <- 1
      }
      ggsave(file, qq_plot(), width = 9, height = 8, dpi = 300, units = "in", scale = scale)
    }
  )
  
  output$qq_download_plotly <- renderUI({
    if (nrow(associations_list()$dframe) <= plotly_limit) {
      downloadButton("qq_downloadly", "Download Interactive")
    } else {
      NULL
    }
  })
  
  output$qq_downloadly <- downloadHandler(
    filename = function(){
      "ninni_qq_plot.html"
    },
    
    content = function(file){
      p <- ggplotly(qq_plot(), tooltip = paste0("label", 1:9))
      saveWidget(as_widget(p), file, selfcontained = TRUE, title = "Ninni Q-Q plot")
    }
  )
  
  # ------------------ Lady Manhattan plot ---------------------
  
  # Toggle coloring by column
  # Choose discrete or continuous color scale (only relevant for numeric values)
  output$lady_manhattan_plot_choices <- renderUI({
    tagList(
      selectizeInput("lady_x_column", "Column for x-axis",
                     choices = c("Variables", colnames(associations_list()$dframe)),
                     selected = "Variables"),
      checkboxInput("lady_coloring", "Coloring according to column"),
      conditionalPanel("input.lady_coloring == true",
                       radioButtons("lady_coloring_type",NULL,
                                    choices = c("Continuous", "Discrete")),
                       selectizeInput("lady_coloring_column", "Column",
                                      choices = colnames(associations_list()$dframe),
                                      options = list(maxItems = 1,
                                                     placeholder = 'Choose a column',
                                                     onInitialize = I('function() { this.setValue(""); }'))))
    )
  })
  
  output$lady_manhattan_plot <-renderUI({
    if (associations_list()$effect_type == "Multiple"){
      return(h5("Multiple different effect types can't be plotted together"))
    }
    if (nrow(associations_list()$dframe) > plotly_limit){
      out <- tagList(h5("Wow, your data is BIG! Plotting static figure."),
                   plotOutput("lady_manhattan_plot_static", height = paste0(input$window_size[2] - 100, "px")))
    }
    else{
      out <- plotlyOutput("lady_manhattan_plotly", height = paste0(input$window_size[2] - 100, "px"))
    }
    out <- tagList(out,
                   uiOutput("lady_manhattan_download"))
    out
  })
  
  ladyplot <- reactive({
    if(input$lady_coloring && !is.null(input$lady_coloring_column) && input$lady_coloring_column != ""){
      lady_manhattan_plot(associations_list()$dframe, input$lady_x_column, input$lady_log2,
                          associations_list()$effect_type, associations_list()$varnum,
                          input$lady_coloring_column,input$lady_coloring_type)
    }
    else{
      lady_manhattan_plot(associations_list()$dframe, input$lady_x_column, input$lady_log2,
                          associations_list()$effect_type, associations_list()$varnum)
    }
  })
  
  output$lady_manhattan_plot_static <- renderPlot({
    ladyplot()
  })
  
  output$lady_manhattan_plotly <- renderPlotly({
    ggp <- ladyplot()
    ggplotly(ggp, tooltip = paste0("label", 1:9))
  })
  
  output$lady_manhattan_download <- renderUI({
    tagList(
      br(),
      fluidRow(
        column(1,
               downloadButton("lady_manhattan_download_button"),
               br(),
               br(),
               uiOutput("manhattan_download_plotly"),
               br()),
        column(2,
               radioButtons("lady_manhattan_download_format", label = NULL,
                            choices = c("png", "pdf")))
      )
    )
  })
  
  output$lady_manhattan_download_button <- downloadHandler(
    filename = function(){
      paste("ninni_manhattan_plot", input$lady_manhattan_download_format, sep = ".")
    },
    
    content = function(file){
      p <- ladyplot()
      if(nrow(associations_list()$dframe) > plotly_limit){
        scale <- 1.5
      } else{
        scale <- 1
      }
      ggsave(file, p, width = 9, height = 8, dpi = 300, units = "in", scale = scale)
    }
  )
  
  output$manhattan_download_plotly <- renderUI({
    if (nrow(associations_list()$dframe) <= plotly_limit) {
      downloadButton("manhattan_downloadly", "Download Interactive")
    } else {
      NULL
    }
  })
  
  output$manhattan_downloadly <- downloadHandler(
    filename = function(){
      "ninni_manhattan.html"
    },
    
    content = function(file){
      p <- ggplotly(ladyplot(), tooltip = paste0("label", 1:9))
      saveWidget(as_widget(p), file, selfcontained = TRUE, title = "Ninni Manhattan plot")
    }
  )
  
  # ----------- Lollipop plot --------------
  
  # Toggle coloring by column
  # Choose discrete or continuous color scale (only relevant for numeric values)
  output$lollipop_choices <- renderUI({
    if (associations_list()$varnum == 1){
      selected <- "Variable1"
      choices <- colnames(associations_list()$dframe)
    } else {
      selected <- "Variables together"
      choices <- c("Variables together", colnames(associations_list()$dframe))
    }
    tagList(
      selectizeInput("lollipop_column", "Column for column",
                     choices = choices,
                     selected = selected),
      numericInput("lollipop_n", "Number of top values to show", value = 10, min = 1),
      checkboxInput("lollipop_coloring", "Coloring according to column"),
      conditionalPanel("input.lollipop_coloring == true",
                       radioButtons("lollipop_coloring_type",NULL,
                                    choices = c("Continuous", "Discrete")),
                       selectizeInput("lollipop_coloring_column", "Column",
                                      choices = colnames(associations_list()$dframe),
                                      options = list(maxItems = 1,
                                                     placeholder = 'Choose a column',
                                                     onInitialize = I('function() { this.setValue(""); }'))))
    )
  })
  
  output$lollipop_plot <-renderUI({
    if (nrow(associations_list()$dframe) > plotly_limit){
      out <- tagList(h5("Wow, your data is BIG! Plotting static figure."),
                     plotOutput("lollipop_plot_static", height = paste0(input$window_size[2] - 100, "px")))
    }
    else{
      out <- plotlyOutput("lollipop_plotly", height = paste0(input$window_size[2] - 100, "px"))
    }
    out <- tagList(out,
                   uiOutput("lollipop_download"))
    out
  })
  
  lolliplot <- reactive({
    if(input$lollipop_coloring && !is.null(input$lollipop_coloring_column) && input$lollipop_coloring_column != ""){
      lollipop_plot(associations_list()$dframe, input$lollipop_column, input$lollipop_n,
                          input$lollipop_coloring_column,input$lollipop_coloring_type)
    }
    else{
      lollipop_plot(associations_list()$dframe, input$lollipop_column, input$lollipop_n)
    }
  })
  
  output$lollipop_plot_static <- renderPlot({
    lolliplot()
  })
  
  output$lollipop_plotly <- renderPlotly({
    ggp <- lolliplot()
    ggplotly(ggp)
  })
  
  output$lollipop_download <- renderUI({
    tagList(
      br(),
      fluidRow(
        column(1,
               downloadButton("lollipop_download_button"),
               br(),
               br(),
               uiOutput("lollipop_download_plotly"),
               br()),
        column(2,
               radioButtons("lollipop_download_format", label = NULL,
                            choices = c("png", "pdf")))
      )
    )
  })
  
  output$lollipop_download_button <- downloadHandler(
    filename = function(){
      paste("ninni_lollipop_plot", input$lollipop_download_format, sep = ".")
    },
    
    content = function(file){
      p <- lolliplot()
      if(nrow(associations_list()$dframe) > plotly_limit){
        scale <- 1.5
      } else{
        scale <- 1
      }
      ggsave(file, p, width = 9, height = 8, dpi = 300, units = "in", scale = scale)
    }
  )
  
  output$lollipop_download_plotly <- renderUI({
    if (nrow(associations_list()$dframe) <= plotly_limit) {
      downloadButton("lollipop_downloadly", "Download Interactive")
    } else {
      NULL
    }
  })
  
  output$lollipop_downloadly <- downloadHandler(
    filename = function(){
      "ninni_lollipop.html"
    },
    
    content = function(file){
      p <- ggplotly(lolliplot())
      saveWidget(as_widget(p), file, selfcontained = TRUE, title = "Ninni lollipop plot")
    }
  )
  
  # --------- UpSet plot -----------
  
  output$upset_choices <- renderUI({
    if (associations_list()$varnum == 1){
      selected <- "Variable1"
      choices <- colnames(associations_list()$dframe)
    } else {
      selected <- "Variables together"
      choices <- c("Variables together", colnames(associations_list()$dframe))
    }
    tagList(
      selectizeInput("upset_group", "Group by",
                     choices = colnames(associations_list()$dframe),
                     selected = "Dataset"),
      selectizeInput("upset_column", "Column for elements",
                     choices = choices,
                     selected = selected),
      numericInput("upset_n", "Number of top intersections to show", value = 10, min = 1),
      selectizeInput("upset_order", "Order by",
                     choices = c("Degree & Frequency", "Frequency")),
      sliderInput("upset_text_scale", "Text size",
                  min = 0.5, max = 4, value = 1, step = 0.1),
      checkboxInput("upset_empty", "Show empty intersections")
    )
  })
  
  output$upset_plot <- renderUI({
    n_groups <- length(unique(associations_list()$dframe[, input$upset_group]))
    if (n_groups < 2) {
      return(h5("Minimum of two groups is needed"))
    }
    
    tagList(h5("UpSet plots are only available as static figures"),
            plotOutput("upset_plot_static", height = paste0(input$window_size[2] - 100, "px")),
            uiOutput("upset_download"))

  })
  
  upplot <- reactive({
    upset_plot(associations_list()$dframe, input$upset_group, input$upset_column, input$upset_n,
               input$upset_order, input$upset_text_scale, input$upset_empty)
  })
  
  output$upset_plot_static <- renderPlot({
    upplot()
  })
  
  output$upset_download <- renderUI({
    tagList(
      br(),
      fluidRow(
        column(1,
               downloadButton("upset_download_button")),
        column(2,
               radioButtons("upset_download_format", label = NULL,
                            choices = c("png", "pdf")))
      )
    )
  })
  
  output$upset_download_button <- downloadHandler(
    filename = function(){
      paste("ninni_upset_plot", input$upset_download_format, sep = ".")
    },
    
    content = function(file){
      p <- upplot()
      if(nrow(associations_list()$dframe) > plotly_limit){
        scale <- 1.5
      } else{
        scale <- 1
      }
      ggsave(file, p, width = 9, height = 8, dpi = 300, units = "in", scale = scale)
    }
  )
  
  # --------- P-value histograms -----------
  
  output$phist_choices <- renderUI({
    
    tagList(
      fluidRow(
        column(6,
               selectizeInput("phist_facet", "Facet by",
                              choices = colnames(associations_list()$dframe),
                              selected = "Dataset",
                              options = list(maxItems = 1,
                                             placeholder = 'Choose a column',
                                             onInitialize = I('function() { this.setValue(""); }')))
               ),
        column(6,
               sliderInput("phist_width", "Plot width",
                           min = 200, max = input$window_size[1],
                           value = 800),
               sliderInput("phist_height", "Plot height",
                           min = 200, max = input$window_size[2],
                           value = 600))
      )
      
      
    )
  })
  
  output$phist_plot <- renderUI({
    if (is.null(input$phist_width) || is.null(input$phist_height)) {
      return(NULL)
    }
    tagList(plotOutput("phist_plot_static",
                       width = input$phist_width,
                       height = input$phist_height),
            uiOutput("phist_download"))
    
  })
  
  phistplot <- reactive({
    p_histogram(associations_list()$dframe, input$phist_facet)
  })
  
  output$phist_plot_static <- renderPlot({
    phistplot()
  })
  
  output$phist_download <- renderUI({
    tagList(
      br(),
      fluidRow(
        column(1,
               downloadButton("phist_download_button")),
        column(2,
               radioButtons("phist_download_format", label = NULL,
                            choices = c("png", "pdf")))
      )
    )
  })
  
  output$phist_download_button <- downloadHandler(
    filename = function(){
      paste0("ninni_phist_plot.", input$phist_download_format)
    },
    
    content = function(file){
      p <- phistplot()
      if (input$phist_download_format == "png") {
        png(file, width = input$phist_width, height = input$phist_height)
        print(p)
        dev.off()
      } else {
        print(9*input$phist_height/input$phist_width)
        if(large()){
          scale <- 1.5
        } else{
          scale <- 1
        }
        ggsave(file, p,
               width = 9, height = 9*input$phist_height/input$phist_width,
               dpi = 300, units = "in", scale = scale)
      }
    }
  )
  
  # output$phist_download_button <- static_downloader(
  #   file_name = "ninni_phist_plot",
  #   plotter = phistplot,
  #   file_format = input$phist_download_format,
  #   width = input$phist_width,
  #   height = input$phist_height,
  #   large = large()
  # )
  
  large <- reactive({
    nrow(associations_list()$dframe) > plotly_limit
  })
  
})