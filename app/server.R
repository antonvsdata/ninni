
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
    query_given <- length(input$ds_label) || input$var_keywords != "" || length(input$metadata_tags)
    if (!query_given) {
      validate("Please submit a query")
    }
    req(query_given)
    
    withProgress(message = "Retrieving dataset from database",{
      asso_list <- get_associations(pool, input$ds_label, input$var_keywords, input$metadata_tags)
      incProgress(0.3)
      asso_list$dframe <- join_variables(pool, asso_list$dframe, asso_list$datasets)
    })
    
    return (asso_list)
  })
  
  associations_list_query <- eventReactive(input$filter,{
    associations_list_query_()
  })
  
  
  
  extra_filters_react <- reactive({
    req(associations_list_query())
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
  
  # Check that filters have valid input values
  valid_filters <- reactive({
    req(associations_list_query())
    
    validate_filters(input)
  })
  
  
  # Filter the associations dataframe
  # Returns a list with following objects:
  # - datasets: table of the datasets
  # - dframe: a data frame with the associations
  # - varnum: the number of variables in the dataset
  # - effect_type
  associations_list <- eventReactive(input$filter,{
    req(associations_list_query())
    req(valid_filters())
    
    filter_associations_dframe(asso_list = associations_list_query(), input)
  })
  
  # Contains information about the loaded data
  output$ds_info <- renderUI({
    tableOutput("ds_info_table")
  })
  
  # Table showing information of the loaded data
  output$ds_info_table <- renderTable({
    req(associations_list())
    
    make_ds_info_table(asso_list = associations_list())
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
  large <- reactive({
    nrow(associations_list()$dframe) > plotly_limit
  })
  
  # ----------- Common plot controls ------------
  
  observeEvent(associations_list()$dframe, {
    # Set columns as choices
    selectize_inputs <- c("qq_coloring_column",
                          "lady_coloring_column",
                          "lady_x_column",
                          "lollipop_column",
                          "lollipop_coloring_column",
                          "upset_group",
                          "upset_column",
                          "phist_facet")
    for (sel_input in selectize_inputs) {
      if (sel_input %in% c("lady_x_column", "lollipop_column", "upset_column")){
        if (associations_list()$varnum == 1){
          choices <- colnames(associations_list()$dframe)
        } else {
          choices <- c("Variables together", colnames(associations_list()$dframe))
        }
      } else {
        choices = colnames(associations_list()$dframe)
      }
      updateSelectizeInput(session, sel_input,
                           choices = choices)
    }
    
  })
  
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
  
  
  
})