
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
    },
    include.rownames=FALSE, include.colnames = FALSE)
  
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
  
  
  
  # ----------- Common plot controls ------------
  
  # All the visualizations can be interactive plotly figures,
  # or static figures, if dataset has more than 10 000 associations
  
  plotly_limit <- 10000
  large <- reactive({
    nrow(associations_list()$dframe) > plotly_limit
  })
  
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
  
  
  output$heatmap_n_plotted <- renderUI({
     
    # Check if there are associations with only one variable
    # They will be removed before plotting the heatmap
    n_not_plotted <- length(which(is.na(associations_list()$dframe$Variable1) |
                                    is.na(associations_list()$dframe$Variable2)))
    if(n_not_plotted > 0){
      n_plotted <- nrow(associations_list()$dframe) - n_not_plotted
      return(h5(paste0("Only associations with 2 variables will be plotted in the heat map.
                                   Removed ", n_not_plotted," associations, plotted ",
                                   n_plotted, " associations.")))
    } else {
      return(NULL)
    }
  })
  
  heatmap <- reactive({
    req(associations_list()$varnum != 1)
    
    plot_effect_heatmap(associations_list()$dframe, log2_effect = input$heatmap_log2,
                        color_scale = input$heatmap_color_scale, midpoint = input$heatmap_midpoint,
                        discretize_effect = input$heatmap_discrete, breaks = input$heatmap_breaks,
                        clustering = input$clustering, symmetrical = input$symmetrical,
                        lower_tri = input$lower_tri)
  })
  
  heatmap_msg <- reactive({
    if (associations_list()$varnum == 1) {
      return(h5("Two variables needed for heatmap"))
    } else {
      return(NULL)
    }
  })
  
  plotServer("heatmap", plotter = heatmap, large = large,
             msg = heatmap_msg)
  
  # ------------------- Volcano plot ---------------
  
  
  
  volcanoplot <- reactive({
    plot_volcano(dframe = associations_list()$dframe, log2_effect = input$volcano_log2,
                 effect_type = associations_list()$effect_type,
                 varnum = associations_list()$varnum, double_filter = input$double_filter,
                 df_p_lim = as.numeric(input$df_p_limit), p_adj = input$df_p_limit_adj,
                 df_effect_lim = input$df_effect_limit, eff_limit_log2 = input$df_eff_limit_log2,
                 shape = input$volcano_shape)
  })
  
  plotServer("volcano_plot", plotter = volcanoplot, large = large)
  
  # ---------------- Q-Q plot -------------------------
  
  
  
  qq_plot <- reactive({
    ggp <- gg_qq(dframe = associations_list()$dframe, variable = input$qq_choice,
          log2_effect = input$qq_log2, effect_type = associations_list()$effect_type,
          varnum = associations_list()$varnum, color_col = input$qq_coloring_column,
          color_type = input$qq_coloring_type)
    ggp
  })
  
  plotServer("qq_plot", plotter = qq_plot, large = large)
  
  # ------------------ Lady Manhattan plot ---------------------
  
 
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
  
  lady_msg <- reactive({
    if (is.null(input$lady_x_column) || input$lady_x_column == "") {
      return(h5("Please select a column for x axis"))
    } else {
      return(NULL)
    }
  })
  
  plotServer("manhattan", plotter = ladyplot, large = large,
             msg = lady_msg)
  
  
  
  # ----------- Lollipop plot --------------
  
  lolliplot <- reactive({
    if(input$lollipop_coloring && !is.null(input$lollipop_coloring_column) && input$lollipop_coloring_column != ""){
      lollipop_plot(associations_list()$dframe, input$lollipop_column, input$lollipop_n,
                          input$lollipop_coloring_column,input$lollipop_coloring_type)
    }
    else{
      lollipop_plot(associations_list()$dframe, input$lollipop_column, input$lollipop_n)
    }
  })
  
  plotServer("lollipop", plotter = lolliplot, large = reactive(FALSE))
  
  # --------- UpSet plot -----------
  
  upset_msg <- reactive({
    n_groups <- length(unique(associations_list()$dframe[, input$upset_group]))
    if (n_groups < 2) {
      return(h5("Minimum of two groups is needed"))
    } else if (is.null(input$upset_column) || input$upset_column == "") {
      return(h5("Please select a column for the elements"))
    } else {
      return(NULL)
    }
  })
  
  upplot <- reactive({
    upset_plot(associations_list()$dframe, input$upset_group, input$upset_column, input$upset_n,
               input$upset_order, input$upset_text_scale, input$upset_empty)
  })
  
  plotServer("upset_plot", plotter = upplot,
             large = large,
             include_plotly = FALSE,
             msg = upset_msg)
  
  
  # --------- P-value histograms -----------
  
  phistplot <- reactive({
    p_histogram(associations_list()$dframe, input$phist_facet)
  })
  
  plotServer("p_histogram", plotter = phistplot,
             large = large, include_plotly = FALSE)

  
})