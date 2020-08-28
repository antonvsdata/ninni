
# All elements are ordered by the tabs in Ninni
shinyServer(function(input, output, session){
  
  #----------- Sidebar ----------------
  
  # Multiple choice dropwdown box for choosing datasets
  
  # Multiple choice dropdown: search database for datasets by metadata tags
  observeEvent(ds_dframe, {
    if (!db_empty()) {
      md_labels <- extract_meta_labels(ds_dframe())
      updateSelectizeInput(session, "metadata_tags", choices = md_labels)
    }
  })
  
  # Handles the query to the database
  # Reactive expressions cache their value, so filtering the same dataset multiple times
  # does not provoke a new database query
  associations_list_query_ <-  reactive({
    if (db_empty()){
      validate("Database is empty")
    }
    req(!db_empty())
    
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
    datatable(ds_dframe(), selection = "none")
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
  
  # Set columns of association data frame as choices in drop downs
  observeEvent(associations_list_query()$dframe, {
    
    selectize_inputs <- c("qq_coloring_column",
                          "lady_coloring_column",
                          "lady_x_column",
                          "lollipop_column",
                          "upset_group",
                          "upset_column",
                          "phist_facet",
                          "ridge_x",
                          "ridge_y",
                          "edge_color",
                          "edge_width",
                          "edge_weight")
    for (sel_input in selectize_inputs) {
      # Only numerics
      if (sel_input %in% "ridge_x") {
        choices <- colnames(associations_list_query()$dframe)[sapply(associations_list_query()$dframe, is.numeric)]
      }
      # Only discrete
      else if (sel_input %in% c("upset_group", "ridge_y", "lollipop_column", "upset_column")) {
        choices <- colnames(associations_list_query()$dframe)[!sapply(associations_list_query()$dframe, is.numeric)]
      } else {
        choices <- colnames(associations_list_query()$dframe)
      }
      if (sel_input %in% c("lady_coloring_column", "phist_facet", "edge_color", "edge_width", "edge_weight")) {
        choices <- c("none", choices)
      }
      # Variables together allowed
      if (sel_input %in% c("lady_x_column", "lollipop_column", "upset_column") &&
          associations_list_query()$varnum == 2){
          choices <- c("Variables together", choices)
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
                        p = input$heatmap_p, p_limit = input$heatmap_p_limit,
                        point_size_range = input$heatmap_point_range,
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
    lady_manhattan_plot(associations_list()$dframe, input$lady_x_column, input$lady_log2,
                        associations_list()$effect_type, associations_list()$varnum,
                        input$lady_coloring_column,input$lady_coloring_type)
    
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
    lollipop_plot(associations_list()$dframe, input$lollipop_column, input$lollipop_n)
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
             large = reactive(TRUE),
             msg = upset_msg)
  
  
  # --------- P-value histograms -----------
  
  phistplot <- reactive({
    p_histogram(associations_list()$dframe, input$phist_facet)
  })
  
  plotServer("p_histogram", plotter = phistplot,
             large = reactive(TRUE))
  
  
  # ------- Ridge plots --------------
  
  ridge <- reactive({
    req(input$ridge_x)
    req(input$ridge_y)
    ridge_plot(associations_list()$dframe, input$ridge_x, input$ridge_y,
               input$ridge_log2, input$ridge_scale, input$ridge_style)
  })
  
  ridge_msg <- reactive({
    if (!is.numeric(associations_list()$dframe[, input$ridge_x])) {
      return(h5("variable on the"))
    }
  })
  
  plotServer("ridge", plotter = ridge, large = reactive(TRUE))
  
  # ------- Network plot ----------
  
  networkgg <- reactive({
    network_plot(associations_list()$dframe, type = input$network_type, layout = input$network_layout,
                 node_names = input$node_names, names_repel = input$node_repel,
                 edge_color = input$edge_color, edge_width = input$edge_width, edge_weight = input$edge_weight,
                 edge_width_range = input$edge_width_range,
                 edge_color_log2 = input$edge_color_log2, edge_width_log2 = input$edge_width_log2,
                 edge_weight_log2 = input$edge_weight_log2,
                 edge_color_scale = input$edge_color_scale, edge_color_midpoint = input$edge_midpoint)
  })
  
  network_msg <- reactive({
    if (nrow(associations_list()$dframe) > 5000) {
      return(h5("Network plots are disabled for over 5000 associations, please filter the associations"))
    } else if (input$network_type == "var_to_var" && associations_list()$varnum == 1) {
      return(h5("Two variable dataset needed"))
    }
  })
  
  output$d3network <- renderUI({
    if (nrow(associations_list()$dframe) < 1000) {
      out <- tagList(forceNetworkOutput("d3network_plot", width = input$network_width,
                                        height = input$network_height))
    } else {
      out <- tagList(
        p("Interactive networks are disabled for large networks.")
      )
    }
    
    out
  })
  
  output$d3network_plot <- renderForceNetwork({
    interactive_network(g = networkgg()$graph, type = input$network_type,
                        node_size = input$node_size, link_distance = input$link_distance,
                        font_size = input$font_size)
  })
  
  
  plotServer("network", plotter = reactive({networkgg()$plot}), large = reactive(TRUE), msg = network_msg)
  
  
  # ---------------- Admin -------------
  
  admin_ui_comp <- eventReactive(input$login, {
    if (!file.exists("www/user.config")) {
      return(error_html("Admin user configuration file not found, check setup instructions on GitHub"))
    }
    user <- read_user_info("www/user.config")
    if (input$username == user$username && bcrypt::checkpw(input$password, user$password)){
      tagList(
        tabsetPanel(
          
          tabPanel("Import data to Ninni",
                   br(),
                   br(),
                   
                   fileInput("dataset_file", "Upload a .csv file listing the datasets", accept = ".csv"),
                   
                   fileInput("metadata_file", "Upload a .csv file containing the dataset metadata", accept = ".csv"),
                   
                   # Upload zip files
                   fileInput("zipfile", "Upload .zip file containing the actual results and variable descriptions as .csv files", accept = ".zip"),
                   # action button to unzip the file
                   
                   tableOutput("data_files"),
                   
                   actionButton("clear_files", "Clear all data files"),
                   br(),
                   br(),
                   checkboxInput("clear", "Clear the database before importing data",
                                 value = FALSE),
                   
                   actionButton("import", "Import data"),
                   
                   
                   
                   htmlOutput("import_errors")),
          
          tabPanel("Delete data from database",
                   
                   br(),
                   br(),
                   
                   selectizeInput("dataset_label", "Dataset label to delete",
                                  choices = NULL,
                                  selected = isolate(input$dataset_label),
                                  options = list(maxItems = 1,
                                                 placeholder = 'Choose a dataset',
                                                 onInitialize = I('function() { this.setValue(""); }'))),
                   
                   actionButton("delete", "Delete"),
                   
                   br(),
                   br(),
                   
                   DT::dataTableOutput("ds_table")
          )
        )
      )
    } else {
      error_html("wrong username or password")
    }
  })
  
  output$admin_ui <- renderUI({
    admin_ui_comp()
  })
  
  
  unzipped <- observe({
    if (is.null(input$zipfile)) {
      return(NULL)
    }
    unzip(input$zipfile$datapath, exdir = "data")
  })
  
  observeEvent(input$clear_files, {
    unlink("data/*", recursive = TRUE, force = TRUE)
  })
  
  output$data_files <- renderTable({
    input$clear_files
    input$zipfile
    
    files <- list.files("data/")
    if (length(files)) {
      sizes <- sapply(paste0("data/", files), function(f) {
        round(file.info(f)$size/1000)
      })
      size_df <- data.frame("Existing files" = files,
                            "Size (kB)" = as.integer(sizes),
                            check.names = FALSE)
    } else {
      size_df <- data.frame("Existing files" = "No files found",
                            check.names = FALSE)
    }
    size_df
  })
  
  datasets <- reactive({
    read.csv(input$dataset_file$datapath, stringsAsFactors = FALSE)
  })
  
  metadata <- reactive({
    if (is.null(input$metadata_file)){
      return(NULL)
    }
    read.csv(input$metadata_file$datapath, stringsAsFactors = FALSE)
  })
  
  file_errors <- eventReactive(input$import, {
    if (is.null(input$dataset_file)) {
      return(error_html("Datasets file not found"))
    }
    check_files(datasets(), metadata())
  })
  
  output$import_errors <- renderUI({
    file_errors()
  })
  
  
  observeEvent(input$import,{
    if (!is.null(file_errors())) {
      return(NULL)
    }
    
    progress <- Progress$new(session, min=0, max=1)
    
    progress$set(message = "Connecting to database")
    imported <- NA
    error_msg <- ""
    t <- system.time({
      
      
      tryCatch({
        imported <- import_data(pool, datasets = datasets(), metadata = metadata(), clear = input$clear,
                                progress = progress)
      }, error = function(e) {
        error_msg <<- e$message
        print(e$message)
      })
      
      progress$close()
    })
    if (is.na(imported)) {
      showModal(modalDialog(
        title = "Data not imported",
        paste("Data could not be imported:", error_msg),
        easyClose = TRUE
      ))
    } else {
      if (imported) {
        showModal(modalDialog(
          title = "Data imported",
          paste("Data imported in", format_time(t["elapsed"])),
          easyClose = TRUE
        ))
      } else {
        showModal(modalDialog(
          title = "Data already in database",
          "All datasets are already´in the database",
          easyClose = TRUE
        ))
      }
    }
    
  })
  
  modal_confirm <- reactive({modalDialog(
    paste("Are you sure you want to delete", input$dataset_label),
    title = "Deleting dataset",
    footer = tagList(
      actionButton("cancel", "Cancel"),
      actionButton("ok", "Delete", class = "btn btn-danger")
    )
  )
  })
  
  observeEvent(input$delete, {
    if (input$dataset_label != "") {
      showModal(modal_confirm())
    }
    
  })
  
  observeEvent(input$cancel, 
               removeModal()
  )
  
  observeEvent(input$ok, {
    
    progress <- Progress$new(session, min=0, max=1)
    
    progress$set(message = "Connecting to database")
    success <- delete_dataset(pool, input$dataset_label, progress = NULL)
    progress$close()
    
    if (success) {
      showModal(modalDialog(
        title = "Dataset deleted",
        paste("Dataset", input$dataset_label, "deleted"),
        easyClose = TRUE
      ))
    } else {
      showModal(modalDialog(
        title = "Dataset not deleted",
        paste("Dataset could not be deleted"),
        easyClose = TRUE
      ))
    }
    
    
  })
  
  # Load datasets from database and update selections for dataset label
  ds_dframe <- reactive({
    input$ok
    input$login
    input$import
    df <- get_datasets(pool)
    updateSelectizeInput(session, "dataset_label",
                         choices = df$Label,
                         selected = isolate(input$dataset_label))
    updateSelectizeInput(session, "ds_label",
                         choices = df$Label)
    df
  })
  
  
  db_empty <- reactive({
    nrow(ds_dframe()) == 0
  })
  
  
  onStop(function(){
    unlink("data/*", recursive = TRUE, force = TRUE)
  })
  
  output$ds_table <- DT::renderDataTable({
    datatable(ds_dframe(), selection = "none")
  })
  
})