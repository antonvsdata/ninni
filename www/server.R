
# All elements are ordered by the tabs in Ninni
shinyServer(function(input,output){
  
  #----------- Sidebar ----------------
  
  # Multiple choice dropwdown box for choosing datasets
  output$ds_choice <- renderUI({
    selectizeInput("ds_label",label = "Dataset label", width = "100%", multiple = TRUE,
                   choices = ds_dframe$Label, options = list(placeholder = "Choose a dataset"))
  })
  # Multiple choice dropdown: search database for datasets by metadata tags
  output$metadata_tags_ui <- renderUI({
    selectizeInput("metadata_tags","Metadata tags", width = "100%", multiple = TRUE,
                   choices = na.omit(ds_dframe$Metadata_labels), options = list(placeholder = "Choose metadata tags"))
  })
  
  # Handles the query to the database
  # Reactive expressions cache their value, so filtering the same dataset multiple times
  # does not provoke a new database query
  associations_list_query <- eventReactive(input$query,{
    if (!length(input$ds_label) & input$var_keywords == "" & !length(input$metadata_tags)){
      return (NULL)
    }
    withProgress(message = "Retrieving dataset from database",{
      associations_list <- get_associations(pool,input$ds_label, input$var_keywords, input$metadata_tags)
      incProgress(0.3)
      associations_list$dframe <- join_variables(pool,associations_list$dframe,associations_list$datasets)
    })
    
    return (associations_list)
  })
  
  associations_list_db <- eventReactive(input$submit,{
    associations_list_query()
  })
  
  # Filters for loaded data
  output$filters <- renderUI({
    if(is.null(standard_filters())){
      return(NULL)
    }
    tagList(
      br(),
      standard_filters(),
      extra_filters(),
      variable_filters(),
      actionButton("filter",
                   label = "Filter")
    )
  })
  
  # Filters for associations, always visible
  standard_filters <- reactive({
    if(is.null(associations_list_query())){
      return(NULL)
    }
    tagList(
      h4("Association filters"),
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
  
  # Generate filters for possible metavariables
  extra_filters <- reactive({
    if(is.null(associations_list_query())){
      return(NULL)
    }
    if(associations_list_query()$varnum == 2){
      col_limit <- 11
    }
    else{ #all datasets have varnum == 1
      col_limit <- 9
    }
    dframe <- associations_list_query()$dframe %>% as.data.frame()
    if(ncol(dframe) == col_limit){
      return(NULL)
    }
    # If the number of columns exceeds the number of standard columns, there are metavariables
    # Generate filters for these metavariables:
    # min & max if numeric
    # keyword search if character
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
  
  # Filters for filtering loaded data by variable
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
  
  # Filter the associations dataframe
  # Returns a list with following objects:
  # - datasets: table of the datasets
  # - dframe: a data frame with the associations
  # - varnum: the number of variables in the dataset
  # - effect_type
  associations_list <- eventReactive(input$filter,{
    
    associations_list <- associations_list_query()
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
      if(associations_list$varnum == 2){
        cols <- c("Variable1", "Variable2")
      }
      else{
        cols <- "Variable1"
      }
      dframe <- filter_by_keyword(dframe,cols,input$var_labels)
    }
    # Description
    # Keywords, comma separated
    if (input$description_labels != ""){
      if(associations_list$varnum == 2){
        cols <- c("Description1","Description2")
      }
      else{
        cols <- "Description1"
      }
      dframe <- filter_by_keyword(dframe,cols,input$description_labels)
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
    
    # Filters for metavariables
    if(!is.null(extra_filters())){
      if(associations_list$varnum == 2){
        col_limit <- 11
      }
      else{ # each varnum == 1
        col_limit <- 9
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
          keywords <- eval(parse(text = expr))
          if(keywords != ""){
            dframe <- filter_by_keyword(dframe,colnames(dframe)[i], keywords)
          }
        }
      }
    }
    associations_list$dframe <- dframe
    return(associations_list)
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
    string <- c("Number of datasets","Effect type(s)", "Number of associations:","Number of unique variables:","P-value < 0.05","P-value (FDR) < 0.05",
             "P-value range:","Effect range:")
    values <- c(nrow(associations_list()$datasets),
                associations_list()$datasets$effect_type %>% unique() %>% paste(collapse=","),
                nrow(associations_list()$dframe))
    if (associations_list()$varnum ==2){
      values <- c(values, c(associations_list()$dframe$Variable1,associations_list()$dframe$Variable2) %>%
                    unique() %>% length())
    }
    else{
      values <- c(values, associations_list()$dframe$Variable1 %>%
                    unique() %>% length())
    }
    values <- c(values,associations_list()$dframe %>% filter(P < 0.05) %>% nrow(),
                associations_list()$dframe %>% filter(P_FDR < 0.05) %>% nrow(),
                paste((associations_list()$dframe$P) %>% min() %>% signif(digits=3), "...",
                      (associations_list()$dframe$P) %>% max() %>% signif(digits=3)),
                paste((associations_list()$dframe$Effect) %>% min() %>% signif(digits=3), "...",
                      (associations_list()$dframe$Effect) %>% max() %>% signif(digits=3)))
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
    for(i in 1:ncol(dframe)){
      if(class(dframe[,i]) == "numeric"){
        dframe[,i] <- signif(dframe[,i],digits = 3)
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
      write.csv(associations_list()$dframe,file, row.names = F)
    }
  )
  
  # All the visualizations can be interactive plotly figures,
  # or static figures, if dataset has more than 10 000 associations
  
  # -------------- Heat map --------------------
  
  output$heatmap <- renderUI({
    if (associations_list()$effect_type == "Multiple"){
      return(h5("Multiple different effect types can't be plotted together"))
    }
    if (associations_list()$varnum == 1){
      return(h5("Heat map requires associations with 2 variables"))
    }
    # Check if the same variables have multiple different associations
    n_copies <- associations_list()$dframe %>%
      group_by(Variable1, Variable2) %>%
      summarise(n = n())
    if (any(n_copies$n > 1)){
      return(h5("Dataset contains multiple copies of the same association: heat map can't be plotted"))
    }
    
    out <- tagList()
    # Check if there are associations with only one variable
    # They will be removed before plotting the heatmap
    n_not_plotted <- length(which(is.na(associations_list()$dframe$Variable1) | is.na(associations_list()$dframe$Variable2)))
    if(n_not_plotted > 0){
      n_plotted <- nrow(associations_list()$dframe) - n_not_plotted
      out <- tagList(out, h5(paste("Only associations with 2 variables will be plotted in the heat map. Removed ",n_not_plotted," associations, plotted ", n_plotted, " associations.", sep="")))
    }
    if (nrow(associations_list()$dframe) > 10000){
      out <- tagList(out,
                     h5("Wow, your data is BIG! Plotting static figure."),
                     plotOutput("heatmap_static", height = paste(input$window_size[2] - 100,"px",sep="")))
    }
    else{
      out <- tagList(out,
                     plotlyOutput("heatmaply", height = paste(input$window_size[2] - 100,"px",sep="")))
    }
    out <- tagList(out,
                   uiOutput("heatmap_download"))
    out
  })
  
  output$heatmaply <- renderPlotly({
    get_heatmap_lowertri(associations_list()$dframe, associations_list()$effect_type,input$clustering,interactive = TRUE)
  })
  
  output$heatmap_static <- renderPlot({
    get_heatmap_lowertri(associations_list()$dframe, associations_list()$effect_type,input$clustering,interactive = FALSE)
  })
  
  output$heatmap_download <- renderUI({
    tagList(
      br(),
      fluidRow(
        column(1,
               downloadButton("heatmap_download_button")),
        column(2,
               radioButtons("heatmap_download_format",label=NULL,
                            choices = c("png","pdf")))
      )
    )
  })
  
  output$heatmap_download_button <- downloadHandler(
    filename = function(){
      paste("ninni_heatmap",input$heatmap_download_format,sep=".")
    },
    
    content = function(file){
      p <- get_heatmap_lowertri(associations_list()$dframe, associations_list()$effect_type,input$clustering,interactive = FALSE)
      if(nrow(associations_list()$dframe) > 10000){
        scale <- 1.5
      } else{
        scale <- 1
      }
      ggsave(file,p, width = 9, height = 8, dpi = 300, units = "in", scale = scale)
    }
  )
  
  # ------------------- Volcano plot ---------------
  
  output$volcano <- renderUI({
    if (associations_list()$effect_type == "Multiple"){
      return(h5("Multiple different effect types can't be plotted together"))
    }
    if (nrow(associations_list()$dframe) > 10000){
      out <- tagList(h5("Wow, your data is BIG! Plotting static figure."),
              plotOutput("volcano_static", height = paste(input$window_size[2] - 100,"px",sep="")))
    }
    else{
      out <- plotlyOutput("volcanoly", height = paste(input$window_size[2] - 100,"px",sep=""))
    }
    out <- tagList(out,
                   uiOutput("volcano_download"))
  })
  
  output$volcano_static <- renderPlot({
    volcanoplot(associations_list()$dframe,associations_list()$effect_type,associations_list()$varnum,input$double_filter,
                       as.numeric(input$df_p_limit),input$df_p_limit_fdr, input$df_effect_limit, input$df_eff_limit_log2, interactive = FALSE)
  })
  
  output$volcanoly <- renderPlotly({
    volcanoplot(associations_list()$dframe,associations_list()$effect_type,associations_list()$varnum,input$double_filter,
                         as.numeric(input$df_p_limit),input$df_p_limit_fdr, input$df_effect_limit, input$df_eff_limit_log2, interactive = TRUE)
    
  })
  
  output$volcano_download <- renderUI({
    tagList(
      br(),
      fluidRow(
        column(1,
               downloadButton("volcano_download_button")),
        column(2,
               radioButtons("volcano_download_format",label=NULL,
                            choices = c("png","pdf")))
      )
    )
  })
  
  output$volcano_download_button <- downloadHandler(
    filename = function(){
      paste("ninni_volcano_plot",input$volcano_download_format,sep=".")
    },
    
    content = function(file){
      p <- volcanoplot(associations_list()$dframe,associations_list()$effect_type,associations_list()$varnum,input$double_filter,
                       as.numeric(input$df_p_limit),input$df_p_limit_fdr, input$df_effect_limit, input$df_eff_limit_log2, interactive = FALSE)
      if(nrow(associations_list()$dframe) > 10000){
        scale <- 1.5
      } else{
        scale <- 1
      }
      ggsave(file,p, width = 9, height = 8, dpi = 300, units = "in", scale = scale)
    }
  )
  
  # ---------------- Q-Q plot -------------------------
  
  output$qq_plot <-renderUI({
    if (associations_list()$effect_type == "Multiple"){
      return(h5("Multiple different effect types can't be plotted together"))
    }
    if (input$qq_choice == "p-values"){
      if (nrow(associations_list()$dframe) > 10000){
        out <- tagList(h5("Wow, your data is BIG! Plotting static figure."),
                     plotOutput("qq_plot_static_ps", height = paste(input$window_size[2] - 100,"px",sep="")))
      }
      else{
        out <- plotlyOutput("qq_plotly_ps",height = paste(input$window_size[2] - 100,"px",sep=""))
      }
    }
    if (input$qq_choice == "norm"){
      if (nrow(associations_list()$dframe) > 10000){
        out <- tagList(h5("Wow, your data is BIG! Plotting static figure."),
                     plotOutput("qq_plot_static_norm", height = paste(input$window_size[2] - 100,"px",sep="")))
      }
      else{
        out <- plotlyOutput("qq_plotly_norm", height = paste(input$window_size[2] - 100,"px",sep=""))
      }
    }
    out <- tagList(out,
                   uiOutput("qq_plot_download"))
    out
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
  
  output$qq_plot_download <- renderUI({
    tagList(
      br(),
      fluidRow(
        column(1,
               downloadButton("qq_plot_download_button")),
        column(2,
               radioButtons("qq_plot_download_format",label=NULL,
                            choices = c("png","pdf")))
      )
    )
  })
  
  output$qq_plot_download_button <- downloadHandler(
    filename = function(){
      paste("ninni_qq_plot",input$qq_plot_download_format,sep=".")
    },
    
    content = function(file){
      if (input$qq_choice == "p-values"){
        p <- qq_pvalues(associations_list()$dframe,associations_list()$varnum, interactive = FALSE)
      }
      if (input$qq_choice == "norm"){
        p <- qq_normal(associations_list()$dframe, associations_list()$effect_type,
                       associations_list()$varnum, interactive = FALSE)
      }
      if(nrow(associations_list()$dframe) > 10000){
        scale <- 1.5
      } else{
        scale <- 1
      }
      ggsave(file,p, width = 9, height = 8, dpi = 300, units = "in", scale = scale)
    }
  )
  
  # ------------------ Lady Manhattan plot ---------------------
  
  # Toggle coloring by column
  # Choose discrete or continuous color scale (only relevant for numeric values)
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
    if (associations_list()$effect_type == "Multiple"){
      return(h5("Multiple different effect types can't be plotted together"))
    }
    if (nrow(associations_list()$dframe) > 10000){
      out <- tagList(h5("Wow, your data is BIG! Plotting static figure."),
                   plotOutput("lady_manhattan_plot_static", height = paste(input$window_size[2] - 100,"px",sep="")))
    }
    else{
      out <- plotlyOutput("lady_manhattan_plotly", height = paste(input$window_size[2] - 100,"px",sep=""))
    }
    out <- tagList(out,
                   uiOutput("lady_manhattan_download"))
    out
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
  
  output$lady_manhattan_download <- renderUI({
    tagList(
      br(),
      fluidRow(
        column(1,
               downloadButton("lady_manhattan_download_button")),
        column(2,
               radioButtons("lady_manhattan_download_format",label=NULL,
                            choices = c("png","pdf")))
      )
    )
  })
  
  output$lady_manhattan_download_button <- downloadHandler(
    filename = function(){
      paste("ninni_lady_manhattan_plot",input$lady_manhattan_download_format,sep=".")
    },
    
    content = function(file){
      if(input$lady_coloring & !is.null(input$lady_coloring_column) & input$lady_coloring_column != ""){
        p <- lady_manhattan_plot(associations_list()$dframe,associations_list()$effect_type,associations_list()$varnum, interactive = FALSE, input$lady_coloring_column,input$lady_coloring_type)
      }
      else{
        p <- lady_manhattan_plot(associations_list()$dframe,associations_list()$effect_type,associations_list()$varnum, interactive = FALSE)
      }
      if(nrow(associations_list()$dframe) > 10000){
        scale <- 1.5
      } else{
        scale <- 1
      }
      ggsave(file,p, width = 9, height = 8, dpi = 300, units = "in", scale = scale)
    }
  )
  
})