shinyUI( fluidPage(
  
  includeCSS("www/styles.css"),
  shinyFeedback::useShinyFeedback(),
  
  # a JavaScript script for capturing the window size
  # found from https://stackoverflow.com/questions/36995142/get-the-size-of-the-window-in-shiny
  tags$head(tags$script('var dimension = [0, 0];
                          $(document).on("shiny:connected", function(e) {
                              dimension[0] = window.innerWidth;
                              dimension[1] = window.innerHeight;
                              Shiny.onInputChange("window_size", dimension);
                          });
                          $(window).resize(function(e) {
                              dimension[0] = window.innerWidth;
                              dimension[1] = window.innerHeight;
                              Shiny.onInputChange("window_size", dimension);
                          });')),
  
  titlePanel("Ninni"),
  
  sidebarLayout(
    # Sidebar contains inputs for searching Ninni's database for information
    # and filters for loaded datasets
    sidebarPanel(
      # Search options for choosing a dataset
      h4("Dataset"),
      #uiOutput("ds_choice"),
      selectizeInput("ds_label",label = "Dataset label", width = "100%", multiple = TRUE,
                     choices = NULL, options = list(placeholder = "Choose a dataset")),
      #uiOutput("metadata_tags_ui"),
      selectizeInput("metadata_tags","Metadata tags", width = "100%", multiple = TRUE,
                     choices = NULL,
                     options = list(placeholder = "Choose metadata tags")),
      textInput("var_keywords","Variable keywords"),
      
      # Filters for filtering associations
      # Like variable names, p-value, effect size
      checkboxInput("toggle_standard_filters", "Show standard filters"),
      conditionalPanel("input.toggle_standard_filters == true",
                       h4("Association filters"),
                       strong("Variable"),
                       textInput("var_labels", "Keywords, comma separated"),
                       
                       fluidRow(
                         column(6,
                                textInput("p_limit", label = "P-value <")),
                         column(4,
                                radioButtons("p_limit_adj", label = NULL,
                                             choices = c("Unadjusted" = FALSE,
                                                         "Adjusted" = TRUE),
                                             selected = FALSE))),
                       fluidRow(
                         column(7,
                                textInput("n_limit",
                                          label = "Minimum n"))
                       ),
                       strong("Effect:"),
                       fluidRow(
                         column(5,
                                textInput("eff_min", label="min")
                         ),
                         column(5,
                                textInput("eff_max", label = "max"))),
                       strong("Description"),
                       textInput("description_labels","Keywords, comma separated")),
      
      # Extra filters based on non-required columns.
      checkboxInput("toggle_extra_filters","Show extra filters"),
      conditionalPanel("input.toggle_extra_filters == true",
                       uiOutput("extra_filters")),
      # Filter for variables, e.g. at least one association with p < 0.05
      # Keeps all associations for particular variable
      checkboxInput("toggle_variable_filters", "Show variable filters"),
      conditionalPanel("input.toggle_variable_filters == true",
                       h4("Variable filters"),
                       h5("At least one association with"),
                       
                       fluidRow(
                         column(6,
                                textInput("var_p_limit", label = "P-value <")),
                         column(4,
                                radioButtons("var_p_limit_adj", label = NULL,
                                             choices = c("Unadjusted" = FALSE,
                                                         "Adjusted" = TRUE),
                                             selected = FALSE))
                         
                       ),
                       strong("Effect size:"),
                       fluidRow(
                         column(5,
                                textInput("var_eff_min", label = "min")
                         ),
                         column(5,
                                textInput("var_eff_max", label = "max"))
                       )
      ),
      actionButton("filter",
                   label = "Filter"),
      
      br(),
      br(),
      # Basic information of the dataset
      htmlOutput("ds_info")
    ),
    mainPanel(
      tabsetPanel(
        # Welcome text and a list of the datasets in database
        tabPanel("Main",
                 h3("Welcome to use Ninni the visualization app!"),
                 p("You can browse Ninni's database using the search fields on the left."),
                 p("You can view the associations of the chosen dataset is the Data Table tab,
                   and visualize the data with the tools provided in other tabs."),
                 p("Ninni will try to provide you with interactive visualization. Unfortunately,
                   for very large datasets this is not possible. If the chosen dataset seems too large,
                   you must either settle for a static figure or filter the dataset."),
                 br(),
                 h3("Datasets"),
                 DT::dataTableOutput("dstable")
        ),
        # All the chosen associations in table format
        tabPanel("Data Table",
                 h3("Associations"),
                 DT::dataTableOutput("tabular"),
                 br(),
                 uiOutput("download")
        ),

        tabPanel("Heat Map",
                 # Toggle hierarchical clustering for heat map
                 radioButtons("clustering",
                              label = "Order",
                              choices = c("Alphabetical" = FALSE,"Clustering" = TRUE),
                              selected = FALSE,
                              inline = TRUE),
                 checkboxInput("lower_tri", "Lower-triangular"),
                 checkboxInput("symmetrical", "Fill to symmetrical"),
                 br(),
                 strong("Effect"),
                 checkboxInput("heatmap_log2",
                               label = "Log2 transform"),
                 checkboxInput("heatmap_discrete",
                               label = "Discretize"),
                 sliderInput("heatmap_breaks",
                              label = "Number of levels on discrete scale",
                              value = 5, min = 3, max = 11),
                 radioButtons("heatmap_color_scale",
                              label = "Type of color scale",
                              choices = c("Sequential", "Diverging")),
                 numericInput("heatmap_midpoint",
                              label = "Midpoint of diverging scale", value = 0),
                 radioButtons("heatmap_p",
                              "Include p-values?",
                              choices = c("No" = "", "raw p-values" = "P", "Adjusted p-values" = "P_adj")),
                 conditionalPanel("input.heatmap_p !== ''",
                                  numericInput("heatmap_p_limit",
                                               "Ignore p-values above",
                                               value = 0.1, min = 0, max = 1, step = 0.05),
                                  sliderInput("heatmap_point_range",
                                              "Size range of the p-value points",
                                              value = c(1,4), min = 1, max = 10)),
                 uiOutput("heatmap_n_plotted"),
                 plotUI("heatmap")
        ),
        
        tabPanel("Volcano plot",
                 # Log2 scale effect?
                 checkboxInput("volcano_log2",
                               label = "log2 transform"),
                 # Choices for double filtering the volcano plot i.e. filtering by p-value and/or effect size
                 # Toggle double filtering
                 checkboxInput("double_filter",
                              label = "Apply visual filters"),
                 conditionalPanel("input.double_filter == true",
                                  fluidRow(
                                    column(4, # Set p-value limit
                                           textInput("df_p_limit",
                                                     label = "P-value <",
                                                     value = "0.05")),
                                    column(5, # Filter by unadjusted or adjusted p-value
                                           radioButtons("df_p_limit_adj",label = NULL,
                                                        choices = c("Unadjusted" = FALSE,
                                                                    "Adjusted" = TRUE),
                                                        selected = FALSE,
                                                        inline = TRUE))
                                    
                                  ),
                                  fluidRow(
                                    column(4, # Set limit for the effect
                                           textInput("df_effect_limit",
                                                     label = "Absolute effect >",
                                                     value = 3)),
                                    conditionalPanel("input.volcano_log2 == true",
                                                     column(5, # Filter by raw or log2 effect
                                                            radioButtons("df_eff_limit_log2",label = NULL,
                                                                         choices = c("Original" = FALSE,
                                                                                     "log2" = TRUE),
                                                                         selected = FALSE,
                                                                         inline = TRUE)))
                                    
                                  )
                 ),
                 checkboxInput("volcano_shape","Shape by dataset"),
                 
                 
                 # The actual volcano plot
                 plotUI("volcano_plot")
        ),
        
        tabPanel("Q-Q plot",
                 radioButtons("qq_choice",
                              label = "Choose the type of Q-Q plot",
                              choices = c("P-values" = "P",
                                          "Effect" = "Effect"),
                              inline = TRUE),
                 conditionalPanel("input.qq_choice == 'Effect'",
                                   checkboxInput("qq_log2",
                                                 label = "log2 transform")),
                 # Toggle coloring by column
                 # Choose discrete or continuous color scale (only relevant for numeric values)
                 checkboxInput("qq_coloring", "Coloring according to column"),
                 conditionalPanel("input.qq_coloring == true",
                                  radioButtons("qq_coloring_type",NULL,
                                               choices = c("Continuous", "Discrete"), inline = TRUE),
                                  selectizeInput("qq_coloring_column","Column",
                                                 choices = NULL,
                                                 options = list(maxItems = 1,
                                                                placeholder = 'Choose a column',
                                                                onInitialize = I('function() { this.setValue(""); }')))),
                 plotUI("qq_plot")
        ),
        
        tabPanel("Signed Manhattan plot",
                 checkboxInput("lady_log2", label = "use sign of log2-transformed effect"),
                 selectizeInput("lady_x_column", "Column for x-axis",
                                choices = NULL,
                                options = list(maxItems = 1,
                                               placeholder = 'Choose a column',
                                               onInitialize = I('function() { this.setValue(""); }'))),
                 checkboxInput("lady_coloring", "Coloring according to column"),
                 conditionalPanel("input.lady_coloring == true",
                                  radioButtons("lady_coloring_type",NULL,
                                               choices = c("Continuous", "Discrete")),
                                  selectizeInput("lady_coloring_column", "Column",
                                                 choices = NULL,
                                                 options = list(maxItems = 1,
                                                                placeholder = 'Choose a column',
                                                                onInitialize = I('function() { this.setValue(""); }')))),
                 plotUI("manhattan")),
        
        tabPanel("Lollipop plot",
                 selectizeInput("lollipop_column", "Column for counting observations",
                                choices = NULL),
                 numericInput("lollipop_n", "Number of top values to show", value = 10, min = 1),
                 plotUI("lollipop")),
        
        tabPanel("UpSet plot",
                 selectizeInput("upset_group", "Group by",
                                choices = NULL,
                                selected = "Dataset"),
                 selectizeInput("upset_column", "Column for elements",
                                choices = NULL,
                                options = list(maxItems = 1,
                                               placeholder = 'Choose a column',
                                               onInitialize = I('function() { this.setValue(""); }'))),
                 numericInput("upset_n", "Number of top intersections to show", value = 10, min = 1),
                 selectizeInput("upset_order", "Order by",
                                choices = c("Degree & Frequency", "Frequency")),
                 sliderInput("upset_text_scale", "Text size",
                             min = 0.5, max = 4, value = 1, step = 0.1),
                 checkboxInput("upset_empty", "Show empty intersections"),
                 plotUI("upset_plot")),
        
        tabPanel("P-value histogram",
                 selectizeInput("phist_facet", "Facet by",
                                choices = NULL,
                                options = list(maxItems = 1,
                                               placeholder = 'Choose a column',
                                               onInitialize = I('function() { this.setValue(""); }'))),
                 plotUI("p_histogram")),
        
        tabPanel("Ridgeplot",
                 selectizeInput("ridge_x", "x-axis",
                                choices = NULL,
                                options = list(maxItems = 1,
                                               placeholder = 'Choose a column',
                                               onInitialize = I('function() { this.setValue(""); }'))),
                 checkboxInput("ridge_log2", "log2-transform x-axis"),
                 selectizeInput("ridge_y", "y-axis (split by)",
                                choices = NULL,
                                options = list(maxItems = 1,
                                               placeholder = 'Choose a column',
                                               onInitialize = I('function() { this.setValue(""); }'))),
                 sliderInput("ridge_scale", "Curve heigth", min = 0.5, max = 5, value = 1.5, step = 0.05),
                 selectizeInput("ridge_style", "Style",
                                choices = c("Black & White" = "bw", "Grey" = "grey", "Colourful" = "colours")),
                 plotUI("ridge")),
        
        tabPanel("Network plot",
                 selectInput("network_type", "Type",
                             choices = c("Variables to variable" = "var_to_var",
                                         "Variables to outcome" = "var_to_outcome")),
                 checkboxInput("network_interactive", "Draw an interactive network"),
                 conditionalPanel("input.network_interactive === false",
                                  selectInput("network_layout", "Network layout method",
                                              choices = c("Nicely" = "nicely",
                                                          "Force-directed by Fruchterman and Reingold"  = "fr",
                                                          "spring-based by Kamada and Kawai" = "kk",
                                                          "Tree" = "tree")),
                                  checkboxInput("node_names", "Include node names"),
                                  checkboxInput("node_repel", "Repel node names from node points"),
                                  selectizeInput("edge_color", "Edge color",
                                                 choices = NULL,
                                                 options = list(maxItems = 1,
                                                                placeholder = 'Choose a column',
                                                                onInitialize = I('function() { this.setValue(""); }'))),
                                  conditionalPanel("input.edge_color !== ''",
                                                   checkboxInput("edge_color_log2", "log2-transform color"),
                                                   radioButtons("edge_color_scale",
                                                                label = "Type of color scale",
                                                                choices = c("Continuous", "Diverging", "Discrete")),
                                                   numericInput("edge_midpoint",
                                                                label = "Midpoint of diverging scale", value = 0)),
                                  selectizeInput("edge_width", "Edge width",
                                                 choices = NULL,
                                                 options = list(maxItems = 1,
                                                                placeholder = 'Choose a column',
                                                                onInitialize = I('function() { this.setValue(""); }'))),
                                  conditionalPanel("input.edge_width !== ''",
                                                   checkboxInput("edge_width_log2", "log2-transform width"),
                                                   sliderInput("edge_width_range", "Edge width range",
                                                               min = 0.2, max = 8, value = c(1, 4), step = 0.05)),
                                  selectizeInput("edge_weight", "Edge weight (used in some layout methods)",
                                                 choices = NULL,
                                                 options = list(maxItems = 1,
                                                                placeholder = 'Choose a column',
                                                                onInitialize = I('function() { this.setValue(""); }'))),
                                  conditionalPanel("input.edge_weight !== ''",
                                                   checkboxInput("edge_weight_log2", "log2-transform weight")),
                                  plotUI("network")),
                 conditionalPanel("input.network_interactive === true",
                                  sliderInput("node_size", "Node size",
                                              min = 10, max = 200, value = 30),
                                  sliderInput("link_distance", "Edge distance",
                                              min = 10, max = 600, value = 50),
                                  sliderInput("font_size", "Font size",
                                              min = 5, max = 50, value = 20),
                                  sliderInput("network_width", "Plot width",
                                              min = 200, max = 3000,
                                              value = 800),
                                  sliderInput("network_height", "Plot height",
                                              min = 200, max = 3000,
                                              value = 600),
                                  uiOutput("d3network"))
                 )
    )
    
  )
)))