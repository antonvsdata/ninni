shinyUI( fluidPage(
  
  includeCSS("styles.css"),
  
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
      h4("Dataset"),
      uiOutput("ds_choice"),
      uiOutput("metadata_tags_ui"),
      textInput("var_keywords","Variable keywords"),
      actionButton("query","Query"),
      
      uiOutput("filters"),
      
      br(),
      br(),
      
      htmlOutput("ds_info")
    ),
    mainPanel(
      tabsetPanel(
        
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
                              selected = FALSE),
                 uiOutput("heatmap")
        ),
        
        tabPanel("Volcano plot",
                 # Choices for double filtering the volcano plot i.e. filtering by p-value and/or effect size
                 
                 # Toggle double filtering
                 radioButtons("double_filter",
                              label = "Visual filters",
                              choices = c("Yes" = TRUE, "No" = FALSE),
                              selected = FALSE,
                              inline = TRUE),
                 
                 fluidRow(
                   column(4, # Set p-value limit
                          textInput("df_p_limit",
                                    label = "P-value <",
                                    value = "0.05")),
                   column(5, # Filter by unadjusted or FDR adjusted p-value
                          radioButtons("df_p_limit_fdr",label = NULL,
                                       choices = c("Unadjusted" = FALSE,
                                                   "FDR" = TRUE),
                                       selected = FALSE,
                                       inline = TRUE))
                   
                 ),
                 fluidRow(
                   column(4, # Set limit for the effect
                          textInput("df_effect_limit",
                                    label = "Absolute effect >",
                                    value = 3)),
                   column(5, # Filter by raw or log2 effect
                          radioButtons("df_eff_limit_log2",label = NULL,
                                       choices = c("Original" = FALSE,
                                                   "log2" = TRUE),
                                       selected = FALSE,
                                       inline = TRUE))
                 ),
                 # The actual volcano plot
                 uiOutput("volcano")
        ),
        
        tabPanel("Q-Q plot",
                 radioButtons("qq_choice",
                                         label = "Choose the type of Q-Q plot",
                                         choices = c("p-values",
                                                     "norm"),
                                         inline = TRUE),
                 uiOutput("qq_plot")
        ),
        
        tabPanel("Lady Manhattan plot",
                 uiOutput("lady_manhattan_plot_choices"),
                 uiOutput("lady_manhattan_plot"))
      )
    )
    
  )
))