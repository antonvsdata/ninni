shinyUI( fluidPage(
  
  includeCSS("styles.css"),
  
  titlePanel("Ninni"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Dataset"),
      uiOutput("ds_choice"),
      
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
                 radioButtons("clustering",
                              label = "Order",
                              choices = c("Alphabetical" = FALSE,"Clustering" = TRUE),
                              selected = FALSE),
                 uiOutput("heatmap")
        ),
        
        tabPanel("Volcano plot",
                 radioButtons("double_filter",
                              label = "Visual filters",
                              choices = c("Yes" = TRUE, "No" = FALSE),
                              selected = FALSE,
                              inline = TRUE),
                 
                 fluidRow(
                   column(4,
                          textInput("df_p_limit",
                                    label = "P-value <",
                                    value = "0.05")),
                   column(5,
                          radioButtons("df_p_limit_fdr",label = NULL,
                                       choices = c("Unadjusted" = FALSE,
                                                   "FDR" = TRUE),
                                       selected = FALSE,
                                       inline = TRUE))
                   
                 ),
                 fluidRow(
                   column(4,
                          textInput("df_effect_limit",
                                    label = "Effect >",
                                    value = 3)),
                   column(5,
                          radioButtons("df_eff_limit_log2",label = NULL,
                                       choices = c("Original" = FALSE,
                                                   "log2" = TRUE),
                                       selected = FALSE,
                                       inline = TRUE))
                 ),
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