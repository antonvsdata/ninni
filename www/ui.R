shinyUI( fluidPage(
  titlePanel("Ninni"),
  
  
  sidebarLayout(
    sidebarPanel(
#      h4("Search for datasets, by dataset label or by keywords. All separated by comma (required)"),
#       textInput("ds_labels",
#                 label = "Labels"),
#       textInput("ds_tags", #value = "BIVARIATE",
#                 label = "Keyword, eg. \"T2D\" or \"type 2 diabetes\"",
#                 value = ""),
      
      uiOutput("ds_choice"),
      
      h4("Filter the search results by variable (separated by comma) or p-value"),
      textInput("var_labels",
                label = "Enter the variable you're interested in"),
      
      textInput("p_limit",
                   label = "Show only p-values below"),
                   #value = 1,
                   #min = 0,
                   #max = 1,
                   #step = 0.05),
      
      textInput("p_fdr_limit",
                   label = "Show only p_fdr below"),
                   #value = 1,
                   #min = 0,
                   #max = 1,
                   #step = 0.05),
      
      textInput("n_limit",
                label = "Show only associations with n greater than"),
      
      
      actionButton("submit_main",
                   label = "Submit")
    ),
    mainPanel(
      tabsetPanel(
        
        tabPanel("Main",
                 p("Welcome to use Ninni, the web-based visualization app"),
                 p("You can browse Ninni's database using the search fields on the left"),
                 p("The tabular representation tab shows the datasets found by your search as well
                   as the associations in the datasets"),
                 p("If the datasets have differing amounts of variables or different effect types,
                   only the dataset information will be shown"),
                 p("The other tabs include the visualization tools provided by Ninni")
        ),
        
        tabPanel("Datasets",
                 DT::dataTableOutput("dstable")),
        
        tabPanel("Tabular representation",
                 h3("Associations"),
                 DT::dataTableOutput("tabular")
        ),
        
        tabPanel("Heat Map",
                 plotlyOutput("heatmap",height = "1000")
        ),
        
        tabPanel("Volcano plot",
                 plotlyOutput("volcano",height = "700"),
                 
                 checkboxInput("double_filter",
                               label = "Enable double filtering",
                               value = FALSE),
                 
                 conditionalPanel(condition = ("input.double_filter == true"),
                                  textInput("df_p_lim",
                                            label = "Limit of p-value (FDR)",
                                            value = 0.01),
                                  textInput("df_effect_lim",
                                            label = "Limit of effect (absolute value, use log2-value for OR or FC)",
                                            value = 3))
        ),
        
        tabPanel("Q-Q plot",
                 plotlyOutput("qqplot", height = "700")
        )
                 
        
        
        
      )
    )
    
  )
))