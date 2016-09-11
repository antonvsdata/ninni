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
      
      h4("Filter the search results"),
      textInput("var_labels",
                label = "Keywords, comma separated"),
      
      textInput("p_limit",label = "P-value <"),
      
      textInput("p_fdr_limit",
                   label = "P-value (FDR) <"),
      
      
      textInput("n_limit",
                label = "Minimum n"),
      
      
      actionButton("submit_main",
                   label = "Submit")
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
        
        tabPanel("Tabular representation",
                 h3("Associations"),
                 DT::dataTableOutput("tabular")
        ),
        
        tabPanel("Heat Map",
                 uiOutput("heatmap")
        ),
        
        tabPanel("Volcano plot",
                 uiOutput("volcano"),
                 radioButtons("double_filter",
                               label = "Visual filters",
                              choices = c("Yes" = TRUE, "No" = FALSE),
                              selected = FALSE,
                              inline = TRUE),
                 
                 conditionalPanel(condition = ("input.double_filter == TRUE"),
                                  textInput("df_p_lim",
                                            label = "Limit of p-value (FDR)",
                                            value = 0.01),
                                  textInput("df_effect_lim",
                                            label = "Limit of effect (absolute value, use log2-value for OR or FC)",
                                            value = 3))
        ),
        
        tabPanel("Q-Q plot",
                 uiOutput("qq")
                 
#                  radioButtons("qq_choice",
#                              label = "Choose the type of Q-Q plot",
#                              choices = c("p-values",
#                                          "norm"),
#                              inline = TRUE),
#                  uiOutput("qq_plot"),
#                  textOutput("woop")
        )
                 
        
        
        
      )
    )
    
  )
))