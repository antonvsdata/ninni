shinyUI( fluidPage(
  titlePanel("Ninni"),
  
  
  sidebarLayout(
    sidebarPanel(
      h4("Search for datasets, by dataset label or by keywords. All separated by comma (required)"),
      textInput("ds_labels",
                label = "Labels"),
      textInput("ds_tags", #value = "BIVARIATE",
                label = "Keyword, eg. \"T2D\" or \"type 2 diabetes\""),
      
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
        tabPanel("Tabular representation",
                 selectInput("table_choice",
                                    label = "Display",
                                    choices = c("Top 10 rows" = "top",
                                                "Bottom 10 rows" = "bot",
                                                "Random 10 rows" = "rnd")),
                 tableOutput("tabular")
        ),
        
        tabPanel("Heat Map",
                 plotOutput("heatmap")
        ),
        
        tabPanel("Volcano plot",
                 plotOutput("volcano")
        ),
        
        tabPanel("Q-Q plot",
                 plotOutput("qqplot")
        )
                 
        
        
        
      )
    )
    
  )
))