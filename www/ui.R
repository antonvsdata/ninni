library(shiny)

shinyUI( fluidPage(
  titlePanel("Da Appp"),
  
  
  sidebarLayout(
    sidebarPanel(
      
      textInput("ds_label",
                label = "Search for datasets, separated by comma"),
      textInput("ds_tag",
                label = "Search by dataset keyword, eg. \"T2D\" or \"type 2 diabetes\""),
      
      textInput("var_label",
                label = "Enter the variable you're interested in"),
      
      numericInput("p_limit",
                   label = "Show only p-values below",
                   value = 1,
                   min = 0,
                   max = 1,
                   step = 0.05),
      
      
      actionButton("submit_main",
                   label = "Submit")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Tabular representation",
                 tableOutput("tabular")
        ),
        
        tabPanel("Heat Map",
                 plotOutput("heatmap")
        ),
        
        tabPanel("Volcano plot",
                 plotOutput("volcano")
        ),
        
        tabPanel("Q-Q plot",
                 plotOutput("qqplot"),
        )
                 
        
        
        
      )
    )
    
  )
))