library(shiny)
library(DBI)
source("db_functions.R")
db_info <- read_db_info("database_import.config")

ui <- fluidPage(
  
  titlePanel("Import data to Ninni"),
  br(),
  br(),
  
  fileInput("dataset_file", "Upload a .csv file listing the datasets", accept = ".csv"),
  
  fileInput("metadata_file", "Upload a .csv file containing the dataset metadata", accept = ".csv"),
  
  # Upload zip files
  fileInput("zipfile", "Upload .zip file containing the actual results and variable descriptions as .csv files", accept = ".zip"),
  # action button to unzip the file
  actionButton("unzip", "Unzip Files"),
  
  # to display the list of unzipped files
  tableOutput("unzipped_table"),
  
  tableOutput("data_files"),
  
  fluidRow(
    column(1,
           actionButton("import", "Import data")),
    
    column(2,
           checkboxInput("append", "Append to database",
                         value = TRUE))
  ),
  
  htmlOutput("import_errors")
  
)

#### server code starts here
server <- function(input, output, session) {
  
  unzipped <- reactive({
    print("event reacted")
    if (is.null(input$zipfile)) {
      return(NULL)
    }
    unzip(input$zipfile$datapath, exdir = "data")
  })
  
  read <- reactive({
    dfs <- lapply(unzipped(), read.csv, stringsAsFactors = FALSE)
    names(dfs) <- gsub("data/", "", unzipped())
    dfs
  })
  
  output$unzipped_table <- renderTable({
    if (is.null(input$zipfile)) {
      return(NULL)
    }
    data.frame("New files" = gsub("data/", "", unzipped()),
               "Number of rows" = sapply(read(), nrow),
               "Number of columns" = sapply(read(), ncol),
               check.names = FALSE)
    
  })
  
  output$data_files <- renderTable({
    files <- list.files("data/")
    sizes <- sapply(files, function(f) {
      round(file.info(paste0("data/", f))$size/1000)
    })
    files_df <- data.frame("Existing files" = files,
                           "Size (kB)" = as.integer(sizes),
                           check.names = FALSE)
  })
  
  datasets <- reactive({
    read.csv(input$dataset_file$datapath, stringsAsFactors = FALSE)
  })
  
  metadata <- reactive({
    read.csv(input$metadata_file$datapath, stringsAsFactors = FALSE)
  })
  
  file_errors <- eventReactive(input$import, {
    if (is.null(input$dataset_file)) {
      return(error_html("Datasets file not found"))
    }
    if (is.null(input$metadata_file)) {
      return(error_html("Metadata file not found"))
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
    
    con <- dbConnect(
      drv = RPostgres::Postgres(),
      dbname = db_info$db_name,
      host = db_info$db_host,
      port = db_info$db_port,
      user = db_info$db_user,
      password = db_info$db_password)
    
    assocs <- read()[datasets()$DATASET_FILENAME]
    
    tryCatch({
      import_data(con, datasets = datasets(), metadata = metadata(),
                  assocs = read()[datasets()$DATASET_FILENAME], append = input$append,
                  progress = progress)
    }, error = function(e) {
      print(e$message)
    })
    
    progress$set(message = "Closing database connection", value = 0.99)
    
    dbDisconnect(con)
    
    progress$set(message = "Data imported", value = 1)
    Sys.sleep(4)
    progress$close()
  })
  
}

shinyApp(ui = ui, server = server)