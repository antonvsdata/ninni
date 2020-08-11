library(shiny)
library(DBI)
source("db_functions.R")
db_info <- read_db_info("database_import.config")
options(shiny.maxRequestSize = 300*1024^2)

ui <- fluidPage(
  
  titlePanel("Import data to Ninni"),
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
  

  
  htmlOutput("import_errors")
  
)

file_sizes <- function(files, title) {
  
 colnames(size_df)[1] <- title
 size_df
}

#### server code starts here
server <- function(input, output, session) {
  
  unzipped <- observe({
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
    
    t <- system.time({
      con <- dbConnect(
        drv = RPostgres::Postgres(),
        dbname = db_info$db_name,
        host = db_info$db_host,
        port = db_info$db_port,
        user = db_info$db_user,
        password = db_info$db_password)
      
      tryCatch({
        imported <- import_data(con, datasets = datasets(), metadata = metadata(), clear = input$clear,
                    progress = progress)
      }, error = function(e) {
        print(e$message)
      })
      
      progress$set(message = "Closing database connection", value = 0.99)
      dbDisconnect(con)
      progress$close()
    })
    
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
    
  })
  
}

shinyApp(ui = ui, server = server)