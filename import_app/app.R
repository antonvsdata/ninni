library(shiny)
library(DBI)
library(DT)
library(dplyr)
source("db_functions.R")
db_info <- read_db_info("database_import.config")
options(shiny.maxRequestSize = 300*1024^2)

ui <- fluidPage(
  tabsetPanel(
    
    tabPanel("Import data to Ninni",
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
             
             
             
             htmlOutput("import_errors")),
    
    tabPanel("Delete data from database",
             
             br(),
             br(),
             
             selectizeInput("dataset_label", "Dataset label to delete",
                            choices = NULL,
                            options = list(maxItems = 1,
                                           placeholder = 'Choose a dataset',
                                           onInitialize = I('function() { this.setValue(""); }'))),
             
             actionButton("delete", "Delete"),
             
             br(),
             br(),
             
             DT::dataTableOutput("ds_table")
             )
    
  )
  
  
  
)

file_sizes <- function(files, title) {
  
 colnames(size_df)[1] <- title
 size_df
}

#### server code starts here
server <- function(input, output, session) {
  
  con <- con <- dbConnect(
    drv = RPostgres::Postgres(),
    dbname = db_info$db_name,
    host = db_info$db_host,
    port = db_info$db_port,
    user = db_info$db_user,
    password = db_info$db_password)
  
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
    imported <- NA
    error_msg <- ""
    t <- system.time({
      
      
      tryCatch({
        imported <- import_data(con, datasets = datasets(), metadata = metadata(), clear = input$clear,
                    progress = progress)
      }, error = function(e) {
        error_msg <- e$message
        print(e$message)
      })
      
      progress$close()
    })
    if (is.na(imported)) {
      showModal(modalDialog(
        title = "Data not imported",
        paste("Data could not be imported:", error_msg),
        easyClose = TRUE
      ))
    } else {
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
    }
    
  })
  
  modal_confirm <- reactive({modalDialog(
    paste("Are you sure you want to delete", input$dataset_label),
    title = "Deleting dataset",
    footer = tagList(
      actionButton("cancel", "Cancel"),
      actionButton("ok", "Delete", class = "btn btn-danger")
    )
  )
  })
  
  observeEvent(input$delete, {
    if (input$dataset_label != "") {
      showModal(modal_confirm())
    }
    
  })
  
  observeEvent(input$cancel, 
               removeModal()
  )
  
  observeEvent(input$ok, {
    
    progress <- Progress$new(session, min=0, max=1)
    
    progress$set(message = "Connecting to database")
    success <- delete_dataset(con, input$dataset_label, progress)
    progress$close()
    
    if (success) {
      updateSelectizeInput(session, "dataset_label",
                           choices = ds_dframe()$Label)
      showModal(modalDialog(
        title = "Dataset deleted",
        paste("Dataset", input$dataset_label, "deleted"),
        easyClose = TRUE
      ))
    } else {
      showModal(modalDialog(
        title = "Dataset not deleted",
        paste("Dataset could not be deleted"),
        easyClose = TRUE
      ))
    }
    
    
  })
  
  ds_dframe <- reactive({
    input$ok
    get_datasets_db(con)
  })
  
  observeEvent(ds_dframe, {
      updateSelectizeInput(session, "dataset_label",
                           choices = ds_dframe()$Label)
    
  })
  
  onStop(function() {
    dbDisconnect(con)
  })
  
  output$ds_table <- DT::renderDataTable({
    datatable(ds_dframe(), selection = "none")
  })
  
}

shinyApp(ui = ui, server = server)